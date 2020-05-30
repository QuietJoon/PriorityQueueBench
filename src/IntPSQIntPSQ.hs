{-# LANGUAGE RecordWildCards #-}
module IntPSQIntPSQ where


import           Control.DeepSeq

import qualified Data.IntPSQ                   as BQ
import           Data.List
import           Data.Maybe

import qualified Data.IntPSQ                   as SQ


import           Data
import           Type
import           Instance


-- BigQueue: Int :: Time, p :: Time, v :: SmallQueue
type BigQueue = BQ.IntPSQ Time SmallQueue
-- SmallQueue: Int :: InstanceID, p :: InternalTime, v :: Instance
type SmallQueue = SQ.IntPSQ InternalTime Instance
type AccList = [Instance]


emptyBigQueue = BQ.empty
emptySmallQueue = SQ.empty

makeSimulation :: [Instance] -> BigQueue
makeSimulation iList = makeSimulationSub iList emptyBigQueue

makeSimulationSub :: [Instance] -> BigQueue -> BigQueue
makeSimulationSub is bQ = foldl (flip setInstance) bQ is

-- alter ::          (Maybe     a  ->     Maybe     a)   -> Key -> IntMap a   ->     IntMap a
-- alter :: Ord p => (Maybe (p, v) -> (b, Maybe (p, v))) -> Int -> IntPSQ p v -> (b, IntPSQ p v)
setInstance :: Instance -> BigQueue -> BigQueue
setInstance i@I {..} =
  snd
    . BQ.alter
        (maybe
          ((), Just (timeSlot, SQ.singleton instanceID (getInternalTime i) i))
          (\(_, v) ->
            ((), Just (timeSlot, SQ.insert instanceID (getInternalTime i) i v))
          )
        )
        timeSlot

runSimulation :: BigQueue -> [Int] -> AccList
runSimulation bQ rnList = theAccList
  where (theBQ, theAccList, _) = runTimeSlot bQ [] rnList

runTimeSlot :: BigQueue -> AccList -> [Int] -> (BigQueue, AccList, [Int])
runTimeSlot bQ accList rnList = if BQ.null bQ
  then (bQ, accList, rnList)
  else runTimeSlot nextBQ nextAccList nextRNList
 where
  Just (_, _, iSQ, newBQ) = BQ.minView bQ
  (nextBQ, _, nextAccList, nextRNList) =
    runTimeSlotSub newBQ iSQ accList rnList

runTimeSlotSub
  :: BigQueue
  -> SmallQueue
  -> AccList
  -> [Int]
  -> (BigQueue, SmallQueue, AccList, [Int])
runTimeSlotSub bQ sQ accList (r : rnList) = maybe
  (bQ, sQ, accList, rnList)
  (const (runTimeSlotSub newBQ newSQ (newInstance : accList) rnList))
  mTheResult
 where
  mTheResult                 = SQ.minView sQ
  (_, _, theInstance, theSQ) = fromJust mTheResult
  currentTime                = timeSlot theInstance
  eNewInstance               = runInstance theInstance r
  newSQ                      = either
    (const theSQ)
    (\i -> if timeSlot i == currentTime
      then SQ.insert (instanceID i) (getInternalTime i) i theSQ
      else theSQ
    )
    eNewInstance
  newBQ = either
    (const bQ)
    (\i -> if timeSlot i == currentTime then bQ else setInstance i bQ)
    eNewInstance
  newInstance = either id id eNewInstance

getBigQueueSize = BQ.fold' (\_ _ v b -> SQ.size v + b) 0
