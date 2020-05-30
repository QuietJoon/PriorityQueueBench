{-# LANGUAGE RecordWildCards #-}
module IntPSQMMPQ where


import           Control.DeepSeq

import qualified Data.IntPSQ                   as BQ
import           Data.List
import           Data.Maybe

import qualified Data.IntMinMaxQueue           as SQ


import           Data
import           Type
import           Instance

--import           Debug.Trace


type BigQueue = BQ.IntPSQ Time SmallQueue
type SmallQueue = SQ.IntMinMaxQueue Instance
type AccList = [Instance]


emptyBigQueue = BQ.empty
emptySmallQueue = SQ.empty

makeSimulation :: [Instance] -> BigQueue
makeSimulation iList = makeSimulationSub iList emptyBigQueue

makeSimulationSub :: [Instance] -> BigQueue -> BigQueue
makeSimulationSub is bQ = foldl (flip setInstance) bQ is

setInstance :: Instance -> BigQueue -> BigQueue
setInstance i@I {..} =
  snd
    . BQ.alter
        (maybe ((), Just (timeSlot, SQ.singleton getInternalTime i))
               (\(_, v) -> ((), Just (timeSlot, SQ.insert getInternalTime i v)))
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
  mTheResult           = SQ.pollMin sQ
  (theInstance, theSQ) = fromJust mTheResult
  currentTime          = timeSlot theInstance
  eNewInstance         = runInstance theInstance r
  newSQ                = either
    (const theSQ)
    (\i -> if timeSlot i == currentTime
      then SQ.insert getInternalTime i theSQ
      else theSQ
    )
    eNewInstance
  newBQ = either
    (const bQ)
    (\i -> if timeSlot i == currentTime then bQ else setInstance i bQ)
    eNewInstance
  newInstance = either id id eNewInstance

getBigQueueSize = BQ.fold' (\_ _ v b -> SQ.size v + b) 0
