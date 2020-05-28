{-# LANGUAGE RecordWildCards #-}
module IntMapPQueue where


import           Control.DeepSeq

import qualified Data.IntMap                   as BQ
import           Data.List
import           Data.Maybe

import qualified Data.PQueue.Min               as SQ


import           Data
import           Type
import           Instance

--import           Debug.Trace


type BigQueue = BQ.IntMap SmallQueue
type SmallQueue = SQ.MinQueue Instance
type AccList = [Instance]


emptyBigQueue = BQ.empty
emptySmallQueue = SQ.empty

makeSimulation :: [Instance] -> BigQueue
makeSimulation iList = makeSimulationSub iList emptyBigQueue

makeSimulationSub :: [Instance] -> BigQueue -> BigQueue
makeSimulationSub is bQ = foldl (flip setInstance) bQ is

setInstance :: Instance -> BigQueue -> BigQueue
setInstance i@I {..} =
  BQ.alter (Just . maybe (SQ.singleton i) (SQ.insert i)) timeSlot

runSimulation :: BigQueue -> [Int] -> AccList
runSimulation bQ rnList = theAccList
  where (theBQ, theAccList, _) = runTimeSlot bQ [] rnList

runTimeSlot :: BigQueue -> AccList -> [Int] -> (BigQueue, AccList, [Int])
runTimeSlot bQ accList rnList = if BQ.null bQ
  then (bQ, accList, rnList)
  else runTimeSlot nextBQ nextAccList nextRNList
 where
  (_, iSQ) = BQ.findMin bQ
  newBQ    = BQ.deleteMin bQ
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
  mTheResult           = SQ.minView sQ
  (theInstance, theSQ) = fromJust mTheResult
  currentTime          = timeSlot theInstance
  eNewInstance         = runInstance theInstance r
  newSQ                = either
    (const theSQ)
    (\i -> if timeSlot i == currentTime then SQ.insert i theSQ else theSQ)
    eNewInstance
  newBQ = either
    (const bQ)
    (\i -> if timeSlot i == currentTime then bQ else setInstance i bQ)
    eNewInstance
  newInstance = either id id eNewInstance

getBigQueueSize = BQ.foldr (\v b -> SQ.size v + b) 0

--printBigQueue bQ = BQ.mapM_ (\x -> do putStrLn ""; printSmallQueue x) bQ
--printSmallQueue sQ = SQ.mapM_ print sQ
