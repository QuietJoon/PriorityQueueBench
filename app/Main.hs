module Main where


import           Control.DeepSeq

import qualified Data.IntMap                   as IM
import           Data.List

import           Util.Adaptor.Random.SplitMix

import           Data
import           Type
import           IntMapMMPQ                    as MMPQ
import           IntMapIntPSQ                  as IPSQ
import           IntMapPQueue                  as IPQu


randomInts = unfoldr (Just . bmwrInt 65536)

main :: IO ()
main = do
  putStrLn "Initialize"
  let i0Gen               = mkGenFromInt 0
  let (makingGen, simGen) = splitGen i0Gen
  let makingList          = randomInts makingGen
  let simInputList        = take 10000000 $ randomInts simGen
  let instanceList        = take 100 $ makeInstances makingList
  let iMMPQBigQueue       = MMPQ.makeSimulation instanceList
  let iIPSQBigQueue       = IPSQ.makeSimulation instanceList
  let iIPQuBigQueue       = IPQu.makeSimulation instanceList

  simInputList `deepseq` putStrLn "Evaluated"

  print iMMPQBigQueue
  print iIPSQBigQueue
  print iIPQuBigQueue

  let theIntMapMMPQResult   = MMPQ.runSimulation iMMPQBigQueue simInputList
  let theIntMapIntPSQResult = IPSQ.runSimulation iIPSQBigQueue simInputList
  let theIntMapPQueueResult = IPQu.runSimulation iIPQuBigQueue simInputList

  mapM_ print theIntMapMMPQResult
  putStrLn "========="
  mapM_ print theIntMapIntPSQResult
  putStrLn "========="
  mapM_ print theIntMapPQueueResult
  print (theIntMapMMPQResult == theIntMapIntPSQResult)
