module Main where

import           Criterion.Main
import           Criterion.Types

import           Control.DeepSeq

import qualified Data.IntMap                   as IM
import           Data.List

import           Util.Adaptor.Random.SplitMix

import qualified Data                          as D
import           IntMapMMPQ                    as MMPQ
import           IntMapIntPSQ                  as IPSQ
import           IntMapPQueue                  as IPQu


myConfig60s =
  defaultConfig { timeLimit = 60.0, resamples = 10000, verbosity = Verbose }


randomInts = unfoldr (Just . bmwrInt 64)

main :: IO ()
main = do
  putStrLn "Initialize"
  let i0Gen               = mkGenFromInt 0
  let (makingGen, simGen) = splitGen i0Gen
  let idList              = [1 ..]
  let makingList          = randomInts makingGen
  let simInputList        = take 10000000 $ randomInts simGen
  let instanceList = take 10000 $ D.makeInstances makingList idList
  let iMMPQBigQueue       = MMPQ.makeSimulation instanceList
  let iIPSQBigQueue       = IPSQ.makeSimulation instanceList
  let iIPQuBigQueue       = IPQu.makeSimulation instanceList

  simInputList `deepseq` putStrLn "Evaluated"

  print iMMPQBigQueue
  print iIPSQBigQueue
  print iIPQuBigQueue

  let
    pqBench =
      bgroup "Simulation"
        $ [ bench "IntMapMMPQ"
            $ nf (MMPQ.runSimulation iMMPQBigQueue) simInputList
          , bench "IntMapIPSQ"
            $ nf (IPSQ.runSimulation iIPSQBigQueue) simInputList
          , bench "IntMapIPQu"
            $ nf (IPQu.runSimulation iIPQuBigQueue) simInputList
          ]
  putStrLn "Do bench"
  defaultMainWith myConfig60s [pqBench]
