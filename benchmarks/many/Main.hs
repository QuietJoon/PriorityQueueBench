module Main where

import           Criterion.Main
import           Criterion.Types

import           Control.DeepSeq

import qualified Data.IntMap                   as IM
import           Data.List

import           Util.Adaptor.Random.SplitMix

import qualified Data                          as D
import           IntPSQMMPQ                    as PSMM
import           IntMapMMPQ                    as MMPQ
import           IntMapIntPSQ                  as IPSQ
import           IntPSQIntPSQ                  as PSPS
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
  let iPSMMBigQueue       = PSMM.makeSimulation instanceList
  let iMMPQBigQueue       = MMPQ.makeSimulation instanceList
  let iIPSQBigQueue       = IPSQ.makeSimulation instanceList
  let iPSPSBigQueue       = PSPS.makeSimulation instanceList
  let iIPQuBigQueue       = IPQu.makeSimulation instanceList

  simInputList `deepseq` putStrLn "Evaluated"

  print iPSMMBigQueue
  print iMMPQBigQueue
  print iIPSQBigQueue
  print iPSPSBigQueue
  print iIPQuBigQueue

  let
    pqBench =
      bgroup "Simulation"
        $ [ bench "IntPSQMMPQ"
            $ nf (PSMM.runSimulation iPSMMBigQueue) simInputList
          , bench "IntMapMMPQ"
            $ nf (MMPQ.runSimulation iMMPQBigQueue) simInputList
          , bench "IntMapIPQu"
            $ nf (IPQu.runSimulation iIPQuBigQueue) simInputList
          , bench "IntPSQIPSQ"
            $ nf (PSPS.runSimulation iPSPSBigQueue) simInputList
          , bench "IntMapIPSQ"
            $ nf (IPSQ.runSimulation iIPSQBigQueue) simInputList
          ]
  putStrLn "Do bench"
  defaultMainWith myConfig60s [pqBench]
