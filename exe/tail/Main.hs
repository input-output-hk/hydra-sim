{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Prelude

import Hydra.Tail.Simulation (
  SimulationSummary (..),
  prepareSimulation,
  runSimulation,
  summarizeEvents,
 )
import Hydra.Tail.Simulation.Analyze (
  analyzeSimulation,
  mkAnalyze,
 )
import Hydra.Tail.Simulation.Csv (
  readEventsThrow,
  readRetriesThrow,
  readTransactionsThrow,
  writeEvents,
  writeRetries,
  writeTransactions,
 )
import Hydra.Tail.Simulation.Options (
  AnalyzeOptions (..),
  Command (..),
  RunOptions (..),
  defaultAnalyzeOptions,
  parseCommand,
  withProgressReport,
 )
import System.FilePath (
  replaceBaseName,
  takeBaseName,
 )
import Text.Pretty.Simple (
  pPrint,
 )

main :: IO ()
main = do
  parseCommand >>= \case
    Prepare options filepath -> do
      pPrint options
      events <- prepareSimulation options
      writeEvents filepath events
    Run options@RunOptions{paymentWindow} inputFilepath -> do
      pPrint options
      events <- readEventsThrow inputFilepath

      let summary = summarizeEvents options events
      pPrint summary

      let trace = runSimulation options events
      let analyzeOptions :: AnalyzeOptions
          analyzeOptions = defaultAnalyzeOptions{paymentWindow}
      (txs, retries, snapshots, submittedTxs) <- withProgressReport (lastSlot summary) options $ \reportProgress ->
        analyzeSimulation analyzeOptions reportProgress trace
      pPrint $ mkAnalyze analyzeOptions txs retries snapshots submittedTxs

      let txsFilepath = resultName inputFilepath "txs"
      putStrLn $ "Writing confirmation times into: " <> txsFilepath
      writeTransactions txsFilepath txs

      let retriesFilepath = resultName inputFilepath "retries"
      putStrLn $ "Writing retries counts into: " <> retriesFilepath
      writeRetries retriesFilepath retries
    Analyze options filepath -> do
      txs <- readTransactionsThrow (resultName filepath "txs")
      retries <- readRetriesThrow (resultName filepath "retries")
      let snapshots = mempty -- FIXME: Get from options or read from file.
      let submittedTxs = 0 -- FIXME
      pPrint $ mkAnalyze options txs retries snapshots submittedTxs

resultName :: FilePath -> String -> FilePath
resultName filepath kind =
  replaceBaseName filepath (takeBaseName filepath <> "[" <> kind <> "]")
