{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

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
import Hydra.Tail.Simulation.PaymentWindow (unLovelace)
import Hydra.Tail.Simulation.SlotNo (unSlotNo)
import System.FilePath (
  replaceBaseName,
  takeBaseName,
  (-<.>),
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
    Run options@RunOptions{paymentWindow, settlementDelay} inputFilepath -> do
      pPrint options
      events <- readEventsThrow inputFilepath

      let summary = summarizeEvents options events
      pPrint summary

      let trace = runSimulation options events
      let analyzeOptions :: AnalyzeOptions
          analyzeOptions = defaultAnalyzeOptions{paymentWindow, settlementDelay}
      (txs, retries, snapshots, submittedTxs) <- withProgressReport (lastSlot summary) options $ \reportProgress ->
        analyzeSimulation analyzeOptions reportProgress trace
      pPrint $ mkAnalyze analyzeOptions txs retries snapshots submittedTxs

      let txsFilepath = resultName inputFilepath options -<.> "txs"
      putStrLn $ "Writing confirmation times into: " <> txsFilepath
      writeTransactions txsFilepath txs

      let retriesFilepath = resultName inputFilepath options -<.> "retries"
      putStrLn $ "Writing retries counts into: " <> retriesFilepath
      writeRetries retriesFilepath retries
    Analyze options filepath -> do
      let txsFilePath = filepath -<.> "txs"
      putStrLn $ "Reading confirmation times from: " <> txsFilePath
      txs <- readTransactionsThrow txsFilePath

      let retriesFilePath = filepath -<.> "retries"
      putStrLn $ "Reading retries counts from: " <> retriesFilePath
      retries <- readRetriesThrow retriesFilePath

      let snapshots = mempty -- FIXME: Get from options or read from file.
      let submittedTxs = 0 -- FIXME
      pPrint $ mkAnalyze options txs retries snapshots submittedTxs

resultName :: FilePath -> RunOptions -> FilePath
resultName filepath RunOptions{paymentWindow, settlementDelay, proactiveSnapshot, enableBackupWallet, paymentCutOff} =
  replaceBaseName filepath $
    takeBaseName filepath
      <> (maybe "" (\w -> "-window:" <> show (unLovelace w)) $ paymentWindow)
      <> ("-delay:" <> show (unSlotNo settlementDelay))
      <> (maybe "" (\p -> "-proActive:" <> show p) $ proactiveSnapshot)
      <> (if paymentCutOff < 1.0 then "-cutOff:" <> show paymentCutOff else "")
      <> (if enableBackupWallet then "-withBackup" else "")
