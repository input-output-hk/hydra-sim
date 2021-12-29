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
  Command (..),
  RunOptions,
  defaultAnalyzeOptions,
  parseCommand,
  proactiveSnapshot,
  settlementDelay,
  withProgressReport,
 )
import Hydra.Tail.Simulation.SlotNo (unSlotNo)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  parseCommand >>= \case
    Prepare options filepath -> do
      pPrint options
      events <- prepareSimulation options
      writeEvents filepath events
    Run options filepath -> do
      pPrint options
      events <- readEventsThrow filepath

      let summary = summarizeEvents options events
      pPrint summary

      let trace = runSimulation options events
      (txs, retries, snapshots, submittedTxs) <- withProgressReport (lastSlot summary) options $ \reportProgress ->
        analyzeSimulation defaultAnalyzeOptions reportProgress trace
      pPrint $ mkAnalyze defaultAnalyzeOptions txs retries snapshots submittedTxs

      let resTxs = resultName options "txs" summary
      putStrLn $ "Writing confirmation times into: " <> resTxs
      writeTransactions resTxs txs

      let resRet = resultName options "retries" summary
      putStrLn $ "Writing retries counts into: " <> resRet
      writeRetries resRet retries
    Analyze options filepath -> do
      txs <- readTransactionsThrow filepath
      retries <- readRetriesThrow filepath
      let snapshots = 0 -- FIXME: Get from options or read from file.
      let submittedTxs = 0 -- FIXME
      pPrint $ mkAnalyze options txs retries snapshots submittedTxs

resultName :: RunOptions -> String -> SimulationSummary -> String
resultName options prefix summary =
  prefix
    <> clients
    <> slots
    <> settlement
    <> proactive
    <> ".csv"
 where
  clients =
    "-" <> show (numberOfClients summary) <> "clients"

  slots =
    "-" <> show (unSlotNo $ lastSlot summary) <> "slots"

  settlement =
    "-" <> show (unSlotNo $ settlementDelay options) <> "s"

  proactive = case proactiveSnapshot options of
    Nothing -> ""
    Just frac -> "-" <> show frac <> "p"
