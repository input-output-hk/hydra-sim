module Main where

import Prelude

import Hydra.Tail.Simulation (
  SimulationSummary (..),
  analyzeSimulation,
  mkAnalyze,
  prepareSimulation,
  readEventsThrow,
  readRetriesThrow,
  readTransactionsThrow,
  runSimulation,
  summarizeEvents,
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
      (txs, retries) <- withProgressReport (lastSlot summary) options $ \reportProgress ->
        analyzeSimulation reportProgress trace
      pPrint $ mkAnalyze defaultAnalyzeOptions txs retries

      let resTxs = resultName options "txs" summary
      putStrLn $ "Writing confirmation times into: " <> resTxs
      writeTransactions resTxs txs

      let resRet = resultName options "retries" summary
      putStrLn $ "Writing retries counts into: " <> resRet
      writeRetries resRet retries
    Analyze options filepath -> do
      txs <- readTransactionsThrow filepath
      retries <- readRetriesThrow filepath
      pPrint $ mkAnalyze options txs retries

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
