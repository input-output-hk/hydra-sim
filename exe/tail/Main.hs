module Main where

import Prelude

import Hydra.Tail.Simulation (
  SimulationSummary (..),
  analyzeSimulation,
  durationOf,
  mkAnalyze,
  prepareSimulation,
  readEventsThrow,
  runSimulation,
  summarizeEvents,
  writeEvents,
  writeTransactions,
 )
import Hydra.Tail.Simulation.Options (
  Command (..),
  RunOptions,
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
      txs <- withProgressReport (lastSlot summary) options $ \reportProgress ->
        analyzeSimulation reportProgress options summary events trace
      -- REVIEW(SN): in tail we did optionally "discardEdges"; here we do
      -- simulate longer than the last event (see 'durationOf')
      let duration = durationOf options events
      pPrint $ mkAnalyze duration summary txs

      let res = resultName options summary
      putStrLn $ "Writing confirmation times into: " <> res
      writeTransactions res txs

resultName :: RunOptions -> SimulationSummary -> String
resultName options summary =
  "txs"
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
