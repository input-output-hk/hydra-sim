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
  parseCommand,
  settlementDelay,
  slotLength,
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

      let resultName =
            "txs-"
              <> show (numberOfClients summary)
              <> "clients-"
              <> show (unSlotNo $ lastSlot summary)
              <> "slots-"
              <> show (unSlotNo $ settlementDelay options)
              <> "s.csv"

      putStrLn $ "Writing confirmation times into: " <> resultName
      writeTransactions resultName txs
