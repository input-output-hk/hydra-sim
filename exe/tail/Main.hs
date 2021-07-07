module Main where

import Prelude

import Hydra.Tail.Simulation
    ( SimulationSummary (..)
    , analyzeSimulation
    , prepareSimulation
    , readEventsThrow
    , runSimulation
    , summarizeEvents
    , writeEvents, writeTransactions, mkAnalyze, durationOf
    )
import Hydra.Tail.Simulation.Options
    ( Command (..), parseCommand, withProgressReport, slotLength )
import Text.Pretty.Simple
    ( pPrint )

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
      -- REVIEW(SN): in tail we did optionally "discardEdges"
      let duration = durationOf events (slotLength options)
      pPrint $ mkAnalyze duration summary txs
      putStrLn "Calculating confirmation times..."
      writeTransactions "confirmedTransactions.csv" txs
      putStrLn "confirmedTransactions.csv"
