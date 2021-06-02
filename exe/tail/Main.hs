module Main where

import Prelude

import Hydra.Tail.Simulation
    ( SimulationSummary (..)
    , analyzeSimulation
    , prepareSimulation
    , readEventsThrow
    , runSimulation
    , summarizeEvents
    , writeEvents
    )
import Hydra.Tail.Simulation.Options
    ( Command (..), parseCommand, withProgressReport )
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
      let summary = summarizeEvents events
      pPrint summary
      let trace = runSimulation options events
      analyze <- withProgressReport (lastSlot summary) options $ \reportProgress ->
        analyzeSimulation reportProgress options summary events trace
      pPrint analyze
