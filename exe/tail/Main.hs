module Main where

import Prelude

import Hydra.Tail.Simulation
    ( analyzeSimulation
    , prepareSimulation
    , readEventsThrow
    , runSimulation
    , summarizeEvents
    , writeEvents
    )
import Hydra.Tail.Simulation.Options
    ( Command (..), parseCommand )
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
      let trace = runSimulation options events
      let analyze = analyzeSimulation options events trace
      pPrint (summarizeEvents events)
      pPrint analyze
