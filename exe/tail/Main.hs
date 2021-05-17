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
    ( Command (..), Options (..), parseCommand )
import Text.Pretty.Simple
    ( pPrint )

main :: IO ()
main = do
  parseCommand >>= \case
    Prepare options -> do
      pPrint options
      events <- prepareSimulation options
      writeEvents (filepath options) events

    Run options -> do
      events <- readEventsThrow (filepath options)
      let trace = runSimulation options events
      let analyze = analyzeSimulation options trace
      pPrint options
      pPrint (summarizeEvents events)
      pPrint analyze
