module Main where

import Prelude

import Control.Monad
    ( forM_ )
import Hydra.Tail.Simulation
    ( analyzeSimulation, prepareSimulation, runSimulation )
import Hydra.Tail.Simulation.Options
    ( Command (..), parseCommand )
import Text.Pretty.Simple
    ( pPrint )

main :: IO ()
main = do
  parseCommand >>= \case
    Prepare options -> do
      events <- prepareSimulation options
      -- TODO: Write events to file
      forM_ events pPrint

    Run options -> do
      -- TODO: Read events from file when a file is provided.
      events <- prepareSimulation options
      let trace = runSimulation options events
      let analyze = analyzeSimulation options trace
      pPrint options
      pPrint analyze
