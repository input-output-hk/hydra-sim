module Main where

import Prelude

import Hydra.Tail.Simulation
    ( analyzeSimulation, runSimulation )
import Hydra.Tail.Simulation.Options
    ( parseOptions )
import Text.Pretty.Simple
    ( pPrint )

main :: IO ()
main = do
  options <- parseOptions
  let trace = runSimulation options
  let analyze = analyzeSimulation options trace
  pPrint options
  pPrint analyze
