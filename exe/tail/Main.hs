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
    ( Command (..), RunOptions (..), Verbosity (..), parseCommand )
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
      analyze <- analyzeSimulation (reportProgress options) options summary events trace
      pPrint analyze
 where
  reportProgress options = case verbosity options of
    Verbose -> putStrLn . ("..." <>) . show
    Quiet -> const (pure ())
