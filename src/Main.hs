module Main where

import HydraSim.Analyse
import HydraSim.Examples.Channels
import HydraSim.Examples.Nodes

main :: IO ()
main = do
  let specs = flip map [IrelandAWS, LondonAWS, FrankfurtAWS] $ \center ->
        NodeSpec {nodeRegion = center, nodeNetworkCapacity = 100, nodeTxs = SendSimple 2 100}
  analyseRun (runNodes specs)
