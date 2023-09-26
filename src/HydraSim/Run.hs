module HydraSim.Run where

import Control.Monad.IOSim (SimTrace, runSimTrace)
import Data.Time (DiffTime, picosecondsToDiffTime)
import HydraSim.Analyse (dynamicTracer)
import HydraSim.Examples.Nodes
import HydraSim.Options

runSimulation :: Integral a => Options -> a -> SimTrace ()
runSimulation opts capacity =
  let specs = flip map (regions opts) $ \center ->
        NodeSpec
          { nodeRegion = center
          , nodeNetworkCapacity = fromIntegral capacity
          , nodeTxs = txType opts
          , nodeTxConcurrency = fromIntegral $ concurrency opts
          , nodeTxNumber = fromIntegral $ numberTxs opts
          , nodeSnapStrategy = snapStrategy opts
          , nodeASigTime = secondsToDiffTimeTriplet $ asigTime opts
          , nodeHeadProtocolFlavor = protocolFlavor opts
          }
   in runSimTrace $ runNodes specs dynamicTracer

secondsToDiffTime :: Double -> DiffTime
secondsToDiffTime = picosecondsToDiffTime . round . (* 1e12)

secondsToDiffTimeTriplet :: (Double, Double, Double) -> (DiffTime, DiffTime, DiffTime)
secondsToDiffTimeTriplet (a, b, c) = (secondsToDiffTime a, secondsToDiffTime b, secondsToDiffTime c)
