module HydraSim.NodesSpec where

import HydraSim.Analyse
import HydraSim.Options
import HydraSim.Run
import Test.Hspec

spec :: Spec
spec = describe "Hydra Simulation with 3 nodes" $ do
    it "correctly confirms all transactions" $ do
        let capacity :: Int
            capacity = 10

            traceRun = runSimulation defaultOptions capacity

        length (confirmedTxs (selectTraceHydraEvents DontShowDebugMessages traceRun)) `shouldBe` fromIntegral (numberTxs defaultOptions * 3)
