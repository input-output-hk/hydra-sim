module HydraSim.NodesSpec where

import HydraSim.Analyse
import HydraSim.Options
import HydraSim.Run
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Hydra Simulation" $ do
    describe "Simple Protocol w/o Conflicts" $ do
        it "correctly confirms all transactions" $ property confirmsAllTransactions

confirmsAllTransactions ::
    Positive (Small Integer) -> Bool
confirmsAllTransactions (Positive (Small numTxs)) =
    let capacity = 10 :: Integer
        traceRun = runSimulation defaultOptions{numberTxs = fromInteger numTxs} capacity
     in length (confirmedTxs (selectTraceHydraEvents DontShowDebugMessages traceRun)) == fromInteger (numTxs * 3)
