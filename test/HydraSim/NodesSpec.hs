module HydraSim.NodesSpec where

import HydraSim.Analyse
import HydraSim.Options
import HydraSim.Run
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Hydra Simulation" $ do
    describe "Simple Protocol w/o Conflicts" $ do
        it "correctly confirms all transactions in snapshots" $ property confirmsAllTransactionsInSnapshots

confirmsAllTransactionsInSnapshots ::
    Positive (Small Integer) -> Bool
confirmsAllTransactionsInSnapshots (Positive (Small numTxs)) =
    let capacity = 10 :: Integer
        traceRun = runSimulation defaultOptions{numberTxs = fromInteger numTxs} capacity
        confirmedTxsInSnapshots = sum $ txsInConfSnap <$> confirmedSnapshots (selectTraceHydraEvents DontShowDebugMessages traceRun)
     in confirmedTxsInSnapshots == fromInteger (numTxs * 3)
