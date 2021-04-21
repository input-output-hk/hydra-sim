module HydraSim.NodesSpec where

import HydraSim.Analyse
import HydraSim.Arbitraries (Sizing (Sizing))
import HydraSim.Options
import HydraSim.Run
import HydraSim.Types (ProtocolFlavor (CoordinatedVanilla), SnapStrategy (SnapAfter))
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Hydra Simulation" $ do
    describe "Simple Protocol w/o Conflicts" $ do
        it "confirms all transactions" $ property confirmsAllTransactions
    describe "Coordinated Vanilla Protocol" $ do
        it "confirms all transactions through snapshots" $ property confirmsAllTransactionsWithSnapshots

confirmsAllTransactions ::
    Sizing -> Property
confirmsAllTransactions (Sizing numTxs snapSize) =
    let capacity = 10 :: Integer
        traceRun = runSimulation defaultOptions{numberTxs = fromInteger numTxs, snapStrategy = SnapAfter (fromInteger snapSize)} capacity
        fullTrace = selectTraceHydraEvents DontShowDebugMessages traceRun
        confirmedTxsInSnapshots = sum $ txsInConfSnap <$> confirmedSnapshots fullTrace
        allConfirmedTxs = length $ confirmedTxs fullTrace
     in collect ("Snapshot size: " <> show snapSize) $
            allConfirmedTxs == fromInteger (numTxs * 3) && confirmedTxsInSnapshots <= allConfirmedTxs

confirmsAllTransactionsWithSnapshots ::
    Sizing -> Property
confirmsAllTransactionsWithSnapshots (Sizing numTxs snapSize) =
    let capacity = 10 :: Integer
        traceRun = runSimulation defaultOptions{numberTxs = fromInteger numTxs, snapStrategy = SnapAfter (fromInteger snapSize), protocolFlavor = CoordinatedVanilla} capacity
        fullTrace = selectTraceHydraEvents DontShowDebugMessages traceRun
        confirmedTxsInSnapshots = sum $ txsInConfSnap <$> confirmedSnapshots fullTrace
     in collect ("Snapshot size: " <> show snapSize) $
            fromInteger (numTxs * 3) == confirmedTxsInSnapshots
