module HydraSim.AnalyseSpec where

import Control.Monad.Class.MonadTime (Time (Time))
import Control.Monad.IOSim (ThreadLabel)
import HydraSim.Analyse
import Test.Hspec

spec :: Spec
spec = describe "Analyse Simulation" $ do
    describe "Average Confirmation Time" $ do
        it "returns Nothing given no transaction" $ do
            avgConfTime [] `shouldBe` Nothing
        it "returns Nothing given no transaction is confirmed" $ do
            avgConfTime [TxUnconfirmed someLabel someTime] `shouldBe` Nothing

someTime :: Time
someTime = Time 10000

someLabel :: ThreadLabel
someLabel = "thread"
