{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module HydraSim.AnalyseSpec where

import Control.Monad.Class.MonadTime (Time (Time))
import Control.Monad.IOSim (ThreadLabel)
import HydraSim.Analyse
import Test.Hspec

spec :: Spec
spec = describe "Analyse Simulation" $ do
  describe "Average Confirmation Time" $ do
    it "returns 0 given no transaction" $ do
      avgConfTime [] `shouldBe` 0
    it "returns 0 given no transaction is confirmed" $ do
      avgConfTime [TxUnconfirmed someLabel someTime] `shouldBe` 0

  describe "Average Snapshot Size" $ do
    it "returns 0 given no confirmed snapshot" $ do
      avgSnapSize [] `shouldBe` 0
    it "returns 0 given unconfirmed snapshot with txs" $ do
      avgSnapSize [SnUnconfirmed someLabel 1 someTime] `shouldBe` 0

someTime :: Time
someTime = Time 10000

someLabel :: ThreadLabel
someLabel = "thread"
