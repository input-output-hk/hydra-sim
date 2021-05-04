module HydraSim.AnalyseSpec where

import HydraSim.Analyse
import Test.Hspec

spec :: Spec
spec = describe "Analyse Simulation" $ do
    describe "Average Confirmation Time" $ do
        it "returns Nothing given no transaction" $ do
            avgConfTime [] `shouldBe` Nothing
