{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Arbitrary instances for various types used in Hydra simulation tests.
 Could be orphan instances hence the `OPTIONS_GHC` pragma.
-}
module HydraSim.Arbitraries where

import Test.QuickCheck

data Sizing = Sizing
    { numTxs :: Integer
    , snapshotSize :: Integer
    }
    deriving (Eq, Show)

instance Arbitrary Sizing where
    arbitrary = do
        numTxs <- getSmall . getPositive <$> arbitrary
        sn <- choose (1, numTxs `div` 10 + 1)
        pure $ Sizing numTxs sn
