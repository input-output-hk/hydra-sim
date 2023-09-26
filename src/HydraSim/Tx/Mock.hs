{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module HydraSim.Tx.Mock (
  MockTx (..),
  TxRef (..),
  TxInput,
) where

import qualified Data.Set as Set
import Data.Time.Clock (DiffTime)
import HydraSim.DelayedComp
import HydraSim.Sized
import HydraSim.Tx.Class

-- | A transaction that is always valid, and takes a fixed time to validate.
data MockTx = MockTx
  { mtxRef :: TxRef MockTx
  , mtxValidationDelay :: DiffTime
  , mtxSize :: Size
  }
  deriving (Show)

instance Tx MockTx where
  newtype TxRef MockTx = TxId Int
    deriving (Eq, Ord, Show)

  newtype TxInput MockTx = TxInput ()
    deriving (Eq, Ord, Show)

  txRef = mtxRef
  txi _ = Set.empty
  txo _ = Set.empty

  txValidate _ tx = delayedComp True (mtxValidationDelay tx)

  txSort = id -- We do not have inputs/outputs, so any order is fine.

instance Sized (TxRef MockTx) where
  size _ = 32 -- txs are referenced by their 32 byte hashes
instance Sized MockTx where
  size = mtxSize

instance Eq MockTx where
  tx == tx' = txRef tx == txRef tx'
