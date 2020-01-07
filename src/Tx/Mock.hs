{-# LANGUAGE TypeFamilies #-}
module Tx.Mock where

import qualified Data.Set as Set
import Data.Time.Clock (DiffTime)

import DelayedComp
import Tx.Class

-- | A transaction that is always valid, and takes a fixed time to validate.
data MockTx = MockTx {
  mtxRef :: TxRef MockTx,
  mtxValidationDelay :: DiffTime
} deriving Show

instance Tx MockTx where
  newtype TxRef MockTx = TxId Int
    deriving (Eq, Ord, Show)
  newtype TxInput MockTx = TxInput ()
    deriving (Eq, Ord, Show)

  txRef = mtxRef
  txi _ = Set.empty
  txo _ = Set.empty

  txValidate _ tx = delayedComp True (mtxValidationDelay tx)
