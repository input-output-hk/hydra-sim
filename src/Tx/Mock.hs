{-# LANGUAGE TypeFamilies #-}
module Tx.Mock where

import Data.Time.Clock (DiffTime)

import Object

newtype TxId = TxId Int
  deriving (Show, Eq, Ord)

data MTx = MTx {
  txId :: TxId,
  txValidationTime :: DiffTime
  }
  deriving (Show, Ord, Eq)

instance Object MTx where
  type ORef MTx = TxId
  oRef = txId

  newtype OValidationContext MTx = MTxVC ()
  oValidate _ tx = (ObjectValid, Just (txValidationTime tx))
