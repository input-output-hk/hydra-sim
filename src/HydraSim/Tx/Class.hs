{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module HydraSim.Tx.Class
  ( Tx (..)
  , applyValidTxs
  ) where


import           Data.Kind (Type)
import           Data.List (foldl')
import           Data.Set (Set)
import qualified Data.Set as Set
import           HydraSim.DelayedComp

-- | Abstract transaction.
class (Eq (TxRef tx), Ord (TxRef tx), Show (TxRef tx),
       Eq (TxInput tx), Ord (TxInput tx), Show (TxInput tx),
       Show tx
      )
  => Tx tx where

  -- | Transaction reference (i.e., hash)
  data TxRef tx :: Type
  txRef :: tx -> TxRef tx

  -- | Type of transaction inputs and outputs
  data TxInput tx :: Type
  -- | Inputs consumed by this transaction
  txi :: tx -> Set (TxInput tx)
  -- | Outputs created by this transaction
  txo :: tx -> Set (TxInput tx)

  -- | Validate a transaction against a UTxO set.
  --
  -- Since transaction validation can involve running scripts, we use a
  -- 'DelayedComp'.
  --
  -- The default implementation of 'txValidate' just checks that all inputs
  -- are included in the UTxO (so it assues that validators always succeed, and
  -- take zero time).
  txValidate :: Set (TxInput tx) -> tx -> DelayedComp Bool
  txValidate utxo tx = promptComp $ txi tx `Set.isSubsetOf` utxo

  -- | Apply a transaction that is known to be valid.
  txApplyValid :: Set (TxInput tx) -> tx -> Set (TxInput tx)
  txApplyValid utxo tx = (utxo Set.\\ txi tx) `Set.union` txo tx

  -- | Apply a transaction to a utxo set (including validating the transaction
  -- against that utxo set).
  txApply :: Set (TxInput tx) -> tx -> DelayedComp (Maybe (Set (TxInput tx)))
  txApply utxo tx = txValidate utxo tx >>= \case
    False -> return Nothing
    True -> return . Just $ txApplyValid utxo tx

  -- | Sort transactions respecting partial order of inputs/outputs
  txSort :: [tx] -> [tx]

applyValidTxs :: Tx tx => Set (TxInput tx) -> [tx] -> Set (TxInput tx)
applyValidTxs initialUtxo txSet =
  foldl' txApplyValid initialUtxo (txSort txSet)
