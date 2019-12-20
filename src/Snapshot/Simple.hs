{-# LANGUAGE TypeFamilies #-}
module Snapshot.Simple where

import Data.Time.Clock (DiffTime)
import Data.Set (Set)
import Object


newtype SnId = SnId Int
  deriving (Show, Eq, Ord)

data Object tx => SimpleSnap tx = SimpleSnap {
  snId :: SnId,
  snLast :: SnId,
  snNewTxs :: Set (ORef tx),
  snValidationTime :: DiffTime
  }
  deriving (Show, Ord, Eq)

instance Object tx => Object (SimpleSnap tx) where
  type ORef (SimpleSnap tx) = SnId
  oRef = snId

  data OValidationContext (SimpleSnap tx) = SimpleSnapVC SnId (Set tx)
  oValidate ctx sn = undefined -- TODO: implement
