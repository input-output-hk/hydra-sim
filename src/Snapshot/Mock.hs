{-# LANGUAGE TypeFamilies #-}
module Snapshot.Mock where

import Data.Time.Clock (DiffTime)

import Object

newtype SnId = SnId Int
  deriving (Show, Eq, Ord)

data MSn = MSn {
  snId :: SnId,
  snValidationTime :: DiffTime
  }
  deriving (Show, Ord, Eq)

instance Object MSn where
  type ORef MSn = SnId
  oRef = snId

  newtype OValidationContext MSn = MSnVC ()
  oValidate _ sn = (ObjectValid, Just (snValidationTime sn))
