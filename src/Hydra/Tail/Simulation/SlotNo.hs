{-# LANGUAGE DerivingVia #-}

module Hydra.Tail.Simulation.SlotNo (
  SlotNo (..),
) where

import Prelude

import GHC.Generics (
  Generic,
 )
import Quiet (
  Quiet (..),
 )

newtype SlotNo = SlotNo {unSlotNo :: Integer}
  deriving stock (Eq, Ord, Generic)
  deriving (Show) via Quiet SlotNo
  deriving (Num, Enum) via Integer
