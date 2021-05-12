{-# LANGUAGE DerivingVia #-}

module Hydra.Tail.Simulation.SlotNo
  ( SlotNo(..)
  ) where

import Prelude

newtype SlotNo = SlotNo Integer
  deriving stock (Eq, Show)
  deriving (Num, Enum) via Integer
