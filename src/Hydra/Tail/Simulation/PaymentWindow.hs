{-# LANGUAGE DerivingVia #-}

module Hydra.Tail.Simulation.PaymentWindow
  ( Balance (..)
  , initialBalance
  , modifyCurrent
  , Lovelace (..)
  , ada
  , PaymentWindowStatus (..)
  , viewPaymentWindow
  ) where

import Prelude

import GHC.Generics
    ( Generic )
import Quiet
    ( Quiet (..) )

newtype Lovelace = Lovelace { unLovelace :: Integer }
  deriving stock (Eq, Ord, Generic)
  deriving Show via Quiet Lovelace
  deriving (Read, Num, Enum) via Integer

ada :: Integer -> Lovelace
ada = Lovelace . (* 1_000_000)

data Balance = Balance
  { initial :: Lovelace
  , current :: Lovelace
  } deriving stock (Show, Eq, Generic)

initialBalance :: Lovelace -> Balance
initialBalance initial =
  Balance{initial, current}
 where
  current = initial

modifyCurrent :: (Lovelace -> Lovelace) -> Balance -> Balance
modifyCurrent fn b@Balance{current} =
  b { current = fn current }

instance Semigroup Balance where
  a <> b = Balance (initial a + initial b) (current a + current b)

instance Monoid Balance where
  mempty = Balance 0 0

data PaymentWindowStatus
  = InPaymentWindow
  | OutOfPaymentWindow
  deriving (Generic)

viewPaymentWindow :: Lovelace -> Balance -> Lovelace -> PaymentWindowStatus
viewPaymentWindow w Balance{initial,current} payment
  | current + payment < initial - w = OutOfPaymentWindow
  | current + payment > initial + w = OutOfPaymentWindow
  | otherwise = InPaymentWindow
