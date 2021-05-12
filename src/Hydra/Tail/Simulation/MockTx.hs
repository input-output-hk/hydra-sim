module Hydra.Tail.Simulation.MockTx
  ( MockTx
  , validationTime
  , mockTx
  ) where

import Prelude

import Crypto.Hash.MD5
    ( hash )
import Data.ByteString.Base16
    ( encodeBase16 )
import Data.Function
    ( (&) )
import Data.Text
    ( Text )
import Data.Time.Clock
    ( DiffTime, picosecondsToDiffTime )
import Hydra.Tail.Simulation.SlotNo
    ( SlotNo (..) )
import HydraSim.DelayedComp
    ( delayedComp )
import HydraSim.Sized
    ( Sized (..) )
import HydraSim.Tx.Class
    ( Tx (..) )
import HydraSim.Types
    ( NodeId (..) )

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data MockTx = MockTx
  { txId :: TxRef MockTx
  , numberOfOutputs :: Integer
  } deriving (Eq, Ord, Show)

instance Tx MockTx where
  newtype TxRef MockTx = TxRef Text
    deriving (Eq, Ord, Show)

  newtype TxInput MockTx = TxInput ()
    deriving (Eq, Ord, Show)

  txRef = txId
  txi _ = Set.empty
  txo _ = Set.empty

  txValidate _ = delayedComp True . validationTime
  txSort = id

instance Sized (TxRef MockTx) where
  size = const 32

instance Sized MockTx where
  size MockTx{numberOfOutputs} =
    150 + fromInteger (70 * numberOfOutputs)

-- TODO: Validation time should vary with the number of outputs?
validationTime
  :: MockTx
  -> DiffTime
validationTime =
  const (picosecondsToDiffTime 4 * 1e8)

mockTx
  :: NodeId
  -> SlotNo
  -> MockTx
mockTx clientId slotNo = MockTx
  { txId =
      -- NOTE: Arguably, we could want to keep this unobfuscasted
      -- for debugging purpose. Though putting these elements in
      -- the transaction 'body' might make more sense?
      (show clientId <> show slotNo)
      & encodeBase16 . hash . T.encodeUtf8 . T.pack
      & TxRef
  , numberOfOutputs =
      1
  }
