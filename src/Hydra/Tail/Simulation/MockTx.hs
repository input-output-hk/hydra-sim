module Hydra.Tail.Simulation.MockTx
  ( MockTx(..)
  , sent
  , received
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
import Hydra.Tail.Simulation.PaymentWindow
    ( Lovelace (..) )
import Hydra.Tail.Simulation.SlotNo
    ( SlotNo (..) )
import HydraSim.DelayedComp
    ( delayedComp )
import HydraSim.Sized
    ( Size, Sized (..) )
import HydraSim.Tx.Class
    ( Tx (..) )
import HydraSim.Types
    ( NodeId (..) )

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data MockTx = MockTx
  { txId :: TxRef MockTx
  , txSize :: Size
  , txAmount :: Lovelace
  } deriving (Eq, Ord, Show)

sent :: MockTx -> Lovelace
sent =
  txAmount

received :: MockTx -> [NodeId] -> Lovelace
received tx recipients = Lovelace $
  unLovelace (txAmount tx) `div` toInteger (length recipients)

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
  size = txSize

-- TODO: Validation time should vary with the number of outputs?
validationTime
  :: MockTx
  -> DiffTime
validationTime =
  const (picosecondsToDiffTime 4 * 1e8)

mockTx
  :: NodeId
  -> SlotNo
  -> Lovelace
  -> Size
  -> MockTx
mockTx clientId slotNo txAmount txSize = MockTx
  { txId =
      -- NOTE: Arguably, we could want to keep this unobfuscasted
      -- for debugging purpose. Though putting these elements in
      -- the transaction 'body' might make more sense?
      (show clientId <> show slotNo <> show txAmount)
      & encodeBase16 . hash . T.encodeUtf8 . T.pack
      & TxRef
  , txAmount
  , txSize
  }
