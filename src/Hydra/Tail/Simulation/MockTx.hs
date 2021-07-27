module Hydra.Tail.Simulation.MockTx (
    MockTx (..),
    sent,
    received,
    validationTime,
    mockTx,
    TxRef (..),
) where

import Prelude

import Data.Text (
    Text,
 )
import Data.Time.Clock (
    DiffTime,
    picosecondsToDiffTime,
 )
import Hydra.Tail.Simulation.PaymentWindow (
    Lovelace (..),
 )
import Hydra.Tail.Simulation.SlotNo (
    SlotNo (..),
 )
import HydraSim.DelayedComp (
    delayedComp,
 )
import HydraSim.Sized (
    Size (..),
    Sized (..),
 )
import HydraSim.Tx.Class (
    Tx (..),
 )
import HydraSim.Types (
    NodeId (..),
 )

import qualified Data.Set as Set
import qualified Data.Text as T

data MockTx = MockTx
    { txId :: TxRef MockTx
    , txSize :: Size
    , txAmount :: Lovelace
    , txRecipients :: [NodeId]
    }
    deriving (Eq, Ord, Show)

sent :: MockTx -> Lovelace
sent =
    txAmount

received :: MockTx -> Lovelace
received MockTx{txAmount, txRecipients} =
    Lovelace $
        unLovelace txAmount `div` toInteger (length txRecipients)

instance Tx MockTx where
    data TxRef MockTx = TxRef
        { slot :: Int
        , ref :: Text
        }
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
validationTime ::
    MockTx ->
    DiffTime
validationTime =
    const (picosecondsToDiffTime 4 * 1e8)

mockTx ::
    NodeId ->
    SlotNo ->
    Lovelace ->
    Size ->
    [NodeId] ->
    MockTx
mockTx (NodeId i) (SlotNo sl) txAmount@(Lovelace am) txSize@(Size sz) txRecipients =
    MockTx{txId, txAmount, txSize, txRecipients}
  where
    txId =
        TxRef
            { slot = fromInteger sl
            , ref =
                T.pack
                    (show sl <> show i <> show am <> show sz <> show (getNodeId <$> txRecipients))
            }
