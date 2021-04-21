{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
module HydraSim.Types
  ( -- * Nodes in the head protocol
    NodeId (..),
    NodeConf (..),
    HState (..),
    hStateEmpty,
    -- * Aggregate signatures
    MS (..),
    -- * Snapshots
    SnapN (..), nextSn, noSnapN,
    -- * Local transaction/snapshot objects
    TxO (..),
    Snap (..), emptySnap,
    -- * strategies
    TxSendStrategy (..), SnapStrategy (..),
    ProtocolFlavor(..),
    -- * Head protocol
    HeadProtocol (..),
    Decision (..),
    SendMessage (..),
    HStateTransformer,
    ProtocolHandler,
    -- * Traces
    TraceProtocolEvent (..)
  ) where

import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           HydraSim.DelayedComp
import           HydraSim.MSig.Mock
import           HydraSim.Sized
import           HydraSim.Tx.Class

-- | Identifiers for nodes in the head protocol.
newtype NodeId = NodeId Int
  deriving (Show, Ord, Eq, Sized)

-- | Local transaction objects
data Tx tx => TxO tx = TxO
  { txoIssuer :: NodeId,
    txoTx :: tx,
    txoT :: Set (TxRef tx),
    txoS :: Set Sig,
    txoSigma :: Maybe ASig
  } deriving (Eq, Ord, Show)

-- | Snapshot Sequence Number
newtype SnapN = SnapN Int
  deriving (Eq, Ord, Show, Sized)

nextSn :: SnapN -> SnapN
nextSn (SnapN n) = SnapN (n + 1)

noSnapN :: SnapN
noSnapN = SnapN (-1)

-- | Snapshot objects
data Tx tx => Snap tx = Snap {
  snos :: SnapN,
  snoO :: Set (TxInput tx),
  snoT :: Set (TxRef tx),
  snoS :: Set Sig,
  snoSigma :: Maybe ASig
  } deriving (Eq, Show)

emptySnap :: Tx tx => Snap tx
emptySnap = Snap {
  snos = noSnapN,
  snoO = Set.empty,
  snoT = Set.empty,
  snoS = Set.empty,
  snoSigma = Nothing
  }

data TxSendStrategy tx =
    SendNoTx
  | SendSingleTx tx
  -- | This will send the whole list of transactions at once.
  --
  -- If you want to send many transactions, this is bound to overflow some
  -- queues; use SendTxs instead.
  | SendTxsDumb [tx]
  -- | @SendTxs limit txs@ will only sends the next transaction when there are
  -- fewer than @limit@ transactions from the original list in flight (i.e., not
  -- confirmed yet).
  | SendTxs Int [tx]
  deriving (Show, Eq)

-- | Strategies for nodes to create snapshots
data SnapStrategy =
  -- | No snapshots are created.
    NoSnapshots
  -- | After a number of transactions have been confirmed, a snapshot is
  -- created.
  | SnapAfter Int
  deriving (Eq, Show, Read)

-- |The "flavor" of Head protocol to run in the simulation.
data ProtocolFlavor =
  -- | Simple protocol without conflict resolution as exposed in ICAR paper
    Vanilla
  -- | Coordinated protocol w/o conflict
  | CoordinatedVanilla
  deriving (Eq, Show, Read)
  
-- Multi-sig functionality
data Tx tx => MS tx = MS {
  ms_sig_tx :: SKey -> tx -> DelayedComp Sig,
  ms_asig_tx :: tx -> Set VKey -> Set Sig -> DelayedComp ASig,
  ms_verify_tx :: AVKey -> tx -> ASig -> DelayedComp Bool,

  ms_sig_sn :: SKey -> (SnapN, Set (TxInput tx)) -> DelayedComp Sig,
  ms_asig_sn :: (SnapN, Set (TxInput tx)) -> Set VKey -> Set Sig -> DelayedComp ASig,
  ms_verify_sn :: AVKey -> (SnapN, Set (TxInput tx)) -> ASig -> DelayedComp Bool
  }

data Tx tx => NodeConf tx = NodeConf {
  hcNodeId :: NodeId,
  hcTxSendStrategy :: TxSendStrategy tx,
  hcMSig :: MS tx,
  -- | Determine who is responsible to create which snapshot.
  hcLeaderFun :: SnapN -> NodeId,
  hcSnapshotStrategy :: SnapStrategy,
  -- | Which protocol to use
  hcProtocolHandler :: ProtocolHandler tx
  }

data Tx tx => HState tx = HState {
  hsSK :: !SKey,
  -- | Verification keys of all nodes (including this one)
  hsVKs :: !(Set VKey),
  -- | Latest signed snapshot number
  hsSnapNSig :: !SnapN,
  -- | Latest confirmed snapshot number
  hsSnapNConf :: !SnapN,
  -- | UTxO set signed by this node
  hsUTxOSig :: !(Set (TxInput tx)),
  -- | Confirmed UTxO set
  hsUTxOConf :: !(Set (TxInput tx)),
  -- | Latest signed snapshot
  hsSnapSig :: !(Snap tx),
  -- | Latest confirmed snapshot
  hsSnapConf :: !(Snap tx),
  -- | Set of txs signed by this node
  hsTxsSig :: !(Map (TxRef tx) (TxO tx)),
  -- | Set of confirmed txs
  hsTxsConf :: !(Map (TxRef tx) (TxO tx)),
  -- | Set of "in flight" transactions (transactions we have sent that are not
  -- yet confirmed).
  hsTxsInflight :: !(Set (TxRef tx))
  } deriving Eq
-- We'll want to show a node's state for debugging, but we want a custom
-- instance, suppressing showing the channels (which don't have a Show
-- instance), and the actual transactions (which would be too vebose -- but we
-- might change that in the future).
instance Tx tx => Show (HState tx) where
  show s = "HState { "
    ++ intercalate ", "
       [
         "hsSK=" ++ show (hsSK s),
         "hsVKs=" ++ show (hsVKs s),
         "hsSnapNSig=" ++ show (hsSnapNSig s),
         "hsSnapNConf=" ++ show (hsSnapNConf s),
         "hsUTxOSig=" ++ show (hsUTxOSig s),
         "hsUTxOConf=" ++ show (hsUTxOConf s),
         "hsSnapSig=" ++ show (hsSnapSig s),
         "hsSnapConf=" ++ show (hsSnapConf s),
         "hsTxsSig=" ++ show (Map.keysSet $ hsTxsSig s),
         "hsTxsConf=" ++ show (Map.keysSet $ hsTxsConf s),
         "hsTxsInflight=" ++ show (hsTxsInflight s)
       ]
    ++ " }"

hStateEmpty :: Tx tx => NodeId -> HState tx
hStateEmpty (NodeId i)= HState {
  hsSK = SKey i,
  hsVKs = Set.singleton $ VKey i,
  hsSnapNSig = noSnapN,
  hsSnapNConf = noSnapN,
  hsUTxOSig = Set.empty,
  hsUTxOConf = Set.empty,
  hsSnapSig = emptySnap,
  hsSnapConf = emptySnap,
  hsTxsSig = Map.empty,
  hsTxsConf = Map.empty,
  hsTxsInflight = Set.empty
  }

-- Protocol Stuff

-- | Events in the head protocol.
--
-- This includes messages that are exchanged between nodes, as well as local
-- client messages.
--
-- Corresponds to Fig 6 in the Hydra paper.
data Tx tx => HeadProtocol tx =
  -- messages from client

  -- | Submit a new transaction to the network
    New tx
  -- | Submit a new snapshot
  | NewSn

  -- inter-node messages

  -- | Request to send a signature for a transaction to a given node
  | SigReqTx tx
  -- | Response to a signature request.
  | SigAckTx (TxRef tx) Sig
  -- | Show a Tx with a multi-sig of every participant.
  | SigConfTx (TxRef tx) ASig

  -- | Request signature for a snapshot.
  | SigReqSn SnapN (Set (TxRef tx))
  -- | Provide signature for a snapshot.
  | SigAckSn SnapN Sig
  -- | Provide an aggregate signature for a confirmed snapshot.
  | SigConfSn SnapN ASig
  deriving (Show, Eq)
instance Tx tx => Sized (HeadProtocol tx) where
  size (New tx) = messageHeaderSize + size tx
  size NewSn = messageHeaderSize
  size (SigReqTx tx) = messageHeaderSize + size tx
  size (SigAckTx txref sig) = messageHeaderSize + size txref + size sig
  size (SigConfTx txref asig) = messageHeaderSize + size txref + size asig
  size (SigReqSn snapN txrefs) = messageHeaderSize + size snapN + sum (Set.map size txrefs)
  size (SigAckSn snapN sig) = messageHeaderSize + size snapN + size sig
  size (SigConfSn snapN asig) = messageHeaderSize + size snapN + size asig
messageHeaderSize :: Size
messageHeaderSize = 2 -- Given that we only have a handful of different message
                      -- in the protocol, this should be plenty.

-- | Decision of the node what to do in response to an event.
data Decision tx =
  -- | The event is invalid. Since the check might take some time, this involves
  -- a 'DelayedComp'.
    DecInvalid (DelayedComp ()) String
  -- | The event cannot be applied yet, but we should put it back in the queue.
  -- Again, this decision might have required some time, which we can encode via
  -- a 'DelayedComp'.
  | DecWait (DelayedComp ())
  -- | The event can be applied, yielding a new state. Optionally, this might
  -- cause messages to be sent to one or all nodes.
  | DecApply {
      -- | Updated state of the node.
      --
      -- The 'DelayedComp' should include both the time taken to compute the new
      -- state, and also any time used for validation checks.
      decisionState :: DelayedComp (HState tx),
      -- | Trace of the decision
      decisionTrace :: TraceProtocolEvent tx,
      -- | I addition to updating the local state, some events also trigger
      -- sending further messages to one or all nodes.
      --
      -- This is a 'DelayedComp', since preparing the message might require time.
      decisionMessage :: DelayedComp (SendMessage tx)
      }

-- | Events may trigger sending or broadcasting additional messages.
data SendMessage tx =
    SendNothing
  | SendTo NodeId (HeadProtocol tx)
  | Multicast (HeadProtocol tx)
  deriving Show

-- | A function that encodes a response to an event
--
-- It takes a state and a message, and produces a 'Decision'
type HStateTransformer tx = HState tx -> HeadProtocol tx -> Decision tx

-- | A function that handles incoming messages for a node.
type ProtocolHandler tx = NodeConf tx -> NodeId ->  HStateTransformer tx

-- | Tracing how the node state changes as transactions are acknowledged, and
-- snapshots are produced.
data Tx tx => TraceProtocolEvent tx =
  -- | A new transaction has been submitted by a node.
    TPTxNew (TxRef tx) NodeId
  -- | A transaction is being signed by a node.
  | TPTxSig (TxRef tx) NodeId
  -- | A tx signature from a node has been received.
  | TPTxAck (TxRef tx) NodeId
  -- | A transaction has become confirmed (i.e., acknowledged by all the nodes).
  | TPTxConf (TxRef tx)

  -- | A new snapshot has been submitted by a node.
  | TPSnNew SnapN NodeId
  -- | Snapshot is being signed by a node.
  | TPSnSig SnapN NodeId
  -- | Snapshot signature has been received from a node.
  | TPSnAck SnapN NodeId
  -- | Snapshot is confirmed.
  | TPSnConf SnapN

  -- | We tried a transition that failed to alter the state.
  | TPInvalidTransition String
  -- | Transition was valid, but had no effect
  | TPNoOp String
  deriving (Eq, Show)

