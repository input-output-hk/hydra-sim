module HeadNode.Types where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Channel
import DelayedComp
import MSig.Mock
import Tx.Class

-- | Local transaction objects
data Tx tx => TxO tx = TxO
  { txoIssuer :: NodeId,
    txoTx :: tx,
    txoT :: Set (TxRef tx),
    txoS :: Set Sig,
    txoSigma :: Maybe ASig
  } deriving (Eq, Ord, Show)

-- Nodes

-- | Identifiers for nodes in the head protocol.
newtype NodeId = NodeId Int
  deriving (Show, Ord, Eq)

data Tx tx => HState m tx = HState {
  hsPartyIndex :: Int,
  hsSK :: SKey,
  -- | Verification keys of all nodes (including this one)
  hsVKs :: Set VKey,
  -- | Channels for communication with peers.
  hsChannels :: (Map NodeId (Channel m (HeadProtocol tx))),
  -- | UTxO set signed by this node
  hsUTxOSig :: Set (TxInput tx),
  -- | Confirmed UTxO set
  hsUTxOConf :: Set (TxInput tx),
  -- | Set of txs signed by this node
  hsTxsSig :: Map (TxRef tx) (TxO tx),
  -- | Set of confirmed txs
  hsTxsConf :: Map (TxRef tx) (TxO tx)
  }

hnStateEmpty :: Tx tx => NodeId -> HState m tx
hnStateEmpty (NodeId i)= HState {
  hsPartyIndex = i,
  hsSK = SKey i,
  hsVKs = Set.singleton $ VKey i,
  hsChannels = Map.empty,
  hsUTxOSig = Set.empty,
  hsUTxOConf = Set.empty,
  hsTxsSig = Map.empty,
  hsTxsConf = Map.empty
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

  -- inter-node messages

  -- | Request to send a signature for a transaction to a given node
  | SigReqTx tx
  -- | Response to a signature request.
  | SigAckTx (TxRef tx) Sig
  -- | Show a Tx with a multi-sig of every participant.
  | SigConfTx (TxRef tx) ASig
  deriving (Show, Eq)

-- | Decision of the node what to do in response to a message
data Decision m tx = Decision {
  -- | Updated state of the node, to be applied immediately
  decisionState :: DelayedComp (HState m tx),
  -- | Trace of the decision
  decisionTrace :: TraceProtocolEvent tx,
  -- | I addition to updating the local state, some events also trigger sending
  -- further messages to one or all nodes.
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
type HStateTransformer m tx = HState m tx -> HeadProtocol tx -> Decision m tx

-- | Traces in the simulation
data TraceHydraEvent tx =
    HydraMessage (TraceMessagingEvent tx)
  | HydraProtocol (TraceProtocolEvent tx)
  deriving (Eq, Show)

-- | Tracing messages that are sent/received between nodes.
data TraceMessagingEvent tx =
    TraceMessageSent NodeId (HeadProtocol tx)
  | TraceMessageMulticast (HeadProtocol tx)
  | TraceMessageClient (HeadProtocol tx)
  | TraceMessageReceived NodeId (HeadProtocol tx)
  | TraceMessageRequeued (HeadProtocol tx)
  deriving (Eq, Show)

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
  -- | We tried a transition that failed to alter the state.
  | TPInvalidTransition String
  -- | Transition was valid, but had no effect
  | TPNoOp String
  deriving (Eq, Show)

