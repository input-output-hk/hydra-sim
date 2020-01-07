module HeadNode.Types where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Channel
import Tx.Class

-- Nodes

data Tx tx => HState m tx = HState {
  -- | All the members of the head, excluding this one.
  --
  -- We assume that our acknowledgement is implied for every tx and snapshot we
  -- know about.
  hsPeers :: Set NodeId,
  -- | Transactions that we try to get confirmation on.
  hsTxs :: Map (TxRef tx) (tx, Acknowledgement),
  -- | Channels for communication with peers.
  hsChannels :: (Map NodeId (Channel m (HeadProtocol tx))),
  hsUtXOConf :: Set (TxInput tx)
  }

hnStateEmpty :: Tx tx => HState m tx
hnStateEmpty = HState {
  hsPeers = Set.empty,
  hsTxs = Map.empty,
  hsChannels = Map.empty,
  hsUtXOConf = Set.empty
  }


newtype NodeId = NodeId Int
  deriving (Show, Ord, Eq)


-- | Tracks which nodes have acknowledged a transaction or snapshot.
--
-- Transactions and Snapshots are sent to all nodes for confirmation, to
-- guarantee consensus. This data type is used to keep track of who has
-- confirmed an item.
data Acknowledgement =
  -- | An item that has been acknowledged by some nodes.
    AcknowledgedPartly (Set NodeId)
  -- | An item that has been ackowledged by everyone.
  | AcknowledgedFully
  deriving (Eq, Show)

-- Protocol Stuff

data Tx tx => HeadProtocol tx =
  -- | Request to send a signature for a transaction to a given node
    RequestTxSignature tx NodeId
  -- | Respond to a signature request. We jut use the NodeId to keep track of
  -- who has signed a transaction.
  | ProvideTxSignature (TxRef tx) NodeId
  -- | Show a Tx with a multi-sig of every participant.
  | ShowAcknowledgedTx tx
  -- | Note to self, to check whether a message is already acknowledged by
  -- everyone else.
  | CheckAcknowledgement (TxRef tx)
  deriving (Show, Eq)

-- | Decision of the node what to do in response to a message
data Decision m tx = Decision {
  -- | Updated state of the node, to be applied immediately
  decisionState :: HState m tx,
  -- | Trace of the decision
  decisionTrace :: TraceProtocolEvent tx,
  -- | Optional action to perform, concurrently, after updating the state. This
  -- can result in another 'HeadProtocol' message, which will be applied to the
  -- node itself.
  decisionJob :: m (Maybe (HeadProtocol tx))
  }

-- | A function that encodes the logic of the protocol. It takes a state and a message, and produces a pair containing the new state, and a list of events to be traced.
type HStateTransformer m tx = HState m tx -> HeadProtocol tx -> Decision m tx

-- Traces

data TraceHydraEvent tx =
    HydraMessage (TraceMessagingEvent tx)
  | HydraProtocol (TraceProtocolEvent tx)
  deriving (Eq, Show)

-- | Tracing messages that are sent/received between nodes.
data TraceMessagingEvent tx =
    TraceMessageSent NodeId (HeadProtocol tx)
  | TraceMessageReceived NodeId (HeadProtocol tx)
  deriving (Eq, Show)

-- | Tracing how the node state changes as transactions are acknowledged, and
-- snapshots are produced.
data Tx tx => TraceProtocolEvent tx =
  -- | A transaction has been acknowledged by a node.
    TPTxAcknowledged (TxRef tx) NodeId
  -- | A transaction has become stable (i.e., acknowledged by all the nodes).
  | TPTxStable (TxRef tx)
  -- | We tried a transition that failed to alter the state.
  | TPInvalidTransition String
  -- | Transition was valid, but had no effect
  | TPNoOp
  deriving (Eq, Show)

