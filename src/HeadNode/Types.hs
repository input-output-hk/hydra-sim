{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module HeadNode.Types where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Channel
import Object

-- | Sum type for transactions and snapshots
data (Object tx, Object sn) => TxOrSnapshot tx sn =
    TOSTx tx
  | TOSSn sn
  deriving (Show)

data (Object tx, Object sn) => TOSRef tx sn =
    TOSRTx (ORef tx)
  | TOSRSn (ORef sn)
  deriving (Show, Ord, Eq)

instance (Object tx, Object sn) =>
  Object (TxOrSnapshot tx sn) where
    type ORef (TxOrSnapshot tx sn) = TOSRef tx sn
    oRef (TOSTx tx) = TOSRTx (oRef tx)
    oRef (TOSSn sn) = TOSRSn (oRef sn)

    data OValidationContext (TxOrSnapshot tx sn) =
      TOSVC (OValidationContext tx) (OValidationContext sn)
    oValidate (TOSVC vcTx _) (TOSTx tx) =
      oValidate vcTx tx
    oValidate (TOSVC _ vcSn) (TOSSn sn) =
      oValidate vcSn sn

newtype NodeId = NodeId Int
  deriving (Show, Ord, Eq)

-- | State of a node in the head protocol
data (Object tx, Object sn) => HState m tx sn = HState {
  -- | All the members of the head, excluding this one.
  --
  -- We assume that our acknowledgement is implied for evry tx and snapshot we
  -- know about.
  hsPeers :: Set NodeId,
  -- | Transactions that we try to get confirmation on.
  hsTxs :: Map (ORef tx) (tx, Acknowledgement),
  -- | Channels for communication with peers.
  hsChannels :: (Map NodeId (Channel m (HeadProtocol (TxOrSnapshot tx sn)))),
  -- | validationContext for transactions and snapshots
  hsValidationContext :: OValidationContext (TxOrSnapshot tx sn)
  }

hnStateEmpty :: (Object tx, Object sn)
  => OValidationContext (TxOrSnapshot tx sn)
  -> HState m tx sn
hnStateEmpty validationContext = HState {
  hsPeers = Set.empty,
  hsTxs = Map.empty,
  hsChannels = Map.empty,
  hsValidationContext = validationContext
  }


-- | Tracks which nodes have acknowledged a transaction or snapshot.
data Acknowledgement =
  -- | An item that has been acknowledged by some nodes.
    Acknowledged (Set NodeId)
  -- | An item that has been confirmed (ackowledged by everyone).
  | Confirmed
  deriving (Eq, Show)

-- | Messages in the head protocol.
data (Object o) => HeadProtocol o =
  -- | Request to send a signature for a transaction to a given node
    HPSigReq NodeId o
  -- | Respond to a signature request. We jut use the NodeId to keep track of
  -- who has signed a transaction.
  | HPSigAck (ORef o) NodeId
  -- | Show a Tx with a multi-sig of every participant.
  | HPSigConf o
  -- | Note to self, to check whether a message is already acknowledged by
  -- everyone else.
  | CheckAcknowledgement (ORef o)
  deriving (Show, Eq)

-- | Decision of the node what to do in response to a message
data Decision m tx sn = Decision {
  -- | Updated state of the node, to be applied immediately
  decisionState :: HState m tx sn,
  -- | Trace of the decision
  decisionTrace :: TraceProtocolEvent (TxOrSnapshot tx sn),
  -- | Optional action to perform, concurrently, after updating the state. This
  -- can result in another 'HeadProtocol' message, which will be applied to the
  -- node itself.
  decisionJob :: m (Maybe (HeadProtocol (TxOrSnapshot tx sn)))
  }

-- | A function that encodes the transitions in the protocol. It takes a state and a message, and produces a 'Decision'.
type HStateTransformer m tx sn = HState m tx sn -> HeadProtocol (TxOrSnapshot tx sn) -> Decision m tx sn

-- | Traces
data TraceHydraEvent o =
    HydraMessage (TraceMessagingEvent o)
  | HydraProtocol (TraceProtocolEvent o)
  deriving (Eq, Show)

-- | Tracing messages that are sent/received between nodes.
data TraceMessagingEvent o =
    TraceMessageSent NodeId (HeadProtocol o)
  | TraceMessageReceived NodeId (HeadProtocol o)
  deriving (Eq, Show)

-- | Tracing how the node state changes as transactions are acknowledged, and
-- snapshots are produced.
data Object o => TraceProtocolEvent o =
  -- | A transaction has been acknowledged by a node.
    TPAck (ORef o) NodeId
  -- | A transaction has become stable (i.e., acknowledged by all the nodes).
  | TPConf (ORef o)
  -- | We tried a transition that failed to alter the state.
  | TPInvalidTransition String
  -- | Transition was valid, but had no effect
  | TPNoOp
  deriving (Eq, Show)

