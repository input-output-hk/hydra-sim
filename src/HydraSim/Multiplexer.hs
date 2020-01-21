{-|
Module HydraSim.Multiplexer
Description: Simulate a network interface that communicates with multiple peers.

We model the timing of messages being sent between nodes in a network.

The network itself is modelled by a delay in time between each message leaving
the sending node and its arrival at the target node. The delay is determined by
the distance between nodes, and is constant for any pair of nodes. In
particular, it does not depend on the message size.

Before being sent across the network, each message has to be serialised. So the
event of a message being sent (or being received) by a node does not correspond
to a single point in time, but rather to a time interval. We take account for
that by modelling each message by its /leading/ and /trailing edge/ (see
'MessageEdge'). The time distance between leading and trailing edge of a message
will typically be linear in the message size (though we allow supplying any
function from message size to length in time, see 'mpWriteCapacity').

In addition to delaying messages through serialisation and network transport, we
also model two points of contention: on the egress side, nodes from one node to
all its peers have to pass through one networking interface, which supports a
finite data rate. Similarly, on the ingress side, arriving messages from all
nodes have to pass through one networking interface.

We model sending and receiving a message as follows:

- The sending node has one queue for all outgoing messages. An outgoing message
  is placed in this queue, together with the target 'NodeId'.

- There is one thread that reads messages from the queue. When it reads a
  message, it will split it into its leading and trailing edge.

    1. It will immediately put the leading edge in a 'Channel' to the target node

    2. It will sleep for a time given my 'mpWriteCapacity' applied to the message size.

    3. It will then put the trailing edge of the message into the queue.

  This models the contention and serialisation delay. Note that because there is
  only one thread sending messages, there is no need for explicit locking.

- Each 'Channel' will have a specified delay between a message being sent and
  received.

  This allows us to model the network delay due to geographical distribution.

- On the receiving side, we again model contention by having one thread that
  listens to all incoming channels.

    1. When it sees the leading edge of a message, it sleeps for a time given my
    'mpReadCapacity' applied to the message size.

    2. When it sees the trailing edge of a message, it puts the message itself
    into the inbox of the node (modelled as a queue holding messages paired with
    the 'NodeId' of the sending node).

Possible Improvements:

- Add some variance to the network delay.

- If messages do not fit the current TCP window, the network delay will
  increase. We do not model this yet.

-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module HydraSim.Multiplexer
  ( -- * Types
    Multiplexer, MessageEdge,
    -- * Setup
    newMultiplexer,
    connect,
    startMultiplexer,
    -- * Send messages
    sendTo,
    multicast,
    -- * Messages within a node
    sendToSelf,
    reenqueue,
    -- * Receive messages
    getMessage
  )
where

import           Control.Monad (forever, void, forM_)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Control.Tracer
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Time.Clock (DiffTime)
import           HydraSim.Channel
import           HydraSim.Multiplexer.Exception
import           HydraSim.Multiplexer.Trace
import           HydraSim.Sized
import           HydraSim.Types (NodeId)
import           Numeric.Natural

-- | We model a message traversing the network as two events: the leading and
-- the trailing edge of the message.
data MessageEdge a =
  -- | The leading edge of the message contains its size, allowing us to model
  -- the networking interface being busy while receiving it.
    Leading Size
  -- | The trailing edge of a message contains the actual message. This reflects
  -- (and ensures) that we can only act upon a message once we have received it
  -- completely.
  | Trailing a
    deriving Show

data Multiplexer m a = Multiplexer {
  -- | Queue for outbound messages.
  mpOutQueue :: TBQueue m (NodeId, a),
  -- | Queue for inbound messages.
  mpInQueue :: TBQueue m (NodeId, a),
  -- | Map holding a bidirectional 'Channel' to per peer.
  mpChannels :: TVar m (Map NodeId (Channel m (MessageEdge a))),
  -- |Write capacity of the network interface
  --
  -- Determines how much time it takes to serialise and send a message of a
  -- given size.
  mpWriteCapacity :: Size -> DiffTime,
  -- | Read capacity
  mpReadCapacity :: Size -> DiffTime
  }

-- | Create a new 'Multiplexer'.
newMultiplexer
  :: MonadSTM m
  => Natural -- ^ Buffer size for outbound message queue
  -> Natural -- ^ Buffer size for incoming message queue
  -> (Size -> DiffTime)
  -- ^ Write capacity
  --
  -- Determines how much time it takes to serialise and send a message of a
  -- given size.
  -> (Size -> DiffTime) -- ^ Read capacity.
  -> m (Multiplexer m a)
newMultiplexer outBufferSize inBufferSize writeCapacity readCapacity = do
  outQueue <- atomically $ newTBQueue outBufferSize
  inQueue <- atomically $ newTBQueue inBufferSize
  channels <- atomically $ newTVar (Map.empty)
  return $ Multiplexer {
    mpOutQueue = outQueue,
    mpInQueue = inQueue,
    mpChannels = channels,
    mpWriteCapacity = writeCapacity,
    mpReadCapacity = readCapacity
    }

-- | Connect two nodes.
connect
  :: (MonadSTM m, MonadTimer m, MonadAsync m)
  => m (Channel m (MessageEdge a), Channel m (MessageEdge a))
  -- ^ Creates a pair of connected channels.
  --
  -- The channels are responsible for adding the delay caused by the messages
  -- propagating through the network.
  -> (NodeId, Multiplexer m a)
  -> (NodeId, Multiplexer m a)
  -> m ()
connect createChannels (nodeId, mp) (nodeId', mp') = do
  (ch, ch') <- createChannels
  atomically $ do
    modifyTVar (mpChannels mp) $ \chs ->
      Map.insert nodeId' ch chs
    modifyTVar (mpChannels mp') $ \chs ->
      Map.insert nodeId ch' chs

-- | Start the multiplexer.
--
-- Once the multiplexer is started, messages can be sent with 'sendTo',
-- 'multicast', 'sendToSelf', and received by reading from the 'mpInQueue' via
-- 'getMessage'.
startMultiplexer
  ::( MonadSTM m, MonadThrow m, MonadTimer m, MonadAsync m,
     Sized a)
  => Tracer m (TraceMultiplexer a)
  -> Multiplexer m a
  -> m ()
startMultiplexer tracer mp = void $
  concurrently (messageSender tracer mp) (messageReceiver tracer mp)

-- | Place a message in the outgoing queue, so that it will be sent by
-- 'messageSender' once the network interface has capacity.
sendTo
  :: (MonadSTM m, Sized a)
  => Multiplexer m a
  -> NodeId
  -> a
  -> m ()
sendTo mp peer ms =
  atomically $ writeTBQueue (mpOutQueue mp) (peer, ms)

-- | Multicast sends a message to all peers, /and/ to the sending node as well.
--
-- Sending to self happens instantaneously, and does not consume netowrking
-- resources.
multicast
  :: (MonadSTM m, Sized a)
  => Tracer m (TraceMultiplexer a)
  -> Multiplexer m a
  -> NodeId -- ^ 'NodeId' of this node
  -> a
  -> m ()
multicast tracer mp nodeId ms = do
  traceWith tracer $ MPMulticast ms
  atomically $ do
    peers <- Map.keys <$> readTVar (mpChannels mp)
    forM_ peers $ \peer -> writeTBQueue (mpOutQueue mp) (peer, ms)
    writeTBQueue (mpInQueue mp) (nodeId, ms)

-- | Place a message directly into the 'mpInQueue'.
--
-- Used when nodes send messages to themselves. This happens instantatneous, and
-- does not consume any networking resources.
sendToSelf
  :: (MonadSTM m)
  => Tracer m (TraceMultiplexer a)
  -> Multiplexer m a
  -> NodeId -- ^ 'NodeId' of this node
  -> a
  -> m ()
sendToSelf tracer mp nodeId ms = do
  traceWith tracer $ MPSendSelf nodeId ms
  atomically $ writeTBQueue (mpInQueue mp) (nodeId, ms)

-- | Put a message back in the local 'mpInQueue', to be processed later.
--
-- In case 'mpInQueue' is currently empty, this will wait (in a non-blocking
-- manner) until another message arrives.
reenqueue
  :: (MonadSTM m, MonadAsync m)
  => Tracer m (TraceMultiplexer a)
  -> Multiplexer m a
  -> (NodeId, a)
  -> m ()
reenqueue tracer mp (peer, ms) = void $ async $ do
  atomically $ (isEmptyTBQueue (mpInQueue mp)) >>= \case
    True -> retry -- Do not put it back in the queue if the queue is empty,
                  -- otherwise we'll just loop forever!
    False -> writeTBQueue (mpInQueue mp) (peer, ms)
  traceWith tracer $ MPReenqueue peer ms

-- | Retrieve the next message in the 'mpInQueue'.
--
-- This is an STM action that will block until a message is availble.
getMessage
  :: (MonadSTM m)
  => Multiplexer m a
  -> STM m (NodeId, a)
getMessage mp = readTBQueue $ mpInQueue mp

-- | This is the thread that sends messages in the 'mpOutQueue' to peers.
messageSender
  :: ( MonadSTM m, MonadThrow m, MonadTimer m,
       Sized a)
  => Tracer m (TraceMultiplexer a)
  -> Multiplexer m a
  -> m ()
messageSender tracer mp = forever $ do
  (peer, ms) <- atomically (readTBQueue (mpOutQueue mp))
  sendMessage tracer mp peer ms

-- | Split a message into it's leading and trailing edge, and place both in the
-- appropriate 'Channel', with a delay depending on the message size.
sendMessage
  :: ( MonadSTM m, MonadThrow m, MonadTimer m,
       Sized a)
  => Tracer m (TraceMultiplexer a)
  -> Multiplexer m a
  -> NodeId
  -> a
  -> m ()
sendMessage tracer mp peer ms = do
  ch <- peerChannel mp peer
  traceWith tracer $ MPSendLeading peer bytes
  send ch leadingEdge
  threadDelay (mpWriteCapacity mp $ bytes)
  traceWith tracer $ MPSendTrailing peer ms
  send ch trailingEdge
  where
    bytes = size ms
    leadingEdge = Leading bytes
    trailingEdge = Trailing ms

-- | Thread responsible for reading messages from all the channels, and putting
-- them into the 'mpInQueue'.
messageReceiver
  :: forall m a . ( MonadSTM m, MonadThrow m, MonadTimer m,
       Sized a)
  => Tracer m (TraceMultiplexer a)
  -> Multiplexer m a
  -> m ()
messageReceiver tracer mp = forever $ do
  chList <- Map.toList <$> (atomically . readTVar $ mpChannels mp)
  (peer, msEdge) <- atomically $ pickNextMessage chList
  case msEdge of
    Leading bytes -> do
      traceWith tracer $ MPRecvLeading peer bytes
      threadDelay (mpReadCapacity mp $ bytes)
      traceWith tracer MPRecvIdling
    Trailing ms -> do
      traceWith tracer $ MPRecvTrailing peer ms
      atomically $ writeTBQueue (mpInQueue mp) (peer, ms)
  where
    -- TODO: There is a bias towards nodes early in the map, which we might want to remove.
    pickNextMessage :: [(NodeId, Channel m (MessageEdge a))] -> STM m (NodeId, MessageEdge a)
    pickNextMessage [] = retry
    pickNextMessage ((peer, ch):chs) =
      (recv ch >>= \ms -> return (peer, ms))
        `orElse` pickNextMessage chs

-- | Get the channel to talk to a specified peer.
--
-- Will throw a 'MissingChannel' exception if the channel does not exist.
peerChannel
  :: (MonadSTM m, MonadThrow m)
  => Multiplexer m a -> NodeId -> m (Channel m (MessageEdge a))
peerChannel mp peer =
  atomically (readTVar (mpChannels mp)) >>= \channels ->
  case peer `Map.lookup` channels of
    Nothing -> throwM $ MissingChannel peer
    Just ch -> return ch
