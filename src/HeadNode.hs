{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module HeadNode
  ( newNode,
    connectNodes,
    startNode
  ) where

import Control.Monad (forever, forM_, void)

import qualified Data.Map.Strict as Map

import qualified Data.Set as Set

-- imports from io-sim, io-sim-classes, contra-tracer
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadTimer
import Control.Tracer

-- imports from this package
import Channel
import DelayedComp
import HeadNode.Handler (handleMessage)

import HeadNode.Types
import MSig.Mock
import Tx.Class

newNode :: (MonadSTM m, Tx tx) =>
  NodeConf tx -> m (HeadNode m tx)
newNode conf = do
  state <- newTMVarM $ hnStateEmpty (hcNodeId conf)
  inbox <- atomically $ newTBQueue 100 -- TODO: make this configurable
  handlers <- newTVarM Map.empty
  return $ HeadNode {
    hnConf = conf,
    hnState = state,
    hnInbox = inbox,
    hnPeerHandlers = handlers
    }

connectNodes
  :: (MonadAsync m, Tx tx)
  => m (Channel m (HeadProtocol tx), Channel m (HeadProtocol tx))
  -> HeadNode m tx
  -> HeadNode m tx
  -> m ()
connectNodes createChannels node node' = do
  (ch, ch') <- createChannels
  addPeer node (hcNodeId $ hnConf node') ch
  addPeer node' (hcNodeId $ hnConf node) ch'

startNode
  :: (MonadSTM m, MonadTimer m, MonadAsync m,
       Tx tx)
  => Tracer m (TraceHydraEvent tx)
  -> HeadNode m tx -> m ()
startNode tracer hn = void $ concurrently (listener tracer hn) (txSender tracer hn)

-- | Add a peer, and install a thread that will collect messages from the
-- channel to the main inbox of the node.
addPeer
  :: (MonadSTM m, MonadAsync m,
           Tx tx)
  => HeadNode m tx -> NodeId -> Channel m (HeadProtocol tx) -> m ()
addPeer hn peerId@(NodeId i) peerChannel = do
  peerHandler <- async protocolHandler
  atomically $ do
    state <- takeTMVar (hnState hn)
    putTMVar (hnState hn) $!
      state { hsVKs = Set.insert (VKey i) $ hsVKs state
            , hsChannels = Map.insert peerId peerChannel $ hsChannels state
            }
    modifyTVar (hnPeerHandlers hn) $ Map.insert peerId peerHandler
  where
    protocolHandler = forever $ do
      recv peerChannel >>= \case
        Nothing -> return ()
        Just message -> do
          atomically $ writeTBQueue (hnInbox hn) (peerId, message)

-- | Add a message from the client (as opposed to from a node) to the message queue.
--
-- This is used for triggering events like transaction submission or snapshot
-- creation.
clientMessage
  :: (MonadSTM m, Tx tx)
  => Tracer m (TraceHydraEvent tx)
  -> HeadNode m  tx
  -> HeadProtocol tx
  -> m ()
clientMessage tracer hn message = do
  traceWith messageTracer $ TraceMessageClient message
  atomically $ writeTBQueue (hnInbox hn) (hcNodeId (hnConf hn), message)
  where
    messageTracer = contramap HydraMessage tracer

-- | This is for the actual logic of the node, processing incoming messages.
listener
  :: forall m tx .
     (MonadSTM m, MonadTimer m, MonadAsync m,
      Tx tx)
  => Tracer m (TraceHydraEvent tx)
  -> HeadNode m tx -> m ()
listener tracer hn = forever $ do
  atomically (readTBQueue $ hnInbox hn) >>= \(peer, ms) -> do
    traceWith messageTracer (TraceMessageReceived peer ms)
    applyMessage peer ms

  where
    messageTracer = contramap HydraMessage tracer
    protocolTracer = contramap HydraProtocol tracer
    hydraDebugTracer = contramap HydraDebug tracer
    thisId = hcNodeId (hnConf hn)
    applyMessage :: NodeId -> HeadProtocol tx -> m ()
    applyMessage peer ms = do
      traceWith hydraDebugTracer ("applyMessage " ++ show peer
                              ++ " " ++ show ms)
      state <- atomically $ takeTMVar (hnState hn)
      traceWith hydraDebugTracer (" state = " ++ show state)
      case handleMessage (hnConf hn) peer state ms of
        DecApply stateUpdate trace ms' -> do
          -- 'runComp' advances the time by the amount the handler takes,
          -- and unwraps the result
          !state' <- runComp stateUpdate
          atomically $ putTMVar (hnState hn) state'
          traceWith hydraDebugTracer (" state' = " ++ show state')
          traceWith protocolTracer trace
          -- TODO: We _could_ think of adding some parallelism here, by doing
          -- this asynchronously. That would slightly violate the assumption
          -- that there is only one event being processed at any time, but since
          -- the state is locked in a 'TMVar', that should be fine.
          runComp ms' >>= sendMessage
        DecWait comp -> do
          runComp comp
          atomically $ do
            writeTBQueue (hnInbox hn) (peer, ms)
            putTMVar (hnState hn) state
          traceWith messageTracer (TraceMessageRequeued ms)
        DecInvalid comp errmsg -> do
          runComp comp
          traceWith protocolTracer (TPInvalidTransition errmsg)
          atomically $ putTMVar (hnState hn) state
    sendMessage :: SendMessage tx -> m ()
    sendMessage SendNothing = return ()
    sendMessage (SendTo peer ms)
      -- messges to the same node are just added to the inbox directly
      | peer == thisId = do
          traceWith messageTracer (TraceMessageSent peer ms)
          atomically $ writeTBQueue (hnInbox hn) (peer, ms)
      | otherwise = do
          s <- atomically $ readTMVar (hnState hn)
          case (Map.lookup peer) . hsChannels $ s of
            Just ch -> do
              traceWith messageTracer (TraceMessageSent peer ms)
              send ch ms
            Nothing ->
              error $ concat ["Error in ", show thisId
                             , ": Did not find peer ", show peer
                             , " in ", show . Map.keys . hsChannels $ s]
    sendMessage (Multicast ms) = do
      traceWith messageTracer (TraceMessageMulticast ms)
      s <- atomically $ readTMVar (hnState hn)
      forM_ (Map.toList $ hsChannels s) $ \(_nodeId, ch) ->
        send ch ms
      -- as described in the paper, multicasting a message always is followed by
      -- the sending node itself acting on the message, as if it had been
      -- received by another node:
      applyMessage thisId ms



txSender :: (MonadAsync m, Tx tx) =>
  Tracer m (TraceHydraEvent tx)
  -> HeadNode m tx -> m ()
txSender tracer hn = case (hcTxSendStrategy (hnConf hn)) of
  SendSingleTx tx -> clientMessage tracer hn (New tx)
  SendNoTx -> return ()
