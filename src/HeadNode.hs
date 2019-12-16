{-# LANGUAGE LambdaCase #-}
module HeadNode where

import Control.Monad (forever, forM_, void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- imports from io-sim, io-sim-classes, contra-tracer
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadTimer
import Control.Tracer

-- imports from this package
import Channel
import HeadProtocol

data TxSendStrategy =
    SendNoTx
  | SendSingleTx Tx
  deriving (Show, Eq)

data HeadNode m = HeadNode {
  hnId :: NodeId,
  hnInbox :: TBQueue m (NodeId, HeadProtocol),
  hnChannels :: TVar m (Map NodeId (Channel m HeadProtocol)),
  hnPeerHandlers :: TVar m (Map NodeId (Async m ())),
  hnTxSendStrategy :: TxSendStrategy
  }

newNode :: MonadSTM m =>
  NodeId -> TxSendStrategy -> m (HeadNode m)
newNode nodeId txSendStrategy = do
  inbox <- atomically $ newTBQueue 100 -- TODO: make this configurable
  channels <- newTVarM Map.empty
  handlers <- newTVarM Map.empty
  return $ HeadNode {
    hnId = nodeId,
    hnInbox = inbox,
    hnChannels = channels,
    hnPeerHandlers = handlers,
    hnTxSendStrategy = txSendStrategy
    }

startNode :: (MonadSTM m, MonadSay m, MonadTimer m, MonadAsync m) =>
  Tracer m TraceProtocolEvent
  -> HeadNode m -> m ()
startNode tracer hn = void $ concurrently (listener tracer hn) (txSender tracer hn)

-- | Add a peer, and install a thread that will collect messages from the
-- channel to the main inbox of the node.
addPeer :: (MonadSTM m, MonadAsync m, MonadSay m) =>
  HeadNode m -> NodeId -> Channel m HeadProtocol -> m ()
addPeer hn peerId peerChannel = do
  peerHandler <- async protocolHandler
  atomically $ do
    modifyTVar (hnChannels hn) $ Map.insert peerId peerChannel
    modifyTVar (hnPeerHandlers hn) $ Map.insert peerId peerHandler
  where
    protocolHandler = forever $ do
      recv peerChannel >>= \case
        Nothing -> return ()
        Just message -> do
          atomically $ writeTBQueue (hnInbox hn) (peerId, message)

-- | This is for the actual logic of the node, processing incoming messages.
listener :: (MonadSTM m, MonadSay m, MonadTimer m, MonadAsync m) =>
  Tracer m TraceProtocolEvent
  -> HeadNode m -> m ()
listener tracer hn = forever $ do
  atomically (readTBQueue $ hnInbox hn) >>= \case
    (nodeId, RequestTxSignature tx) -> do
      traceWith tracer $ TraceReceivedTxForSignature (txId tx) nodeId
      threadDelay (txValidationTime tx)
      Map.lookup nodeId <$> atomically (readTVar (hnChannels hn)) >>= \case
        Just ch -> send ch $ ProvideTxSignature (tx, hnId hn)
        Nothing -> error "This should not happen."
    (_, ProvideTxSignature (tx, nodeId)) ->
      traceWith tracer $ TraceReceivedSignatureForTx (txId tx) nodeId

txSender :: (MonadAsync m, MonadSay m) =>
  Tracer m TraceProtocolEvent
  ->   HeadNode m -> m ()
txSender tracer hn = case (hnTxSendStrategy hn) of
  SendSingleTx tx ->  do
    channelList <- Map.toList <$> atomically (readTVar (hnChannels hn))
    forM_ channelList $ \(nodeId, ch) -> do
      traceWith tracer $ TraceSentTxForSignature (txId tx) nodeId
      send ch $ RequestTxSignature tx
  SendNoTx -> return ()
