{-# LANGUAGE LambdaCase #-}
module HeadNode where

import Control.Monad (forever, forM_, void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadTimer

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
  HeadNode m -> m ()
startNode hn = void $ concurrently (listener hn) (txSender hn)

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
          say $ concat ["Got message ", show message, " from ", show peerId]
          atomically $ writeTBQueue (hnInbox hn) (peerId, message)

-- | This is for the actual logic of the node, processing incoming messages.
listener :: (MonadSTM m, MonadSay m, MonadTimer m, MonadAsync m) =>
  HeadNode m -> m ()
listener hn = forever $ do
  atomically (readTBQueue $ hnInbox hn) >>= \case
    (nodeId, RequestTxSignature tx) -> do
      say $ concat ["Node ", show (hnId hn), " received ", show tx]
      threadDelay (txValidationTime tx)
      say $ concat ["Node ", show (hnId hn), " validated ", show tx]
      Map.lookup nodeId <$> atomically (readTVar (hnChannels hn)) >>= \case
        Just ch -> send ch $ ProvideTxSignature (tx, hnId hn)
        Nothing -> error "This should not happen."
    (_, ProvideTxSignature (tx, nodeId)) ->
      say $ concat ["got confirmation for ", show tx, " from ", show nodeId]

txSender :: (MonadAsync m, MonadSay m) =>
  HeadNode m -> m ()
txSender hn = case (hnTxSendStrategy hn) of
  SendSingleTx tx ->  do
    channelList <- Map.toList <$> atomically (readTVar (hnChannels hn))
    forM_ channelList $ \(nodeId, ch) -> do
      say $ concat ["sending ", show tx, " from ", show (hnId hn), " to ", show nodeId]
      send ch $ RequestTxSignature tx
  SendNoTx -> return ()
