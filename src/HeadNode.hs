{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HeadNode where

import Control.Monad (forever, forM_, void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- imports from io-sim, io-sim-classes, contra-tracer
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadTimer
import Control.Tracer

-- imports from this package
import Channel
import DelayedComp
import HeadNode.Types
import Tx.Class

data TxSendStrategy tx =
    SendNoTx
  | SendSingleTx tx
  deriving (Show, Eq)

data Tx tx => HeadNode m tx = HeadNode {
  hnId :: NodeId,
  hnState :: TVar m (HState m tx),
  hnInbox :: TBQueue m (NodeId, HeadProtocol tx),
  hnPeerHandlers :: TVar m (Map NodeId (Async m ())),
  hnTxSendStrategy :: TxSendStrategy tx
  }

newNode :: (MonadSTM m, Tx tx) =>
  NodeId -> TxSendStrategy tx -> m (HeadNode m tx)
newNode nodeId txSendStrategy = do
  state <- newTVarM hnStateEmpty
  inbox <- atomically $ newTBQueue 100 -- TODO: make this configurable
  handlers <- newTVarM Map.empty
  return $ HeadNode {
    hnId = nodeId,
    hnState = state,
    hnInbox = inbox,
    hnPeerHandlers = handlers,
    hnTxSendStrategy = txSendStrategy
    }

startNode
  :: (MonadSTM m, MonadSay m, MonadTimer m, MonadAsync m,
       Tx tx)
  => Tracer m (TraceHydraEvent tx)
  -> HeadNode m tx -> m ()
startNode tracer hn = void $ concurrently (listener tracer hn) (txSender tracer hn)

-- | Add a peer, and install a thread that will collect messages from the
-- channel to the main inbox of the node.
addPeer
  :: (MonadSTM m, MonadAsync m, MonadSay m,
           Tx tx)
  => HeadNode m tx -> NodeId -> Channel m (HeadProtocol tx) -> m ()
addPeer hn peerId peerChannel = do
  peerHandler <- async protocolHandler
  atomically $ do
    modifyTVar (hnState hn) $ \state ->
      state { hsPeers = Set.insert peerId $ hsPeers state
            , hsChannels = Map.insert peerId peerChannel $ hsChannels state
            }
    modifyTVar (hnPeerHandlers hn) $ Map.insert peerId peerHandler
  where
    protocolHandler = forever $ do
      recv peerChannel >>= \case
        Nothing -> return ()
        Just message -> do
          atomically $ writeTBQueue (hnInbox hn) (peerId, message)

-- | This is for the actual logic of the node, processing incoming messages.
listener
  :: forall m tx .
     (MonadSTM m, MonadSay m, MonadTimer m, MonadAsync m,
      Tx tx)
  => Tracer m (TraceHydraEvent tx)
  -> HeadNode m tx -> m ()
listener tracer hn = forever $ do
  atomically (readTBQueue $ hnInbox hn) >>= \(peer, ms) -> do
    traceWith messageTracer (TraceMessageReceived peer ms)
    applyMessage ms

  where messageTracer = contramap HydraMessage tracer
        protocolTracer = contramap HydraProtocol tracer
        applyMessage :: HeadProtocol tx -> m ()
        applyMessage ms = do
          decision <- atomically $ do
            s <- readTVar (hnState hn)
            let decision = handleMessage (hnId hn) s ms :: Decision m tx
            writeTVar (hnState hn) (decisionState decision)
            return (decision)
          traceWith protocolTracer (decisionTrace decision)
          void $ async (decisionJob decision >>= \case
                           Just ms' -> applyMessage ms'
                           Nothing -> return ())



-- | This is the actual logic of the proto
handleMessage :: (MonadTimer m, Tx tx) => NodeId -> HStateTransformer m tx

handleMessage nodeId s (RequestTxSignature tx peer) =
  Decision s TPNoOp $ do
    isValid <- runComp $ txValidate (hsUtXOConf s) tx -- TODO: unused
    case (Map.lookup peer) . hsChannels $ s of
      Just ch -> send ch $ ProvideTxSignature (txRef tx) nodeId
      Nothing -> error $ concat ["Error in ", show nodeId, ": Did not find peer ", show peer, " in ", show . Map.keys . hsChannels $ s]
    return Nothing

-- Getting acknowledgement for a transaction from a peer
handleMessage _nodeId s (ProvideTxSignature txid peer) =
  case txid `Map.lookup` hsTxs s of
    Nothing -> Decision
      s
      (TPInvalidTransition $ "Tried to add signature for " ++ show txid ++ " which is not known.")
      (return Nothing)
    Just (tx, AcknowledgedPartly peers) -> Decision
      (s {hsTxs = Map.insert txid (tx, AcknowledgedPartly (peer `Set.insert` peers)) (hsTxs s)})
      (TPTxAcknowledged txid peer)
      (return (Just $ CheckAcknowledgement txid))
    Just (_tx, AcknowledgedFully) ->
      Decision s (TPInvalidTransition $ "Tried to add signature for " ++ show txid ++ " which is already stable.")
               (return Nothing)

-- Check whether a tx has been confirmed by everyone
handleMessage _nodeId s (CheckAcknowledgement txid) =
  case txid `Map.lookup` hsTxs s of
    Nothing -> Decision
      s
      (TPInvalidTransition $ "Tried to check whether " ++ show txid ++ " is acknowledged, but couldn't find it.")
      (return Nothing)
    Just (tx, AcknowledgedPartly peers) ->
      if peers == hsPeers s
        then Decision
               (s {hsTxs = Map.insert txid (tx, AcknowledgedFully) (hsTxs s)})
               (TPTxStable txid)
               (broadcast s (ShowAcknowledgedTx tx) >> return Nothing)
        else Decision
               s
               TPNoOp
               (return Nothing)
    Just (_, AcknowledgedFully) -> Decision s TPNoOp (return Nothing)

-- Receiving a tx, with multi-sig from everyone
handleMessage _nodeId s (ShowAcknowledgedTx tx) =
  Decision
    (s {hsTxs = Map.insert (txRef tx) (tx, AcknowledgedFully) (hsTxs s)})
    (TPTxStable (txRef tx))
    (return Nothing)

-- | Send a message to all peers
broadcast :: (Monad m, Tx tx)
  => HState m tx
  -> HeadProtocol tx
  -> m ()
broadcast s ms =
  forM_ (Map.toList $ hsChannels s) $ \(_nodeId, ch) -> do
    -- TODO: I'd like to do a "traceWith tracer $ TraceMessageSent nodeId ms"
    -- here, but then I'd need to have that tracer passed to the decisionJob,
    -- which feels a bit weird.
    send ch ms

txSender :: (MonadAsync m, MonadSay m, Tx tx) =>
  Tracer m (TraceHydraEvent tx)
  -> HeadNode m tx -> m ()
txSender tracer hn = case (hnTxSendStrategy hn) of
  SendSingleTx tx ->  do
    atomically $ modifyTVar (hnState hn) $ \s -> s { hsTxs = Map.insert (txRef tx) (tx, AcknowledgedPartly Set.empty) (hsTxs s)}
    channelList <- Map.toList . hsChannels <$> atomically (readTVar (hnState hn))
    forM_ channelList $ \(nodeId, ch) -> do
      traceWith messageTracer $ TraceMessageSent nodeId $ RequestTxSignature tx (hnId hn)
      send ch $ RequestTxSignature tx (hnId hn)
    -- TODO: I'd like to replace that with the following, but then I'd also have
    -- to get tracing to broadcast. s <- atomically $ readTVar (hnState hn)
    -- broadcast s $ RequestTxSignature tx (hnId hn)
  SendNoTx -> return ()
  where messageTracer = contramap HydraMessage tracer
