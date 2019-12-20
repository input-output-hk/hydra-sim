{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HeadNode where

import Control.Monad (forever, forM_, void, when)
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
import HeadNode.Types
import Object

data TxSendStrategy tx =
    SendNoTx
  | SendSingleTx tx
  deriving (Show, Eq)

data (Object tx, Object sn) => HeadNode m tx sn = HeadNode {
  hnId :: NodeId,
  hnState :: TVar m (HState m tx sn),
  hnInbox :: TBQueue m (NodeId, HeadProtocol (TxOrSnapshot tx sn)),
  hnPeerHandlers :: TVar m (Map NodeId (Async m ())),
  hnTxSendStrategy :: TxSendStrategy tx
  }

newNode :: (MonadSTM m, Object tx, Object sn) =>
  NodeId
  -> OValidationContext tx
  -> OValidationContext sn
  -> TxSendStrategy tx
  -> m (HeadNode m tx sn)
newNode nodeId vcTx vcSn txSendStrategy
  = do
  state <- newTVarM (hnStateEmpty (TOSVC vcTx vcSn))
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
  :: (MonadSTM m, MonadSay m, MonadTimer m, MonadAsync m
     , Object tx, Object sn)
  => Tracer m (TraceHydraEvent (TxOrSnapshot tx sn))
  -> HeadNode m tx sn -> m ()
startNode tracer hn = void $ concurrently (listener tracer hn) (txSender tracer hn)

-- | Add a peer, and install a thread that will collect messages from the
-- channel to the main inbox of the node.
addPeer :: (MonadSTM m, MonadAsync m, MonadSay m, Object tx, Object sn) =>
  HeadNode m tx sn -> NodeId -> Channel m (HeadProtocol (TxOrSnapshot tx sn)) -> m ()
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
  :: forall m tx sn
     . (MonadSTM m, MonadSay m, MonadTimer m, MonadAsync m
       , Object tx, Object sn)
  => Tracer m (TraceHydraEvent (TxOrSnapshot tx sn))
  -> HeadNode m tx sn -> m ()
listener tracer hn = forever $ do
  atomically (readTBQueue $ hnInbox hn) >>= \(peer, ms) -> do
    traceWith messageTracer (TraceMessageReceived peer ms)
    applyMessage ms

  where messageTracer = contramap HydraMessage tracer
        protocolTracer = contramap HydraProtocol tracer
        applyMessage :: HeadProtocol (TxOrSnapshot tx sn) -> m ()
        applyMessage ms = do
          decision <- atomically $ do
            s <- readTVar (hnState hn)
            let decision = handleMessage (hnId hn) s ms :: Decision m tx sn
            writeTVar (hnState hn) (decisionState decision)
            return (decision)
          traceWith protocolTracer (decisionTrace decision)
          void $ async (decisionJob decision >>= \case
                           Just ms' -> applyMessage ms'
                           Nothing -> return ())



-- | This is the actual logic of the proto
handleMessage
  :: (Object tx, Object sn , MonadTimer m)
  => NodeId -> HStateTransformer m tx sn

handleMessage nodeId s (HPSigReq peer o) =
  Decision s TPNoOp $ do
    let (validity, mdelay) = oValidate (hsValidationContext s) o
    case mdelay of
      Just delay -> threadDelay delay
      Nothing -> return ()
    when (validity == ObjectValid) $ case (Map.lookup peer) . hsChannels $ s of
      Just ch -> send ch $ HPSigAck (oRef o) nodeId
      Nothing -> error $ concat ["Error in ", show nodeId, ": Did not find peer ", show peer, " in ", show . Map.keys . hsChannels $ s]
    return Nothing

-- Getting acknowledgement for a transaction from a peer
handleMessage _nodeId s (HPSigAck oref@(TOSRTx txid) peer) =
  case txid `Map.lookup` hsTxs s of
    Nothing -> Decision
      s
      (TPInvalidTransition $ "Tried to add signature for " ++ show txid ++ " which is not known.")
      (return Nothing)
    Just (tx, Acknowledged peers) -> Decision
      (s {hsTxs = Map.insert txid (tx, Acknowledged (peer `Set.insert` peers)) (hsTxs s)})
      (TPAck oref peer)
      (return (Just $ CheckAcknowledgement oref))
    Just (_tx, Confirmed) ->
      Decision s (TPInvalidTransition $ "Tried to add signature for " ++ show txid ++ " which is already stable.")
               (return Nothing)

-- Check whether a tx has been confirmed by everyone
handleMessage _nodeId s (CheckAcknowledgement oref@(TOSRTx txid)) =
  case txid `Map.lookup` hsTxs s of
    Nothing -> Decision
      s
      (TPInvalidTransition $ "Tried to check whether " ++ show txid ++ " is acknowledged, but couldn't find it.")
      (return Nothing)
    Just (tx, Acknowledged peers) ->
      if peers == hsPeers s
        then Decision
               (s {hsTxs = Map.insert txid (tx, Confirmed) (hsTxs s)})
               (TPConf oref)
               (broadcast s (HPSigConf (TOSTx tx)) >> return Nothing)
        else Decision
               s
               TPNoOp
               (return Nothing)
    Just (_, Confirmed) -> Decision s TPNoOp (return Nothing)

-- Receiving a tx, with multi-sig from everyone
handleMessage _nodeId s (HPSigConf (TOSTx tx)) =
  Decision
    (s {hsTxs = Map.insert (oRef tx) (tx, Confirmed) (hsTxs s)})
    (TPConf (TOSRTx $ oRef tx))
    (return Nothing)

-- TODO: implement those
handleMessage _nodeId _s (HPSigAck (TOSRSn _ ) _peer) = undefined
handleMessage _nodeId _s (HPSigConf (TOSSn _ )) = undefined
handleMessage _nodeId _s (CheckAcknowledgement (TOSRSn _ )) = undefined


-- | Send a message to all peers
broadcast
  :: (Object tx, Object sn, Monad m)
  => HState m tx sn
  -> HeadProtocol (TxOrSnapshot tx sn)
  -> m ()
broadcast s ms =
  forM_ (Map.toList $ hsChannels s) $ \(_nodeId, ch) -> do
    -- TODO: I'd like to do a "traceWith tracer $ TraceMessageSent nodeId ms"
    -- here, but then I'd need to have that tracer passed to the decisionJob,
    -- which feels a bit weird.
    send ch ms

txSender
  :: (Object tx, Object sn, MonadAsync m, MonadSay m)
  => Tracer m (TraceHydraEvent (TxOrSnapshot tx sn))
  -> HeadNode m tx sn -> m ()
txSender tracer hn = case (hnTxSendStrategy hn) of
  SendSingleTx tx ->  do
    atomically $ modifyTVar (hnState hn) $ \s -> s { hsTxs = Map.insert (oRef tx) (tx, Acknowledged Set.empty) (hsTxs s)}
    channelList <- Map.toList . hsChannels <$> atomically (readTVar (hnState hn))
    forM_ channelList $ \(nodeId, ch) -> do
      traceWith messageTracer $ TraceMessageSent nodeId $ HPSigReq (hnId hn) (TOSTx tx)
      send ch $ HPSigReq (hnId hn) (TOSTx tx)
    -- TODO: I'd like to replace that with the following, but then I'd also have
    -- to get tracing to broadcast. s <- atomically $ readTVar (hnState hn)
    -- broadcast s $ RequestTxSignature tx (hnId hn)
  SendNoTx -> return ()
  where messageTracer = contramap HydraMessage tracer
