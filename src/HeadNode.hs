{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HeadNode where

import Control.Monad (forever, forM_, void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- imports from io-sim, io-sim-classes, contra-tracer
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadTimer
import Control.Tracer

-- imports from this package
import Channel
import DelayedComp
import HeadNode.ProtocolFunctions
import HeadNode.Types
import MSig.Mock
import Tx.Class

data TxSendStrategy tx =
    SendNoTx
  | SendSingleTx tx
  deriving (Show, Eq)

-- Multi-sig functionality for a given node.
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
  hcLeaderFun :: SnapN -> NodeId
  }

data Tx tx => HeadNode m tx = HeadNode {
  hnConf :: NodeConf tx,
  hnState :: TMVar m (HState m tx),
  hnInbox :: TBQueue m (NodeId, HeadProtocol tx),
  hnPeerHandlers :: TVar m (Map NodeId (Async m ()))
  }

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
    putTMVar (hnState hn) $
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
    thisId = hcNodeId (hnConf hn)
    applyMessage :: NodeId -> HeadProtocol tx -> m ()
    applyMessage peer ms = do
      state <- atomically $ takeTMVar (hnState hn)
      case handleMessage (hnConf hn) peer state ms of
        DecApply stateUpdate trace ms' -> do
          -- 'runComp' advances the time by the amount the handler takes,
          -- and unwraps the result
          state' <- runComp stateUpdate
          atomically $ putTMVar (hnState hn) state'
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

-- | This is the actual logic of the protocol
handleMessage
  :: (MonadTimer m, Tx tx)
  => NodeConf tx
  -> NodeId
  -> HStateTransformer m tx

handleMessage conf _peer s (New tx)
  | isValid = DecApply
    (validComp >> pure s)
    (TPTxNew (txRef tx) (hcNodeId conf))
    (return (Multicast (SigReqTx tx)))
  | otherwise =
    DecInvalid
    (void validComp)
    ("Transaction " ++ show (txRef tx) ++ " is not valid in the confirmed UTxO")
  where
    validComp = txValidate (hsUTxOConf s) tx
    isValid = unComp validComp

handleMessage conf peer s (SigReqTx tx)
  | isValid = DecApply
    (do void validComp
        pure $ s {
          hsTxsSig = Map.insert (txRef tx) (txObj (hsTxsConf s) peer tx) (hsTxsSig s),
          hsUTxOSig = hsUTxOSig s `txApplyValid` tx
          })
    (TPTxSig (txRef tx) (hcNodeId conf))
    (do sigma <- (ms_sig_tx . hcMSig $ conf) (hsSK s) tx
        return $ SendTo peer (SigAckTx (txRef tx) sigma))
  | otherwise = DecWait (void validComp)
  where
    validComp = txValidate (hsUTxOConf s) tx
    isValid = unComp validComp

handleMessage conf peer s (SigAckTx txref sig)
  -- assert that all the requirements are fulfilled
  | Map.notMember txref (hsTxsSig s) =
    DecInvalid (return ()) $ "Transaction " ++ show txref ++ " not found."
  | txoIssuer txob /= (hcNodeId conf) =
    DecInvalid (return ()) $ "Transaction " ++ show txref ++ " not issued by " ++ show (hcNodeId conf)
  | sig `Set.member` txoS txob =
    DecInvalid (return ()) $ "Transition " ++ show txref ++ " already signed with " ++ show sig
  | otherwise = DecApply
    (pure $ s { hsTxsSig = Map.insert txref txob' (hsTxsSig s) })
    (TPTxAck txref peer)
    (if Set.size (txoS txob') < Set.size (hsVKs s)
     then pure SendNothing
     else do
        asig <- (ms_asig_tx . hcMSig $ conf) (txoTx txob') (hsVKs s) (txoS txob')
        return $ Multicast (SigConfTx txref asig)
        )
  where
    txob = (hsTxsSig s) Map.! txref
    txob' = txob { txoS = sig `Set.insert` txoS txob}

handleMessage conf _peer s (SigConfTx txref asig)
  | Map.notMember txref (hsTxsSig s) =
    DecInvalid (return ()) $ "Transaction " ++ show txref ++ " not found."
  | not isValid = DecInvalid (void validComp)
    ("Invalid aggregate signature " ++ show asig ++ ", " ++ show avk ++ " in SigConfTx")
  | otherwise = DecApply
    (do
        _ <- validComp -- this is only to wait, we already guarded against the
                       -- signature being invalid
        let txsSig = Map.insert txref txob' (hsTxsSig s)
        pure $ s {
          hsUTxOConf = hsUTxOConf s `txApplyValid` txoTx txob,
          hsTxsSig = txsSig,
          hsTxsConf = txsSig
          })
    (TPTxConf txref)
    (pure SendNothing)
  where
    avk = ms_avk $ hsVKs s
    txob = (hsTxsSig s) Map.! txref
    txob' = txob { txoSigma = Just asig }
    validComp = (ms_verify_tx . hcMSig $ conf) avk (txoTx txob) asig
    isValid = unComp validComp

handleMessage conf _peer s NewSn
  | (hcLeaderFun conf) snapN /= (hcNodeId conf) =
    DecInvalid (return ()) $ (show (hcNodeId conf)) ++ " Cannot create snaphot " ++ show snapN
  | otherwise = DecApply
    (pure s)
    (TPSnNew snapN (hcNodeId conf))
    (pure $ Multicast (SigReqSn snapN txSet))
  where
    snapN = nextSn (hsSnapNConf s)
    txSet = Map.keysSet $ maxTxos (hsTxsConf s)

handleMessage conf peer s (SigReqSn snapN txRefs)
  | snapN /= nextSn lastSn =
    DecInvalid (return ()) $ ("Did not expec snapshot " ++ show snapN ++ ", last was " ++ show lastSn)
  | (hcLeaderFun conf) snapN /= peer =
    DecInvalid (return ()) $ (show peer ++ " should not create snapshot " ++ show snapN)
  | hsSnapNConf s /= hsSnapNSig s = DecWait (return ())
  | not (txRefs `Set.isSubsetOf` Map.keysSet (hsTxsConf s)) = DecWait (return ())
  | otherwise = DecApply
    (return s')
    (TPSnSig snapN (hcNodeId conf))
    (do sig <- (ms_sig_sn . hcMSig $ conf) (hsSK s) (snapN, snoO (hsSnapSig s'))
        return $ SendTo peer (SigAckSn snapN sig))
  where
    lastSn = hsSnapNSig s
    s' = s { hsSnapNSig = snapN,
             hsSnapSig = snObj s snapN (snoO $ hsSnapConf s) txRefs
           }

handleMessage conf peer s (SigAckSn snapN sig)
  | snapN /= (hsSnapNSig s) = DecInvalid (return ())
    ("Got signature for snapshot " ++ show snapN
     ++ " when signed snapshot was " ++ show (hsSnapNSig s))
  | (hcLeaderFun conf) snapN /= hcNodeId conf = DecInvalid (return ())
    ("Received signature for snapshot " ++ show snapN
     ++ " which was not ours to create")
  | (sig `Set.member` snoS snob) = DecInvalid (return ())
    ("Received signature " ++ show sig ++ " for snapshot "
     ++ show snapN ++ ", which we already had.")
  | otherwise = DecApply
    (return $ s { hsSnapSig = snob' })
    (TPSnAck snapN peer)
    (if Set.size (snoS snob') < Set.size (hsVKs s)
     then pure SendNothing
     else do
        asig <- (ms_asig_sn . hcMSig $ conf) (snapN, snoO snob') (hsVKs s) (snoS snob')
        return $ Multicast (SigConfSn snapN asig))
  where
    snob = hsSnapSig s
    snob' = snob {snoS = sig `Set.insert` snoS snob}

handleMessage conf _peer s (SigConfSn snapN asig)
  | snapN /= (hsSnapNSig s) = DecInvalid (return ())
    ("Can't confirm snapshot " ++ show snapN
      ++ " when last signed snapshot is " ++ show (hsSnapNSig s))
  | (hsSnapNSig s) == (hsSnapNConf s) = DecInvalid (return ())
    ("Trying to confirm " ++ show snapN ++ " but it is leady confirmed.")
  | not isValid = DecInvalid (void validComp)
    ("Invalid aggregate signature " ++ show asig ++ ", " ++ show avk ++ " in SigConfSn")
  | otherwise = DecApply
    (do void validComp
        return s')
    (TPSnConf snapN)
    (pure SendNothing)
  where
    avk = ms_avk $ hsVKs s
    snob = hsSnapSig s
    snob' = snob { snoSigma = Just asig }
    validComp = (ms_verify_sn . hcMSig $ conf) avk (snapN, snoO snob) asig
    isValid = unComp validComp
    s' = s {
      hsSnapNConf = snapN,
      hsSnapSig = snob',
      hsSnapConf = snob,
      hsTxsConf = (hsTxsConf s) Map.\\ reach (hsTxsConf s) (snoT snob'),
      hsTxsSig = (hsTxsSig s) Map.\\ reach (hsTxsSig s) (snoT snob')
      }


txSender :: (MonadAsync m, Tx tx) =>
  Tracer m (TraceHydraEvent tx)
  -> HeadNode m tx -> m ()
txSender tracer hn = case (hcTxSendStrategy (hnConf hn)) of
  SendSingleTx tx -> clientMessage tracer hn (New tx)
  SendNoTx -> return ()
