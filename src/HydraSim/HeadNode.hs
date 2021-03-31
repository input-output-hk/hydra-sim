{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module HydraSim.HeadNode
  ( HeadNode,
    newNode,
    connectNodes,
    startNode,
    traceState
  ) where

import           Control.Monad (forever, void)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork (myThreadId, labelThread)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Control.Tracer
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           HydraSim.Channel
import           HydraSim.DelayedComp
import           HydraSim.HeadNode.SimpleProtocolHandler (handleMessage)
import           HydraSim.MSig.Mock
import           HydraSim.Multiplexer
import           HydraSim.Sized
import           HydraSim.Trace
import           HydraSim.Tx.Class
import           HydraSim.Types

-- | A node in the head protocol.
data Tx tx => HeadNode m tx = HeadNode {
  -- | Static configuration
  hnConf :: NodeConf tx,
  -- | Current local state
  hnState :: TMVar m (HState tx),
  -- | A 'Multiplexer' to handle communication with other nodes.
  hnMultiplexer :: Multiplexer m (HeadProtocol tx)
  }

-- | Set up a new node to participate in the head protocol.
newNode
  :: (MonadSTM m, Tx tx)
  => NodeConf tx
  -> (Size -> DiffTime)
  -- ^ Write capacity of the node's network device.
  --
  -- Determines how much time it takes to serialise and send a message of a
  -- given size.
  -> (Size -> DiffTime)
  -- ^ Read capacity of the node's network device.
  -> m (HeadNode m tx)
newNode conf writeCapacity readCapacity = do
  state <- newTMVarM $ hStateEmpty (hcNodeId conf)
  multiplexer <- newMultiplexer
                 (show . hcNodeId $ conf)
                 1000 1000
                 -- TODO: make buffer sizes configurable. The actual numbers
                 -- don't really matter, but we do not want to be bounded by
                 -- this.
                 writeCapacity readCapacity
  return $ HeadNode {
    hnConf = conf,
    hnState = state,
    hnMultiplexer = multiplexer
    }

-- | Connect two nodes.
connectNodes
  :: forall m tx . (MonadAsync m, MonadTimer m,
      Tx tx)
  => m (Channel m (MessageEdge (HeadProtocol tx)),
        Channel m (MessageEdge (HeadProtocol tx)))
  -> HeadNode m tx
  -> HeadNode m tx
  -> m ()
connectNodes createChannels node node' = do
  connect createChannels
    (hcNodeId (hnConf node), hnMultiplexer node)
    (hcNodeId (hnConf node'), hnMultiplexer node')
  addPeer node (hcNodeId $ hnConf node')
  addPeer node' (hcNodeId $ hnConf node)
  where
    addPeer :: HeadNode m tx -> NodeId -> m ()
    addPeer hn (NodeId i) = atomically $ do
      state <- takeTMVar (hnState hn)
      putTMVar (hnState hn) $!
        state { hsVKs = Set.insert (VKey i) $ hsVKs state }

-- | Start a node.
--
-- This starts the multiplexer, the event loop handling messages, and threads
-- for sending transactions and making snapshots, according to the strategies
-- specified in the node config.
startNode
  :: (MonadSTM m, MonadTimer m, MonadAsync m, MonadThrow m,
       Tx tx)
  => Tracer m (TraceHydraEvent tx)
  -> HeadNode m tx -> m ()
startNode tracer hn = void $
  concurrently (labelThisThread nodeLabel >> listener tracer hn) $
  concurrently (startMultiplexer mpTracer (hnMultiplexer hn)) $
  concurrently (labelThisThread nodeLabel >> txSender tracer hn)
               (labelThisThread nodeLabel >> snDaemon tracer hn)
  where
    mpTracer = contramap HydraMessage tracer
    nodeLabel = show . hcNodeId . hnConf $ hn

labelThisThread :: MonadAsync m => String -> m ()
labelThisThread nodeLabel = do
      myId <- myThreadId
      labelThread myId nodeLabel

-- | write the current state to the trace
traceState
  :: (MonadSTM m, Tx tx)
  => Tracer m (TraceHydraEvent tx) -> HeadNode m tx -> m ()
traceState tracer hn = do
  s <- atomically $ readTMVar (hnState hn)
  traceWith tracer $ HydraState s

-- | Add a message from the client (as opposed to from a node) to the message queue.
--
-- This is used for triggering events like transaction submission or snapshot
-- creation.
clientMessage
  :: (MonadSTM m, Tx tx)
  => Tracer m (TraceHydraEvent tx)
  -> HeadNode m tx
  -> HeadProtocol tx
  -> m ()
clientMessage tracer hn = sendToSelf mpTracer (hnMultiplexer hn) (hcNodeId (hnConf hn))
  where
    mpTracer = contramap HydraMessage tracer

-- | This is for the actual logic of the node, processing incoming messages.
listener
  :: forall m tx .
     (MonadSTM m, MonadTimer m, MonadAsync m,
      Tx tx)
  => Tracer m (TraceHydraEvent tx)
  -> HeadNode m tx -> m ()
listener tracer hn = forever $
  atomically (getMessage mplex) >>= uncurry applyMessage

  where
    mplex = hnMultiplexer hn
    mpTracer = contramap HydraMessage tracer
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
          -- We refine the protocol specification, in spawning a new thread for
          -- computing and sending follow-up messages. Since we not acdo cess
          -- the local state there, this is safe to do, and an obvious
          -- performance optimisation.
          void . async $ do
            labelThisThread $ show thisId
            runComp ms' >>= sendMessage
        DecWait comp -> do
          runComp comp
          atomically $ putTMVar (hnState hn) state
          reenqueue mpTracer mplex (peer, ms)
        DecInvalid comp errmsg -> do
          runComp comp
          traceWith protocolTracer (TPInvalidTransition errmsg)
          atomically $ putTMVar (hnState hn) state
    sendMessage :: SendMessage tx -> m ()
    sendMessage SendNothing = return ()
    sendMessage (SendTo peer ms)
      -- messages to the same node are just added to the inbox directly, without
      -- going over the network
      | peer == thisId = sendToSelf mpTracer mplex thisId ms
      | otherwise = sendTo mplex peer ms
    sendMessage (Multicast ms) =
      multicast mpTracer mplex thisId ms

txSender
  :: (MonadAsync m, Tx tx)
  => Tracer m (TraceHydraEvent tx)
  -> HeadNode m tx -> m ()
txSender tracer hn = case hcTxSendStrategy (hnConf hn) of
  SendNoTx -> return ()
  SendSingleTx tx -> clientMessage tracer hn (New tx)
  SendTxsDumb txs -> mapM_ (clientMessage tracer hn . New) txs
  SendTxs limit txs ->
    let go [] = return ()
        go (tx:rest) = do
          atomically $ do
            s <- takeTMVar (hnState hn)
            if Set.size (hsTxsInflight s) < limit
            then putTMVar (hnState hn) s { hsTxsInflight = txRef tx `Set.insert` hsTxsInflight s }
            else do
              putTMVar (hnState hn) s
              retry
          clientMessage tracer hn (New tx)
          go rest
    in go txs

snDaemon
  :: forall m tx .
     (MonadSTM m, MonadAsync m, Tx tx)
  => Tracer m (TraceHydraEvent tx)
  -> HeadNode m tx -> m ()
snDaemon tracer hn = case hcSnapshotStrategy conf of
  NoSnapshots -> return ()
  SnapAfter n ->
    let
      waitForOurTurn :: SnapN -> STM m SnapN
      waitForOurTurn lastSn = do
        s <- readTMVar (hnState hn)
        let snapN = hsSnapNConf s
        if Map.size (hsTxsConf s) >= n
           && hcLeaderFun conf (nextSn snapN) == hcNodeId conf
           -- to prevent filling our inbox with duplicate NewSn messages:
           && snapN >= lastSn
          then return $ nextSn snapN
          else retry
      doSnapshot :: SnapN -> m ()
      doSnapshot lastSn = do
        lastSn' <- atomically (waitForOurTurn lastSn)
        clientMessage tracer hn NewSn
        doSnapshot lastSn'
    in doSnapshot noSnapN
  where conf = hnConf hn
