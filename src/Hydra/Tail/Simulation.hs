{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Hydra.Tail.Simulation where

import Prelude

import Control.Concurrent.Class.MonadSTM (
  MonadSTM,
  TMVar,
  TVar,
  atomically,
  modifyTVar',
  newTMVarIO,
  newTVarIO,
  putTMVar,
  readTVar,
  retry,
  takeTMVar,
  tryTakeTMVar,
  writeTVar,
 )
import Control.Exception (
  Exception,
 )
import Control.Monad (
  forM,
  forM_,
  forever,
  guard,
  join,
  void,
 )
import Control.Monad.Class.MonadAsync (
  MonadAsync,
  async,
  concurrently_,
  forConcurrently_,
 )
import Control.Monad.Class.MonadThrow (
  MonadThrow,
  throwIO,
 )
import Control.Monad.Class.MonadTime (
  MonadTime,
 )
import Control.Monad.Class.MonadTimer (
  MonadTimer,
  threadDelay,
 )
import Control.Monad.IOSim (
  IOSim,
  SimTrace,
  Trace (..),
  runSimTrace,
 )
import qualified Control.Monad.IOSim as IOSim
import Control.Monad.Trans.Class (
  lift,
 )
import Control.Monad.Trans.State.Strict (
  StateT,
  evalStateT,
  execStateT,
  state,
 )
import Control.Tracer (
  Tracer (..),
  contramap,
  traceWith, emit,
 )
import Data.Generics.Internal.VL.Lens (
  (^.),
 )
import Data.Generics.Labels ()
import Data.List (
  delete,
  foldl',
  nub,
 )
import Data.Map (
  (!),
 )
import Data.Map.Strict (
  Map,
 )
import qualified Data.Map.Strict as Map
import Data.Maybe (
  fromMaybe,
  mapMaybe,
 )
import Data.Ratio (
  (%),
 )
import Data.Time.Clock (
  DiffTime,
  secondsToDiffTime,
 )
import GHC.Generics (
  Generic,
 )
import Hydra.Tail.Simulation.MockTx (
  MockTx (..),
  mockTx,
  received,
  sent,
 )
import Hydra.Tail.Simulation.Options (
  ClientOptions (..),
  NetworkCapacity (..),
  PrepareOptions (..),
  RunOptions (..),
  ServerOptions (..),
  kbitsPerSecond,
 )
import Hydra.Tail.Simulation.PaymentWindow (
  Ada (..),
  Balance (..),
  Lovelace (..),
  PaymentWindowStatus (..),
  ada,
  initialBalance,
  lovelace,
  modifyCurrent,
  viewPaymentWindow,
 )
import Hydra.Tail.Simulation.SlotNo (
  SlotNo (..),
 )
import Hydra.Tail.Simulation.Utils (
  frequency,
  modifyM,
  updateF,
  withLabel,
  withTMVar,
  withTMVar_,
 )
import HydraSim.Examples.Channels (
  AWSCenters (..),
  channel,
 )
import HydraSim.Multiplexer (
  Multiplexer,
  getMessage,
  newMultiplexer,
  sendTo,
  startMultiplexer,
 )
import qualified HydraSim.Multiplexer as Multiplexer
import HydraSim.Multiplexer.Trace (
  TraceMultiplexer (..),
 )
import HydraSim.Sized (
  Size (..),
  Sized (..),
 )
import HydraSim.Tx.Class (
  Tx (..),
 )
import HydraSim.Types (
  NodeId (..),
 )
import System.Random (
  StdGen,
  newStdGen,
  randomR,
 )

--
-- Simulation
--

prepareSimulation :: PrepareOptions -> IO [Event]
prepareSimulation options@PrepareOptions{numberOfClients, duration} = do
  let clientIds = [1 .. fromInteger numberOfClients]
  let events = fmap join $
        forM [0 .. pred duration] $ \currentSlot -> do
          join <$> traverse (stepClient options currentSlot) clientIds
  newStdGen >>= evalStateT events

runSimulation :: RunOptions -> [Event] -> SimTrace ()
runSimulation opts@RunOptions{serverOptions} events = runSimTrace $ do
  let (serverId, clientIds) = (0, [1 .. greatestKnownClient events])
  server <- newServer serverId clientIds serverOptions
  clients <- forM clientIds $ \clientId -> do
    client <- newClient clientId
    (clientId, client) <$ connectClient client server
  void $
    async $
      concurrently_
        (runServer trServer opts server)
        ( concurrently_
            (runEventLoop trEventLoop opts serverId (Map.fromList clients) (trim events))
            (forConcurrently_ (snd <$> clients) (\c -> runClient (trClient c) serverId opts c))
        )
  -- XXX(SN): This does not take into account that there might still be clients
  -- running and this likely leads to a differing number of confirmed
  -- transactions for the same events dataset with different parameters (e.g.
  -- payment window or pro-active snapshot limit)
  threadDelay (durationOf opts events)
 where
  -- We remove any transaction that is above the payment window, for they are
  -- actually unprocessable by the server (theoritically, transactions can be as
  -- large as 2*W, but even after a snapshot, accepting a transaction larger than W
  -- would require the client to first spend the difference. For simplicity, we
  -- consider that the server would reject any transaction larger than `W`, so we
  -- filter them out of the simulation if any.
  trim = filter $ \case
    Event _ _ (NewTx tx) ->
      let double = fromIntegral . unLovelace
          asLovelace = Lovelace . round
       in maybe
            True
            (\w -> sent tx <= asLovelace (double w * (opts ^. #paymentCutOff)))
            (opts ^. #paymentWindow)
    _ -> True

  tracer :: Tracer (IOSim s) TraceTailSimulation
  tracer = Tracer $ emit IOSim.traceM

  trClient :: Client m -> Tracer (IOSim a) TraceClient
  trClient Client{identifier} = contramap (TraceClient identifier) tracer

  trServer :: Tracer (IOSim a) TraceServer
  trServer = contramap TraceServer tracer

  trEventLoop :: Tracer (IOSim a) TraceEventLoop
  trEventLoop = contramap TraceEventLoop tracer

--
-- (Simplified) Tail-Protocol
--

-- | Messages considered as part of the simplified Tail pre-protocol. We don't know exactly
-- what the Tail protocol hence we have a highly simplified view of it and reduce it to a
-- mere message broker between many producers and many consumers (the clients), linked together
-- via a single message broker (the server).
data Msg
  = --
    -- ↓↓↓ Client messages ↓↓↓
    --

    -- | A new transaction, sent to some peer. The current behavior of this simulation
    -- consider that each client is only sending to one single peer. Later, we probably
    -- want to challenge this assumption by analyzing real transaction patterns from the
    -- main chain and model this behavior.
    NewTx !MockTx
  | -- | Notify the server that a snapshot was performed.
    SnapshotDone !Wallet
  | --
    -- ↓↓↓ Server messages ↓↓↓
    --

    -- | The server will notify concerned clients with transactions they have subscribed to.
    -- How clients subscribe and how the server is keeping track of the subscription is currently
    -- out of scope and will be explored at a later stage.
    NotifyTx !MockTx
  | -- | The server requests a client to perform a snapshot.
    NeedSnapshot !Wallet
  | -- | The server replies to each client submitting a transaction with an acknowledgement.
    AckTx !MockTx
  deriving (Generic, Show)

instance Sized Msg where
  size = \case
    NewTx tx ->
      sizeOfHeader + size tx + sizeOfAddress * fromIntegral (length $ txRecipients tx)
    NeedSnapshot{} ->
      0
    SnapshotDone{} ->
      sizeOfHeader
    NotifyTx tx ->
      sizeOfHeader + size tx
    AckTx tx ->
      sizeOfHeader + size (txId tx)
   where
    sizeOfAddress = 57
    sizeOfHeader = 2

data TraceTailSimulation
  = TraceServer TraceServer
  | TraceClient ClientId TraceClient
  | TraceEventLoop TraceEventLoop
  deriving (Show)

--
-- Server
--

type ServerId = NodeId

data Server m = Server
  { multiplexer :: Multiplexer m Msg
  , identifier :: ServerId
  , region :: AWSCenters
  , registry :: TMVar m (Map ClientId (ClientState, Map Wallet Balance, [Msg]))
  , transactions :: TVar m (Map (TxRef MockTx) (MockTx, ClientId, [ClientId]))
  }
  deriving (Generic)

newServer ::
  MonadSTM m =>
  ServerId ->
  [ClientId] ->
  ServerOptions ->
  m (Server m)
newServer identifier clientIds ServerOptions{region, writeCapacity, readCapacity} = do
  multiplexer <-
    newMultiplexer
      "server"
      outboundBufferSize
      inboundBufferSize
      (capacity writeCapacity)
      (capacity readCapacity)
  registry <- newTMVarIO clients
  transactions <- newTVarIO mempty
  return Server{multiplexer, identifier, region, registry, transactions}
 where
  outboundBufferSize = 1000000
  inboundBufferSize = 1000000
  -- NOTE: We care little about how much each client balance is in practice. Although
  -- the 'Balance' is modelled as a product (initial, current) because of the intuitive
  -- view it offers, we are really only interested in the delta. Balances can therefore
  -- be _negative_ as part of the simulation.
  clients =
    Map.fromList
      [ (clientId, (Online, emptyWallets, mempty))
      | clientId <- clientIds
      ]

runServer ::
  forall m.
  (MonadAsync m, MonadTimer m, MonadThrow m) =>
  Tracer m TraceServer ->
  RunOptions ->
  Server m ->
  m ()
runServer tracer options Server{multiplexer, registry, transactions} = do
  concurrently_
    (startMultiplexer (contramap TraceServerMultiplexer tracer) multiplexer)
    (withLabel "Main: Server" serverMain)
 where
  serverMain =
    forever $
      atomically (getMessage multiplexer) >>= handleMessage

  handleMessage = \case
    (clientId, NewTx tx) -> do
      -- Some of the recipients or the sender may be out of their payment window
      -- (i.e. 'DoingSnapshot'), if that's the case, we cannot process the transaction
      -- until they are done.
      clientStatuses <- fmap Map.toList $
        withTMVar registry $ \clients ->
          (,clients)
            <$> Map.traverseMaybeWithKey
              (evaluateClient (options ^. #enableBackupWallet) (options ^. #paymentWindow) (clientId, tx))
              clients

      withTMVar_ registry $
        execStateT $
          forM_ clientStatuses $ \case
            (clientId, ShouldRequestSnapshot _ (Just w)) -> do
              lift $ sendTo multiplexer clientId $ NeedSnapshot w
              modifyM $
                updateF clientId $ \(st, wallets, pending) -> do
                  pure $ Just (doingSnapshot w st, wallets, pending)
            _ ->
              pure ()

      case mapMaybe matchBlocked clientStatuses of
        [] -> do
          atomically $ modifyTVar' transactions $ Map.insert (txRef tx) (tx, clientId, txRecipients tx)
          forM_ (txRecipients tx) $ \recipient -> do
            sendTo multiplexer recipient (NotifyTx tx)
        (clientId : _) -> do
          -- NOTE: This enqueues the message for one (and only one) of the participants.
          -- who's currently blocked (there can be more than one).
          --
          -- A transaction shall be marked as pending if one of the
          -- participants (sender or recipients) is blocked (doing snapshot).
          --
          -- There's no "ledger" on the server, so if we re-enqueue a transaction
          -- for each blocked participant, we may replay the transaction more than once!
          --
          -- Thus, it is sufficient (and necessary) to re-enqueue the transaction only
          -- once, for one of the participant. Once that participant is done
          -- snapshotting, the transaction will eventually be retried and from here we
          -- really have two scenarios:
          --
          -- a) That client happened to be the last one in the transaction which had
          --    to snapshot, and the transaction can now proceed.
          --
          -- b) There's another client which is still blocked. Then, the transaction
          --    will be re-enqueued again, for that client (or at least, for the first
          --    other block client).
          --
          -- Eventually, once all clients are done, it goes through.
          traceWith tracer (TraceTransactionBlocked (txId tx))
          withTMVar_ registry $
            execStateT $
              modifyM $
                updateF clientId $ \(st, wallets, pending) -> do
                  pure $ Just (st, wallets, NewTx tx : pending)
    (clientId, AckTx ack) -> do
      let ackTx = \case
            Nothing -> (Nothing, Nothing)
            Just (tx, sender, clients) ->
              let clients' = delete clientId clients
               in if null clients' then (Just (tx, sender), Nothing) else (Nothing, Just (tx, sender, clients'))

      processed <- atomically $ do
        (a, s) <- Map.alterF ackTx (txRef ack) <$> readTVar transactions
        a <$ writeTVar transactions s

      case processed of
        Nothing -> pure ()
        Just (tx, sender) -> do
          withTMVar_ registry $
            execStateT $ do
              forM_ (txRecipients tx) $ \recipient -> do
                modifyM $
                  updateF recipient $ \(st, wallets, pending) -> do
                    let wallets' = modifyBalance st (+ received tx) wallets
                    pure $ Just (st, wallets', pending)
              modifyM $
                updateF sender $ \(st, wallets, pending) -> do
                  let wallets' = modifyBalance st (\x -> x - sent tx) wallets

                  -- Check whether we should send 'NeedSnapshot' for pro-active
                  -- snapshotting (after the tx). Logically this would be done on the
                  -- client-side but we have the payment window 'registry' only on the
                  -- server for now.
                  st' <- do
                    sendTo multiplexer sender (AckTx tx)
                    case anyInProactiveSnapshotLimit options wallets' of
                      Nothing ->
                        pure st
                      Just w -> do
                        sendTo multiplexer clientId $ NeedSnapshot w
                        pure $ doingSnapshot w st

                  pure $ Just (st', wallets', pending)
    (clientId, SnapshotDone w) -> do
      pending <- withTMVar registry $ \clients -> do
        case Map.lookup clientId clients of
          Nothing -> pure ([], clients)
          Just (_st, wallets, pending) -> do
            let wallets' = Map.adjust (\Balance{current} -> initialBalance current) w wallets
            let clients' = Map.insert clientId (Online, wallets', []) clients
            pure (pending, clients')
      mapM_ handleMessage (reverse $ (clientId,) <$> pending)
    (clientId, msg) ->
      throwIO (UnexpectedServerMsg clientId msg)

data ShouldRequestSnapshot = ShouldRequestSnapshot
  { isBlocked :: Bool
  , wallet :: Maybe Wallet
  }

matchBlocked :: (ClientId, ShouldRequestSnapshot) -> Maybe ClientId
matchBlocked (clientId, ShouldRequestSnapshot{isBlocked}) =
  clientId <$ guard isBlocked

-- | Return 'f (Just ClientId)' iif a client would exceed (bottom or top) its payment window
-- from the requested payment, or if it's already performing a snapshot.
--
-- NOTE: There's a slight _abuse_ here. Transactions are indeed written from the PoV
-- of the _sender_. So the amount corresponds to how much did the sender "lost" in the
-- transaction, but, there can be multiple recipients! Irrespective of this, we consider
-- in the simulation that *each* recipient receives the full amount.
evaluateClient ::
  Applicative f =>
  Bool ->
  Maybe Lovelace ->
  (ClientId, MockTx) ->
  ClientId ->
  (ClientState, Map Wallet Balance, pending) ->
  f (Maybe ShouldRequestSnapshot)
evaluateClient _ Nothing _ _ _ =
  pure Nothing
evaluateClient useSndWallet (Just paymentWindow) (sender, tx) clientId (st, wallets, _)
  | useSndWallet =
      case (mAmount, st) of
        (Just{}, DoingSnapshot ws)
          | all (`elem` ws) [FstWallet, SndWallet] ->
              pure $ Just $ ShouldRequestSnapshot True Nothing
        (Just amount, DoingSnapshot [FstWallet]) ->
          case viewPaymentWindow paymentWindow (wallets ! SndWallet) amount of
            OutOfPaymentWindow ->
              pure $ Just $ ShouldRequestSnapshot True (Just SndWallet)
            InPaymentWindow ->
              pure Nothing
        (Just amount, DoingSnapshot [SndWallet]) ->
          case viewPaymentWindow paymentWindow (wallets ! FstWallet) amount of
            OutOfPaymentWindow ->
              pure $ Just $ ShouldRequestSnapshot True (Just FstWallet)
            InPaymentWindow ->
              pure Nothing
        (Just amount, _) ->
          case viewPaymentWindow paymentWindow (wallets ! FstWallet) amount of
            OutOfPaymentWindow ->
              case viewPaymentWindow paymentWindow (wallets ! SndWallet) amount of
                OutOfPaymentWindow ->
                  pure $ Just $ ShouldRequestSnapshot True (Just FstWallet)
                InPaymentWindow ->
                  pure $ Just $ ShouldRequestSnapshot False (Just FstWallet)
            InPaymentWindow ->
              pure Nothing
        (Nothing, _) ->
          pure Nothing
  | otherwise =
      case (mAmount, st) of
        (Just{}, DoingSnapshot{}) ->
          pure $ Just $ ShouldRequestSnapshot True Nothing
        (Just amount, Online) ->
          case viewPaymentWindow paymentWindow (wallets ! FstWallet) amount of
            OutOfPaymentWindow ->
              pure $ Just $ ShouldRequestSnapshot True (Just FstWallet)
            InPaymentWindow ->
              pure Nothing
        (Nothing, _) ->
          pure Nothing
 where
  mAmount
    | clientId `elem` txRecipients tx =
        Just $ negate (sent tx)
    | clientId == sender =
        Just $ received tx
    | otherwise =
        Nothing

anyInProactiveSnapshotLimit :: RunOptions -> Map Wallet Balance -> Maybe Wallet
anyInProactiveSnapshotLimit opts wallets =
  case Map.toList (Map.filter (inProactiveSnapshotLimit opts) wallets) of
    [] -> Nothing
    (w, _) : _ -> Just w

inProactiveSnapshotLimit :: RunOptions -> Balance -> Bool
inProactiveSnapshotLimit RunOptions{paymentWindow, proactiveSnapshot} Balance{initial, current} =
  case (paymentWindow, proactiveSnapshot) of
    (Just w, Just frac) -> absBalance > limit w frac
    _ -> False
 where
  absBalance = abs $ current - initial

  limit w frac = fromDouble (toDouble w * frac)

  toDouble :: Lovelace -> Double
  toDouble = fromInteger . unLovelace

  fromDouble :: Double -> Lovelace
  fromDouble = Lovelace . truncate

data TraceServer
  = TraceServerMultiplexer (TraceMultiplexer Msg)
  | TraceTransactionBlocked (TxRef MockTx)
  deriving (Show)

data ServerMain = ServerMain deriving (Show)
instance Exception ServerMain

data UnexpectedServerMsg = UnexpectedServerMsg NodeId Msg
  deriving (Show)
instance Exception UnexpectedServerMsg

newtype UnknownClient = UnknownClient NodeId
  deriving (Show)
instance Exception UnknownClient

--
-- Client
--

type ClientId = NodeId

data ClientState = Online | DoingSnapshot [Wallet]
  deriving (Generic, Show, Eq)

doingSnapshot :: Wallet -> ClientState -> ClientState
doingSnapshot w = \case
  Online -> DoingSnapshot [w]
  DoingSnapshot ws -> DoingSnapshot (nub $ w : ws)

data Wallet = FstWallet | SndWallet
  deriving (Generic, Show, Eq, Ord, Enum, Bounded)

emptyWallets :: Map Wallet Balance
emptyWallets =
  Map.fromList
    [ (FstWallet, initialBalance 0)
    , (SndWallet, initialBalance 0)
    ]

modifyBalance ::
  ClientState ->
  (Lovelace -> Lovelace) ->
  Map Wallet Balance ->
  Map Wallet Balance
modifyBalance st fn =
  case st of
    Online ->
      Map.adjust (modifyCurrent fn) FstWallet
    DoingSnapshot ws
      | FstWallet `elem` ws ->
          Map.adjust (modifyCurrent fn) SndWallet
    _ ->
      Map.adjust (modifyCurrent fn) FstWallet

data Client m = Client
  { multiplexer :: Multiplexer m Msg
  , identifier :: ClientId
  , region :: AWSCenters
  , ackLock :: TMVar m ()
  , snapshotLocks :: Map Wallet (TMVar m ())
  }
  deriving (Generic)

newClient :: MonadSTM m => ClientId -> m (Client m)
newClient identifier = do
  snapshotLocks <-
    Map.fromList
      <$> sequence
        [ (FstWallet,) <$> newTMVarIO ()
        , (SndWallet,) <$> newTMVarIO ()
        ]
  ackLock <- newTMVarIO ()
  multiplexer <-
    newMultiplexer
      ("client-" <> show (getNodeId identifier))
      outboundBufferSize
      inboundBufferSize
      (capacity $ kbitsPerSecond 512)
      (capacity $ kbitsPerSecond 512)
  return Client{multiplexer, identifier, region, ackLock, snapshotLocks}
 where
  outboundBufferSize = 1000
  inboundBufferSize = 1000
  region = LondonAWS

-- | Run a client given a list of events. The client does work through all of
-- these events and react on messages from the server at the same time.
--
-- NOTE: Although events are scheduled for a certain 'SlotNo', there is no
-- global notion of time and clients to increment their own 'currentSlot', which
-- might be delayed due to the blocking nature of snapshotting.
runClient ::
  forall m.
  (MonadAsync m, MonadTimer m, MonadThrow m) =>
  Tracer m TraceClient ->
  ServerId ->
  RunOptions ->
  Client m ->
  m ()
runClient tracer serverId opts Client{multiplexer, identifier, ackLock, snapshotLocks} = do
  concurrently_
    (startMultiplexer (contramap TraceClientMultiplexer tracer) multiplexer)
    (withLabel ("Client: " <> show identifier) $ forever clientMain)
 where
  clientMain :: m ()
  clientMain =
    atomically (getMessage multiplexer) >>= \case
      (_, AckTx{}) ->
        -- NOTE(SN): For pro-active snapshot handling, we would ideally keep
        -- track for the client's payment window here and decide whether or not
        -- to snapshot. For simplicity reasons, this is also shifted to the
        -- server (for now) as we do track payment windows there.
        atomically $ putTMVar ackLock ()
      (_, NotifyTx tx) ->
        sendTo multiplexer serverId (AckTx tx)
      (_, NeedSnapshot w) -> do
        void $
          async $
            withLabel ("client " <> show identifier <> " snapshotting " <> show w) $
              withTMVar_ (snapshotLocks ! w) $ \() -> do
                threadDelay settlementDelay_
                sendTo multiplexer serverId (SnapshotDone w)
      (nodeId, msg) ->
        throwIO $ UnexpectedClientMsg nodeId msg
   where
    settlementDelay_ =
      secondsToDiffTime (unSlotNo (opts ^. #settlementDelay)) * (opts ^. #slotLength)

data UnexpectedClientMsg = UnexpectedClientMsg NodeId Msg
  deriving (Show)
instance Exception UnexpectedClientMsg

stepClient ::
  forall m.
  (Monad m) =>
  PrepareOptions ->
  SlotNo ->
  ClientId ->
  StateT StdGen m [Event]
stepClient options currentSlot identifier = do
  pSubmit <- state (randomR (1, 100))
  let submit = pSubmit % 100 <= submitLikelihood
  recipient <- pickRecipient identifier (options ^. #numberOfClients)

  -- NOTE: The distribution is extrapolated from real mainchain data.
  amount <-
    fmap (lovelace . (`Ada` 0)) $
      state $
        frequency
          [ (122, randomR (1, 10))
          , (144, randomR (10, 100))
          , (143, randomR (100, 1000))
          , (92, randomR (1000, 10000))
          , (41, randomR (10000, 100000))
          , (12, randomR (100000, 1000000))
          ]

  -- NOTE: The distribution is extrapolated from real mainchain data.
  txSize <-
    fmap Size $
      state $
        frequency
          [ (318, randomR (192, 512))
          , (129, randomR (512, 1024))
          , (37, randomR (1024, 2048))
          , (12, randomR (2048, 4096))
          , (43, randomR (4096, 8192))
          , (17, randomR (8192, 16384))
          ]

  pure
    [ Event currentSlot identifier msg
    | (predicate, msg) <-
        [
          ( submit
          , NewTx (mockTx identifier currentSlot amount txSize [recipient])
          )
        ]
    , predicate
    ]
 where
  ClientOptions{submitLikelihood} = options ^. #clientOptions

data TraceClient
  = TraceClientMultiplexer (TraceMultiplexer Msg)
  deriving (Show)

--
-- Events
--

-- In this simulation, we have decoupled the generation of events from their
-- processing. 'Event's are used as an interface, serialized to CSV. This way,
-- the simulation can be fed with data coming from various places.
data Event = Event
  { slot :: !SlotNo
  , from :: !ClientId
  , msg :: !Msg
  }
  deriving (Generic, Show)

data TraceEventLoop
  = TraceEventLoopTick SlotNo
  | TraceEventLoopTxScheduled (TxRef MockTx)
  deriving (Show)

data SimulationSummary = SimulationSummary
  { numberOfClients :: !Integer
  , numberOfTransactions :: !Integer
  , averagePayment :: !Lovelace
  , highestPayment :: !Lovelace
  , totalVolume :: !Ada
  , lastSlot :: !SlotNo
  }
  deriving (Generic, Show)

runEventLoop ::
  forall m.
  (MonadAsync m, MonadTimer m) =>
  Tracer m TraceEventLoop ->
  RunOptions ->
  ServerId ->
  Map ClientId (Client m) ->
  [Event] ->
  m ()
runEventLoop tracer opts serverId clients =
  withLabel "EventLoop" . loop 0
 where
  loop !currentSlot = \case
    [] ->
      pure ()
    (e : q) | (slot :: Event -> SlotNo) e > currentSlot -> do
      traceWith tracer (TraceEventLoopTick currentSlot)
      threadDelay (opts ^. #slotLength)
      loop (currentSlot + 1) (e : q)
    (e : q) -> do
      traceWith tracer (TraceEventLoopTick currentSlot)
      void $
        async $
          withLabel ("async-" <> show (slot e) <> "-" <> show (from e)) $ do
            let Client{multiplexer, ackLock, snapshotLocks} = clients ! from e
            case msg e of
              NewTx tx -> traceWith tracer (TraceEventLoopTxScheduled (txId tx))
              _ -> pure ()
            atomically $ takeTMVar ackLock

            -- Ensure we can only send message to the server if we aren't doing a snapshot.
            w <- atomically $ do
              mLockFst <- tryTakeTMVar (snapshotLocks ! FstWallet)
              mLockSnd <-
                if opts ^. #enableBackupWallet
                  then tryTakeTMVar (snapshotLocks ! SndWallet)
                  else pure Nothing
              case (mLockFst, mLockSnd) of
                (Nothing, Nothing) ->
                  retry
                (Nothing, Just ()) ->
                  pure SndWallet
                (Just (), Nothing) ->
                  pure FstWallet
                (Just (), Just ()) ->
                  FstWallet <$ putTMVar (snapshotLocks ! SndWallet) ()
            sendTo multiplexer serverId (msg e)
            atomically $ putTMVar (snapshotLocks ! w) ()

            -- If no transaction was sent, we can put back the ack lock and process the next event.
            -- Otherwise, the lock is replaced by the 'clientMain' when processing a 'AckTx'
            -- message from the server.
            case msg e of
              NewTx{} ->
                pure ()
              _ ->
                atomically $ putTMVar ackLock ()

      loop currentSlot q

summarizeEvents :: RunOptions -> [Event] -> SimulationSummary
summarizeEvents RunOptions{paymentWindow} events =
  SimulationSummary
    { numberOfClients
    , numberOfTransactions
    , averagePayment
    , highestPayment
    , totalVolume
    , lastSlot
    }
 where
  numberOfClients = toInteger $ fromEnum $ greatestKnownClient events
  (ada -> totalVolume, highestPayment, numberOfTransactions) =
    foldl' count (0, 0, 0) events
   where
    w = fromMaybe (Lovelace $ round @Double @_ 1e99) paymentWindow
    count (!volume, !hi, st) = \case
      (Event _ _ (NewTx MockTx{txAmount}))
        | txAmount <= w ->
            (volume + txAmount, max txAmount hi, st + 1)
      _ ->
        (volume, hi, st)
  averagePayment =
    Lovelace $ unLovelace (lovelace totalVolume) `div` numberOfTransactions
  lastSlot = last events ^. #slot

-- | Calculate simulation time as the last event + twice the settlement delay.
durationOf :: RunOptions -> [Event] -> DiffTime
durationOf RunOptions{slotLength} events =
  slotLength * fromIntegral (unSlotNo (last events ^. #slot) + 1)

--
-- Helpers
--

greatestKnownClient :: [Event] -> ClientId
greatestKnownClient events =
  max
    ( maximum
        (from <$> events)
    )
    ( maximum
        ( mapMaybe
            ( \case
                Event{msg = NewTx MockTx{txRecipients}} -> Just (maximum txRecipients)
                _ -> Nothing
            )
            events
        )
    )

getRegion ::
  [AWSCenters] ->
  NodeId ->
  AWSCenters
getRegion regions (NodeId i) =
  regions !! (i `mod` length regions)

pickRecipient ::
  Monad m =>
  ClientId ->
  Integer ->
  StateT StdGen m ClientId
pickRecipient (NodeId me) n = do
  i <- state (randomR (1, n))
  if i == fromIntegral me
    then pickRecipient (NodeId me) n
    else pure $ NodeId (fromIntegral i)

connectClient ::
  (MonadAsync m, MonadTimer m, MonadTime m) =>
  Client m ->
  Server m ->
  m ()
connectClient client server =
  Multiplexer.connect
    (channel (client ^. #region) (server ^. #region))
    (client ^. #identifier, client ^. #multiplexer)
    (server ^. #identifier, server ^. #multiplexer)
