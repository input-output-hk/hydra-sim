{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Hydra.Tail.Simulation where

import Prelude

import Control.Exception (
  Exception,
 )
import Control.Monad (
  forM,
  forM_,
  forever,
  join,
  void,
 )
import Control.Monad.Class.MonadAsync (
  MonadAsync,
  async,
  concurrently_,
  forConcurrently_,
  replicateConcurrently_,
 )
import Control.Monad.Class.MonadSTM (
  MonadSTM,
  TMVar,
  TVar,
  atomically,
  modifyTVar',
  newTMVarIO,
  newTVarIO,
  putTMVar,
  readTVar,
  takeTMVar,
  writeTVar,
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
  traceWith,
 )
import Data.Generics.Internal.VL.Lens (
  (^.),
 )
import Data.Generics.Labels ()
import Data.List (
  delete,
  foldl',
  maximumBy,
 )
import Data.Map.Strict (
  Map,
 )
import qualified Data.Map.Strict as Map
import Data.Ratio (
  (%),
 )
import qualified Data.Set as Set
import Data.Time.Clock (
  DiffTime,
  picosecondsToDiffTime,
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
import HydraSim.DelayedComp (
  DelayedComp,
  delayedComp,
  runComp,
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

runSimulation :: RunOptions -> [Event] -> Trace ()
runSimulation opts@RunOptions{serverOptions} events = runSimTrace $ do
  let (serverId, clientIds) = (0, [1 .. fromInteger (getNumberOfClients events)])
  server <- newServer serverId clientIds serverOptions
  clients <- forM clientIds $ \clientId -> do
    client <- newClient clientId
    client <$ connectClient client server
  void $
    async $
      concurrently_
        (runServer trServer opts server)
        (forConcurrently_ clients (runClient trClient (trim events) serverId opts))
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
      let double = fromIntegral . unLovelace . lovelace
          asLovelace = Lovelace . round
       in maybe
            True
            (\w -> sent tx <= asLovelace (double w * (opts ^. #paymentCutOff)))
            (opts ^. #paymentWindow)
    _ -> True

  tracer :: Tracer (IOSim a) TraceTailSimulation
  tracer = Tracer IOSim.traceM

  trClient :: Tracer (IOSim a) TraceClient
  trClient = contramap TraceClient tracer

  trServer :: Tracer (IOSim a) TraceServer
  trServer = contramap TraceServer tracer

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
    SnapshotDone
  | --
    -- ↓↓↓ Server messages ↓↓↓
    --

    -- | The server will notify concerned clients with transactions they have subscribed to.
    -- How clients subscribe and how the server is keeping track of the subscription is currently
    -- out of scope and will be explored at a later stage.
    NotifyTx !MockTx
  | -- | The server requests a client to perform a snapshot.
    NeedSnapshot
  | -- | The server replies to each client submitting a transaction with an acknowledgement.
    AckTx !(TxRef MockTx)
  deriving (Generic, Show)

instance Sized Msg where
  size = \case
    NewTx tx ->
      sizeOfHeader + size tx + sizeOfAddress * fromIntegral (length $ txRecipients tx)
    NeedSnapshot{} ->
      sizeOfHeader
    SnapshotDone{} ->
      sizeOfHeader
    NotifyTx tx ->
      sizeOfHeader + size tx
    AckTx txId ->
      sizeOfHeader + size txId
   where
    sizeOfAddress = 57
    sizeOfHeader = 2

data TraceTailSimulation
  = TraceServer TraceServer
  | TraceClient TraceClient
  deriving (Show)

--
-- Server
--

type ServerId = NodeId

data Server m = Server
  { multiplexer :: Multiplexer m Msg
  , identifier :: ServerId
  , region :: AWSCenters
  , registry :: TMVar m (Map ClientId (ClientState, Balance, [Msg], [Msg]))
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
      [ (clientId, (Online, initialBalance 0, mempty, mempty))
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
    ( replicateConcurrently_
        (options ^. #serverOptions . #concurrency)
        (withLabel "Main: Server" serverMain)
    )
 where
  serverMain =
    forever $
      atomically (getMessage multiplexer) >>= handleMessage

  handleMessage = \case
    (clientId, NewTx tx) -> do
      void $ runComp (txValidate Set.empty tx)
      void $ runComp lookupClient

      -- Some of the recipients or the sender may be out of their payment window
      -- (i.e. 'DoingSnapshot'), if that's the case, we cannot process the transaction
      -- until they are done.
      blocked <- withTMVar registry $ \clients ->
        (,clients)
          <$> Map.traverseMaybeWithKey
            (matchDoingSnapshot (options ^. #paymentWindow) (clientId, tx))
            clients
      if not (null blocked)
        then do
          traceWith tracer (TraceTransactionBlocked (txId tx))
          withTMVar_ registry $
            execStateT $ do
              forM_ (zip [0 ..] (Map.elems blocked)) $ \case
                (ix, (client, Just NeedSnapshot)) -> do
                  lift $ sendTo multiplexer client NeedSnapshot
                  modifyM $
                    updateF client $ \(_st, balance, mailbox, pending) -> do
                      -- NOTE: This enqueues the message for one (and only one) of the
                      -- participants. A transaction shall be marked as pending if one of the
                      -- participants (sender or recipients) are blocked (require snapshots)
                      -- until both clients need to have performed their snapshot.
                      -- There's no "ledger" on the server, so if we re-enqueue a transaction
                      -- per blocked participant, we'll may replay the transaction more than once!
                      --
                      -- Thus, it is sufficient (and necessary) to re-enqueue the transaction only
                      -- once, for one of the participant. Once that participant is done
                      -- snapshotting, it'll retry the transaction and from here we really have
                      -- two scenarios:
                      --
                      -- a) That client happened to be the last one in the transaction which had
                      --    to snapshot, and the transaction can now proceed.
                      --
                      -- b) There's another client which is still blocked. Then, the transaction
                      --    will be re-enqueued again, for that client (or at least, for the first
                      --    other block client).
                      --
                      -- Eventually, once all clients are done, it goes through.
                      let pending' = if ix == (0 :: Int) then NewTx tx : pending else pending
                      pure $ Just (DoingSnapshot, balance, mailbox, pending')
                _ -> do
                  pure () -- Already blocked, and snapshot already requested.
        else do
          withTMVar_ registry $
            execStateT $ do
              lift $ atomically $ modifyTVar' transactions $ Map.insert (txRef tx) (tx, clientId, txRecipients tx)
              forM_ (txRecipients tx) $ \recipient -> do
                modifyM $
                  updateF recipient $ \case
                    (Online, balance, mailbox, pending) -> do
                      sendTo multiplexer recipient (NotifyTx tx)
                      pure $ Just (Online, balance, mailbox, pending)
                    (st, balance, mailbox, pending) -> do
                      let msg = NotifyTx tx
                      pure $ Just (st, balance, msg : mailbox, pending)
    (clientId, AckTx ref) -> do
      let ackTx = \case
            Nothing -> (Nothing, Nothing)
            Just (tx, sender, clients) ->
              let clients' = delete clientId clients
               in if null clients' then (Just (tx, sender), Nothing) else (Nothing, Just (tx, sender, clients'))

      processed <- atomically $ do
        (a, s) <- Map.alterF ackTx ref <$> readTVar transactions
        a <$ writeTVar transactions s

      case processed of
        Nothing -> pure ()
        Just (tx, sender) -> do
          withTMVar_ registry $
            execStateT $ do
              forM_ (txRecipients tx) $ \recipient -> do
                modifyM $
                  updateF recipient $ \(st, balance, mailbox, pending) -> do
                    let balance' = modifyCurrent (+ received tx) balance
                    pure $ Just (st, balance', mailbox, pending)
              modifyM $
                updateF sender $ \(st, balance, mailbox, pending) -> do
                  let balance' = modifyCurrent (\x -> x - sent tx) balance

                  -- Check whether we should send 'NeedSnapshot' for pro-active
                  -- snapshotting (after the tx). Logically this would be done on the
                  -- client-side but we have the payment window 'registry' only on the
                  -- server for now.
                  (st', mailbox') <- case st of
                    Online -> do
                      sendTo multiplexer sender (AckTx $ txRef tx)
                      if inProactiveSnapshotLimit options balance'
                        then do
                          sendTo multiplexer clientId NeedSnapshot
                          pure (DoingSnapshot, mailbox)
                        else do
                          pure (st, mailbox)
                    _ -> do
                      let ack = AckTx (txRef tx)
                      if inProactiveSnapshotLimit options balance'
                        then do
                          pure (DoingSnapshot, NeedSnapshot : ack : mailbox)
                        else do
                          pure (st, ack : mailbox)

                  pure $ Just (st', balance', mailbox', pending)
    (clientId, SnapshotDone) -> do
      runComp lookupClient
      pending <- withTMVar registry $ \clients -> do
        case Map.lookup clientId clients of
          Nothing -> pure ([], clients)
          Just (_st, Balance{current}, mailbox, pending) -> do
            let clients' = Map.insert clientId (Online, initialBalance current, mailbox, []) clients
            pure (pending, clients')
      mapM_ handleMessage (reverse $ (clientId,) <$> pending)
    (clientId, msg) ->
      throwIO (UnexpectedServerMsg clientId msg)

-- | A computation simulating the time needed to lookup a client in an in-memory registry.
-- The value is taken from running benchmarks of the 'containers' Haskell library on a
-- high-end laptop. The time needed to perform a lookup was deemed non negligeable in front of
-- the time needed to validate a transaction.
--
-- Note that a typical hashmap or map is implemented using balanced binary trees and provide a O(log(n))
-- lookup performances, so the cost of looking a client in a map of 1000 or 100000 clients is _roughly the same_.
lookupClient :: DelayedComp ()
lookupClient =
  delayedComp () (picosecondsToDiffTime 500 * 1e6) -- 500μs

-- | Return 'f (Just ClientId)' iif a client would exceed (bottom or top) its payment window
-- from the requested payment, or if it's already performing a snapshot.
--
-- NOTE: There's a slight _abuse_ here. Transactions are indeed written from the PoV
-- of the _sender_. So the amount corresponds to how much did the sender "lost" in the
-- transaction, but, there can be multiple recipients! Irrespective of this, we consider
-- in the simulation that *each* recipient receives the full amount.
matchDoingSnapshot ::
  Applicative f =>
  Maybe Ada ->
  (ClientId, MockTx) ->
  ClientId ->
  (ClientState, Balance, mailbox, pending) ->
  f (Maybe (ClientId, Maybe Msg))
matchDoingSnapshot Nothing _ _ _ =
  pure Nothing
matchDoingSnapshot (Just paymentWindow) (sender, tx) clientId (st, balance, _, _)
  | clientId `elem` txRecipients tx =
    case (st, viewPaymentWindow (lovelace paymentWindow) balance (received tx)) of
      (DoingSnapshot, _) ->
        pure (Just (clientId, Nothing))
      (_, OutOfPaymentWindow) ->
        pure (Just (clientId, Just NeedSnapshot))
      (_, InPaymentWindow) ->
        pure Nothing
  | clientId == sender =
    case (st, viewPaymentWindow (lovelace paymentWindow) balance (negate $ sent tx)) of
      (DoingSnapshot, _) ->
        pure (Just (clientId, Nothing))
      (_, OutOfPaymentWindow) ->
        pure (Just (clientId, Just NeedSnapshot))
      (_, InPaymentWindow) ->
        pure Nothing
  | otherwise =
    pure Nothing

inProactiveSnapshotLimit :: RunOptions -> Balance -> Bool
inProactiveSnapshotLimit RunOptions{paymentWindow, proactiveSnapshot} Balance{initial, current} =
  case (paymentWindow, proactiveSnapshot) of
    (Just w, Just frac) -> absBalance > limit w frac
    _ -> False
 where
  absBalance = abs $ current - initial

  limit w frac = fromDouble (toDouble (lovelace w) * frac)

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

data ClientState = Online | DoingSnapshot
  deriving (Generic, Show, Eq)

data Client m = Client
  { multiplexer :: Multiplexer m Msg
  , identifier :: ClientId
  , region :: AWSCenters
  }
  deriving (Generic)

newClient :: MonadSTM m => ClientId -> m (Client m)
newClient identifier = do
  multiplexer <-
    newMultiplexer
      ("client-" <> show (getNodeId identifier))
      outboundBufferSize
      inboundBufferSize
      (capacity $ kbitsPerSecond 512)
      (capacity $ kbitsPerSecond 512)
  return Client{multiplexer, identifier, region}
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
  [Event] ->
  ServerId ->
  RunOptions ->
  Client m ->
  m ()
runClient tracer events serverId opts Client{multiplexer, identifier} = do
  st <- newTMVarIO Online
  semaphore <- newTMVarIO ()
  concurrently_
    (startMultiplexer (contramap TraceClientMultiplexer tracer) multiplexer)
    ( concurrently_
        (withLabel ("EventLoop: " <> show identifier) $ clientEventLoop semaphore st 0 events)
        (withLabel ("Main: " <> show identifier) $ forever $ clientMain semaphore st)
    )
 where
  clientMain :: TMVar m () -> TMVar m ClientState -> m ()
  clientMain !waitForAck !var =
    atomically (getMessage multiplexer) >>= \case
      (_, AckTx{}) ->
        -- NOTE(SN): For pro-active snapshot handling, we would ideally keep
        -- track for the client's payment window here and decide whether or not
        -- to snapshot. For simplicity reasons, this is also shifted to the
        -- server (for now) as we do track payment windows there.
        atomically $ putTMVar waitForAck ()
      (_, NotifyTx tx) ->
        sendTo multiplexer serverId (AckTx (txRef tx))
      (_, NeedSnapshot{}) -> do
        -- NOTE: Holding on the MVar here prevents the client's event loop from
        -- processing any new event. The other will block until the snapshot is done.
        withTMVar_ var $ \st -> do
          threadDelay settlementDelay_
          sendTo multiplexer serverId SnapshotDone
          pure st
      (nodeId, msg) ->
        throwIO $ UnexpectedClientMsg nodeId msg
   where
    settlementDelay_ =
      secondsToDiffTime (unSlotNo (opts ^. #settlementDelay))
        * (opts ^. #slotLength)

  clientEventLoop :: TMVar m () -> TMVar m ClientState -> SlotNo -> [Event] -> m ()
  clientEventLoop !waitForAck !var !currentSlot = \case
    [] ->
      pure ()
    (e : q)
      | from e /= identifier ->
        clientEventLoop waitForAck var currentSlot q
    (e : q) | (slot :: Event -> SlotNo) e <= currentSlot -> do
      atomically $ takeTMVar waitForAck
      withTMVar_ var $ \_st -> do
        Online <$ sendTo multiplexer serverId (msg e)
      case msg e of
        NewTx{} -> do
          traceWith tracer (TraceClientWakeUp currentSlot)
          pure ()
        _ -> atomically $ putTMVar waitForAck ()
      clientEventLoop waitForAck var currentSlot q
    (e : q) -> do
      threadDelay (opts ^. #slotLength)
      clientEventLoop waitForAck var (currentSlot + 1) (e : q)

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
    fmap Ada $
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
          , NewTx (mockTx identifier currentSlot (lovelace amount) txSize [recipient])
          )
        ]
    , predicate
    ]
 where
  ClientOptions{submitLikelihood} = options ^. #clientOptions

data TraceClient
  = TraceClientMultiplexer (TraceMultiplexer Msg)
  | TraceClientWakeUp SlotNo
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

data SimulationSummary = SimulationSummary
  { numberOfClients :: !Integer
  , numberOfEvents :: !Integer
  , numberOfTransactions :: !NumberOfTransactions
  , averageTransaction :: !Ada
  , lastSlot :: !SlotNo
  }
  deriving (Generic, Show)

data NumberOfTransactions = NumberOfTransactions
  { total :: !Integer
  , belowPaymentWindow :: !Integer
  , belowHalfOfPaymentWindow :: !Integer
  , belowTenthOfPaymentWindow :: !Integer
  }
  deriving (Generic, Show)

summarizeEvents :: RunOptions -> [Event] -> SimulationSummary
summarizeEvents RunOptions{paymentWindow} events =
  SimulationSummary
    { numberOfClients
    , numberOfEvents
    , numberOfTransactions
    , averageTransaction
    , lastSlot
    }
 where
  numberOfEvents = toInteger $ length events
  numberOfClients = getNumberOfClients events
  (volumeTotal, numberOfTransactions) = foldl' count (0, NumberOfTransactions 0 0 0 0) events
   where
    w = maybe 1e99 (asDouble . lovelace) paymentWindow
    count (!volume, st) = \case
      (Event _ _ (NewTx MockTx{txAmount})) ->
        ( volume + if asDouble txAmount <= w then txAmount else 0
        , st
            { total =
                countIf True st total
            , belowPaymentWindow =
                countIf (asDouble txAmount <= w) st belowPaymentWindow
            , belowHalfOfPaymentWindow =
                countIf (asDouble txAmount <= (w / 2)) st belowHalfOfPaymentWindow
            , belowTenthOfPaymentWindow =
                countIf (asDouble txAmount <= (w / 10)) st belowTenthOfPaymentWindow
            }
        )
      _ -> (volume, st)
    countIf predicate st fn =
      if predicate then fn st + 1 else fn st
    asDouble = fromIntegral @_ @Double . unLovelace
  averageTransaction =
    ada $ Lovelace $ unLovelace volumeTotal `div` (numberOfTransactions ^. #belowPaymentWindow)
  lastSlot = last events ^. #slot

-- | Calculate simulation time as the last event + twice the settlement delay.
durationOf :: RunOptions -> [Event] -> DiffTime
durationOf RunOptions{slotLength, settlementDelay} events =
  slotLength * fromIntegral (unSlotNo $ (last events ^. #slot) + 2 * settlementDelay)

getNumberOfClients :: [Event] -> Integer
getNumberOfClients =
  toInteger . getNodeId . from . maximumBy (\a b -> getNodeId (from a) `compare` getNodeId (from b))

--
-- Helpers
--

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
