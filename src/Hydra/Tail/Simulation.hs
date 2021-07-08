{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Tail.Simulation where

import Prelude

import Control.Exception
    ( Exception )
import Control.Monad
    ( forM, forM_, forever, join, liftM5, void, when )
import Control.Monad.Class.MonadAsync
    ( MonadAsync
    , async
    , concurrently_
    , forConcurrently_
    , replicateConcurrently_
    )
import Control.Monad.Class.MonadSTM
    ( MonadSTM, TMVar, atomically, newTMVarIO )
import Control.Monad.Class.MonadThrow
    ( MonadThrow, throwIO )
import Control.Monad.Class.MonadTime
    ( MonadTime, Time (..) )
import Control.Monad.Class.MonadTimer
    ( MonadTimer, threadDelay )
import Control.Monad.IOSim
    ( IOSim, ThreadLabel, Trace (..), runSimTrace )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.State.Strict
    ( StateT, evalStateT, execStateT, state )
import Control.Tracer
    ( Tracer (..), contramap, traceWith )
import Data.Foldable
    ( traverse_ )
import Data.Functor
    ( ($>) )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Labels
    ()
import Data.List
    ( foldl', maximumBy )
import Data.Map.Strict
    ( Map )
import Data.Ratio
    ( (%) )
import Data.Text
    ( Text )
import Data.Time.Clock
    ( DiffTime, picosecondsToDiffTime, secondsToDiffTime )
import GHC.Generics
    ( Generic )
import Safe
    ( readMay )
import System.Random
    ( StdGen, newStdGen, randomR )

import qualified Control.Monad.IOSim as IOSim
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Hydra.Tail.Simulation.MockTx
    ( MockTx (..), mockTx, received, sent, TxRef(..))
import Hydra.Tail.Simulation.Options
    ( ClientOptions (..)
    , NetworkCapacity (..)
    , PrepareOptions (..)
    , RunOptions (..)
    , ServerOptions (..)
    , kbitsPerSecond
    )
import Hydra.Tail.Simulation.PaymentWindow
    ( Ada (..)
    , Balance (..)
    , Lovelace (..)
    , PaymentWindowStatus (..)
    , ada
    , initialBalance
    , lovelace
    , modifyCurrent
    , viewPaymentWindow
    )
import Hydra.Tail.Simulation.SlotNo
    ( SlotNo (..) )
import Hydra.Tail.Simulation.Utils
    ( foldTraceEvents
    , frequency
    , modifyM
    , updateF
    , withLabel
    , withTMVar
    , withTMVar_
    )
import HydraSim.Analyse
    ( diffTimeToSeconds )
import HydraSim.DelayedComp
    ( DelayedComp, delayedComp, runComp )
import HydraSim.Examples.Channels
    ( AWSCenters (..), channel )
import HydraSim.Multiplexer
    ( Multiplexer
    , getMessage
    , newMultiplexer
    , reenqueue
    , sendTo
    , startMultiplexer
    )
import HydraSim.Multiplexer.Trace
    ( TraceMultiplexer (..) )
import HydraSim.Sized
    ( Size (..), Sized (..) )
import HydraSim.Tx.Class
    ( Tx (..) )
import HydraSim.Types
    ( NodeId (..) )

import qualified HydraSim.Multiplexer as Multiplexer
import Data.Maybe (mapMaybe)

--
-- Simulation
--

prepareSimulation :: PrepareOptions -> IO [Event]
prepareSimulation options@PrepareOptions{numberOfClients,duration} = do
  let clientIds = [1..fromInteger numberOfClients]
  let events = fmap join $ forM [0 .. pred duration] $ \currentSlot -> do
        join <$> traverse (stepClient options currentSlot) clientIds
  newStdGen >>= evalStateT events

runSimulation :: RunOptions -> [Event] -> Trace ()
runSimulation opts@RunOptions{slotLength,serverOptions} events = runSimTrace $ do
  let (serverId, clientIds) = (0, [1..fromInteger (getNumberOfClients events)])
  server <- newServer serverId clientIds serverOptions
  clients <- forM clientIds $ \clientId -> do
    client <- newClient clientId
    client <$ connectClient client server
  void $ async $ concurrently_
    (runServer trServer opts server)
    (forConcurrently_ clients (runClient trClient (trim events) serverId opts))
  threadDelay (durationOf events slotLength)
 where
  -- We remove any transaction that is above the payment window, for they are
  -- actually unprocessable by the server (theoritically, transactions can be as
  -- large as 2*W, but even after a snapshot, accepting a transaction larger than W
  -- would require the client to first spend the difference. For simplicity, we
  -- consider that the server would reject any transaction larger than `W`, so we
  -- filter them out of the simulation if any.
  trim = filter $ \case
    Event _ _ (NewTx tx) ->
      maybe True (\w -> sent tx <= lovelace w) (opts ^. #paymentWindow)
    _ -> True

  tracer :: Tracer (IOSim a) TraceTailSimulation
  tracer = Tracer IOSim.traceM

  trClient :: Tracer (IOSim a) TraceClient
  trClient = contramap TraceClient tracer

  trServer :: Tracer (IOSim a) TraceServer
  trServer = contramap TraceServer tracer

data Analyze = Analyze
  { numberOfConfirmedTransactions :: Int
    -- ^ Number of confirmed transactions within the timespan of the simulation
  , maxThroughput :: Double
    -- ^ Throughput as generated by clients
  , actualThroughput :: Double
    -- ^ Actual Throughput measured from confirmed transactions.
    -- Said differently, the ratio of transactions compared to the number of snapshots.
  , averageConfirmationTime :: Double
    -- ^ Average time for a transaction to get 'confirmed'. This includes snapshotting when
    -- relevant.
  } deriving (Generic, Show)

type Transactions = Map (TxRef MockTx) [DiffTime]

analyzeSimulation
  :: forall m. Monad m
  => (SlotNo -> Maybe Analyze -> m ())
  -> RunOptions
  -> SimulationSummary
  -> [Event]
  -> Trace ()
  -> m Transactions
analyzeSimulation notify RunOptions{slotLength} summary _events trace = do
  (confirmations, _) <-
    let fn :: (ThreadLabel, Time, TraceTailSimulation)
           -> (Map (TxRef MockTx) [DiffTime], SlotNo)
           -> m (Map (TxRef MockTx) [DiffTime], SlotNo)
        fn = \case
          (_threadLabel, Time t, TraceClient (TraceClientMultiplexer (MPRecvTrailing _nodeId (AckTx ref)))) ->
            (\(!m, !sl) -> pure
              ( Map.update (\ts -> Just (t : ts)) ref m
              , sl
              ))

          (_threadLabel, Time t, TraceClient (TraceClientMultiplexer (MPSendTrailing _nodeId (NewTx tx)))) ->
            (\(!m, !sl) -> pure (Map.insert (txRef tx) [t] m, sl))

          (_threadLabel, _time, TraceClient (TraceClientWakeUp sl')) ->
            (\(!m, !sl) ->
              if sl' > sl then
                if sl' /= 0 && unSlotNo sl' `mod` 60 == 0 then
                  let duration = slotLength * fromIntegral (unSlotNo sl') in
                  notify sl' (Just $ mkAnalyze duration summary m) $> (m, sl')
                else
                  notify sl' Nothing $> (m, sl')
              else
                pure (m, sl)
            )
          _ ->
            pure

     in foldTraceEvents fn (mempty, -1) trace
  pure confirmations

mkAnalyze
  :: DiffTime
  -> SimulationSummary
  -> Transactions
  -> Analyze
mkAnalyze duration SimulationSummary{numberOfTransactions} txs =
  let confirmedTxs =
        Map.elems $ Map.filter ((== 2) . length) txs

      totalConfirmationTime =
        diffTimeToSeconds $ foldl' (\total -> \case
          [end, start] -> total + (end - start)
          _ -> total) 0 confirmedTxs

      numberOfConfirmedTransactions =
        length confirmedTxs
  in
  Analyze
    { numberOfConfirmedTransactions
    , maxThroughput =
        fromIntegral (numberOfTransactions ^. #belowPaymentWindow) / diffTimeToSeconds duration
    , actualThroughput =
        fromIntegral numberOfConfirmedTransactions / diffTimeToSeconds duration
    , averageConfirmationTime =
        totalConfirmationTime / fromIntegral numberOfConfirmedTransactions
    }

writeTransactions :: FilePath -> Transactions -> IO ()
writeTransactions filepath transactions = do
  TIO.writeFile filepath $ T.unlines $
    "txId,confirmationTime"
    : mapMaybe toCsv (Map.toList transactions)
 where
  toCsv :: (TxRef MockTx, [DiffTime]) -> Maybe Text
  toCsv (TxRef ref, [end, start]) =
    Just $ replaceCommas ref <> "," <> tshow (diffTimeToSeconds (end - start))
  toCsv _ =
    Nothing

  replaceCommas :: Text -> Text
  replaceCommas = T.map (\c -> if c == ',' then ';' else c)

tshow :: Show a => a -> Text
tshow = T.pack . show

--
-- (Simplified) Tail-Protocol
--

-- | Messages considered as part of the simplified Tail pre-protocol. We don't know exactly
-- what the Tail protocol hence we have a highly simplified view of it and reduce it to a
-- mere message broker between many producers and many consumers (the clients), linked together
-- via a single message broker (the server).
data Msg
  --
  -- ↓↓↓ Client messages ↓↓↓
  --
  = NewTx !MockTx
  -- ^ A new transaction, sent to some peer. The current behavior of this simulation
  -- consider that each client is only sending to one single peer. Later, we probably
  -- want to challenge this assumption by analyzing real transaction patterns from the
  -- main chain and model this behavior.

  | Pull
  -- ^ Sent when waking up to catch up on messages received when offline.

  | Connect
  -- ^ Client connections and disconnections are modelled using 0-sized messages.

  | Disconnect
  -- ^ Client connections and disconnections are modelled using 0-sized messages.

  | SnapshotDone
  -- ^ Clients informing the server about the end of a snapshot

  --
  -- ↓↓↓ Server messages ↓↓↓
  --

  | NotifyTx !MockTx
  -- ^ The server will notify concerned clients with transactions they have subscribed to.
  -- How clients subscribe and how the server is keeping track of the subscription is currently
  -- out of scope and will be explored at a later stage.

  | NeedSnapshot
  -- ^ The server requests a client to perform a snapshot.

  | AckTx !(TxRef MockTx)
  -- ^ The server replies to each client submitting a transaction with an acknowledgement.
  deriving (Generic, Show)

instance Sized Msg where
  size = \case
    NewTx tx ->
      sizeOfHeader + size tx + sizeOfAddress * fromIntegral (length $ txRecipients tx)
    Pull ->
      sizeOfHeader
    Connect{} ->
      0
    Disconnect{} ->
      0
    NeedSnapshot{} ->
      0
    SnapshotDone{} ->
      0
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
  , identifier  :: ServerId
  , region :: AWSCenters
  , registry :: TMVar m (Map ClientId (ClientState, Balance, [Msg], [Msg]))
  } deriving (Generic)

newServer
  :: MonadSTM m
  => ServerId
  -> [ClientId]
  -> ServerOptions
  -> m (Server m)
newServer identifier clientIds ServerOptions{region,writeCapacity,readCapacity} = do
  multiplexer <- newMultiplexer
    "server"
    outboundBufferSize
    inboundBufferSize
    (capacity writeCapacity)
    (capacity readCapacity)
  registry <- newTMVarIO clients
  return Server { multiplexer, identifier, region, registry }
 where
  outboundBufferSize = 1000000
  inboundBufferSize = 1000000
  -- NOTE: We care little about how much each client balance is in practice. Although
  -- the 'Balance' is modelled as a product (initial, current) because of the intuitive
  -- view it offers, we are really only interested in the delta. Balances can therefore
  -- be _negative_ as part of the simulation.
  clients = Map.fromList
    [ (clientId, (Offline, initialBalance 0, mempty, mempty))
    | clientId <- clientIds
    ]

runServer
  :: forall m. (MonadAsync m, MonadTimer m, MonadThrow m)
  => Tracer m TraceServer
  -> RunOptions
  -> Server m
  -> m ()
runServer tracer options Server{multiplexer, registry} = do
  concurrently_
    (startMultiplexer (contramap TraceServerMultiplexer tracer) multiplexer)
    (replicateConcurrently_
      (options ^. #serverOptions . #concurrency)
      (withLabel "Main: Server" serverMain)
    )
 where
  reenqueue' = reenqueue (contramap TraceServerMultiplexer tracer)

  serverMain :: m ()
  serverMain = do
    atomically (getMessage multiplexer) >>= \case
      (clientId, NewTx tx) -> do
        void $ runComp (txValidate Set.empty tx)
        void $ runComp lookupClient

        -- Some of the recipients or the sender may be out of their payment window
        -- (i.e. 'Blocked'), if that's the case, we cannot process the transaction
        -- until they are done.
        blocked <- withTMVar registry $ \clients -> (,clients)
            <$> Map.traverseMaybeWithKey
                  (matchBlocked (options ^. #paymentWindow) (clientId, tx))
                  clients
        if not (null blocked) then do
          withTMVar_ registry $ execStateT $ do
            forM_ (zip [0..] (Map.elems blocked)) $ \case
              (ix, (client, Just NeedSnapshot)) -> do
                lift $ sendTo multiplexer client NeedSnapshot
                modifyM $ updateF client $ \(_st, balance, mailbox, pending) -> do
                  let pending' = if ix == (0 :: Int) then NewTx tx:pending else pending
                  pure $ Just (Blocked, balance, mailbox, pending')
              _ -> do
                pure () -- Already blocked, and snapshot already requested.

        else do
          withTMVar_ registry $ execStateT $ do
            forM_ (txRecipients tx) $ \recipient -> do
              modifyM $ updateF recipient $ \case
                (Online, balance, mailbox, pending) -> do
                  sendTo multiplexer recipient (NotifyTx tx)
                  pure $ Just (Online, modifyCurrent (+ received tx) balance , mailbox, pending)
                (st, balance, mailbox, pending) -> do
                  let msg = NotifyTx tx
                  traceWith tracer $ TraceServerStoreInMailbox clientId msg (length mailbox + 1)
                  pure $ Just (st, modifyCurrent (+ received tx) balance, msg:mailbox, pending)

            modifyM $ updateF clientId $ \(st, balance, mailbox, pending) ->
              pure $ Just (st, modifyCurrent (\x -> x - sent tx) balance, mailbox, pending)

          sendTo multiplexer clientId (AckTx $ txRef tx)

        -- TODO(SN): Check whether we should send 'NeedSnapshot' for pro-active
        -- snapshotting (after the tx). Logically this would be done on the
        -- client-side but we have the payment window 'registry' only on the
        -- server for now.

        serverMain

      (clientId, Pull) -> do
        runComp lookupClient
        withTMVar_ registry $ \clients -> do
          updateF clientId (\case
            (st, balance, mailbox, pending) -> do
              mapM_ (sendTo multiplexer clientId) (reverse mailbox)
              pure $ Just (st, balance, [], pending)
            ) clients
        serverMain

      (clientId, Connect) -> do
        runComp lookupClient
        withTMVar_ registry $ \clients -> do
          return $ Map.update (\(_, balance, mailbox, pending) -> Just (Online, balance, mailbox, pending)) clientId clients
        serverMain

      (clientId, Disconnect) -> do
        runComp lookupClient
        withTMVar_ registry $ \clients -> do
          return $ Map.update (\(_, balance, mailbox, pending) -> Just (Offline, balance, mailbox, pending)) clientId clients
        serverMain

      (clientId, SnapshotDone) -> do
        runComp lookupClient
        withTMVar_ registry $ updateF clientId $ \(_st, Balance{current}, mailbox, pending) -> do
          traverse_ (reenqueue' multiplexer) (reverse $ (clientId,) <$> pending)
          return $ Just (Offline, initialBalance current, mailbox, [])
        serverMain

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
  delayedComp () (picosecondsToDiffTime 500*1e6) -- 500μs

-- | Return 'f (Just ClientId)' iif a client would exceed (bottom or top) its payment window
-- from the requested payment, or if it's already performing a snapshot.
--
-- NOTE: There's a slight _abuse_ here. Transactions are indeed written from the PoV
-- of the _sender_. So the amount corresponds to how much did the sender "lost" in the
-- transaction, but, there can be multiple recipients! Irrespective of this, we consider
-- in the simulation that *each* recipient receives the full amount.
matchBlocked
  :: Applicative f
  => Maybe Ada
  -> (ClientId, MockTx)
  -> ClientId
  -> (ClientState, Balance, mailbox, pending)
  -> f (Maybe (ClientId, Maybe Msg))
matchBlocked Nothing _ _ _ =
  pure Nothing
matchBlocked (Just paymentWindow) (sender, tx) clientId (st, balance, _, _)
  | clientId `elem` txRecipients tx =
      case (st, viewPaymentWindow (lovelace paymentWindow) balance (received tx)) of
        (Blocked, _) ->
          pure (Just (clientId, Nothing))
        (_, OutOfPaymentWindow) ->
          pure (Just (clientId, Just NeedSnapshot))
        (_, InPaymentWindow) ->
          pure Nothing

  | clientId == sender =
      case (st, viewPaymentWindow (lovelace paymentWindow) balance (negate $ sent tx)) of
        (Blocked, _) ->
          pure (Just (clientId, Nothing))
        (_, OutOfPaymentWindow) ->
          pure (Just (clientId, Just NeedSnapshot))
        (_, InPaymentWindow) ->
          pure Nothing

  | otherwise =
      pure Nothing

data TraceServer
  = TraceServerMultiplexer (TraceMultiplexer Msg)
  | TraceServerStoreInMailbox ClientId Msg Int
  deriving (Show)

data ServerMain = ServerMain deriving Show
instance Exception ServerMain

data UnexpectedServerMsg = UnexpectedServerMsg NodeId Msg
  deriving Show
instance Exception UnexpectedServerMsg

data UnknownClient = UnknownClient NodeId
  deriving Show
instance Exception UnknownClient

--
-- Client
--

type ClientId = NodeId

data ClientState = Online | Offline | Blocked
  deriving (Generic, Show, Eq)

data Client m = Client
  { multiplexer :: Multiplexer m Msg
  , identifier  :: ClientId
  , region :: AWSCenters
  } deriving (Generic)

newClient :: MonadSTM m => ClientId -> m (Client m)
newClient identifier = do
  multiplexer <- newMultiplexer
    ("client-" <> show (getNodeId identifier))
    outboundBufferSize
    inboundBufferSize
    (capacity $ kbitsPerSecond 512)
    (capacity $ kbitsPerSecond 512)
  return Client { multiplexer, identifier, region }
 where
  outboundBufferSize = 1000
  inboundBufferSize = 1000
  region = LondonAWS

runClient
  :: forall m. (MonadAsync m, MonadTimer m, MonadThrow m)
  => Tracer m TraceClient
  -> [Event]
  -> ServerId
  -> RunOptions
  -> Client m
  -> m ()
runClient tracer events serverId opts Client{multiplexer, identifier} = do
  st <- newTMVarIO Offline
  concurrently_
    (startMultiplexer (contramap TraceClientMultiplexer tracer) multiplexer)
    (concurrently_
      (withLabel ("EventLoop: " <> show identifier) $ clientEventLoop st 0 events)
      (withLabel ("Main: " <> show identifier) $ forever $ clientMain st)
    )
 where
  clientMain :: TMVar m ClientState -> m ()
  clientMain var =
    atomically (getMessage multiplexer) >>= \case
      (_, AckTx{}) ->
        -- NOTE(SN): For pro-active snapshot handling, we would ideally keep
        -- track for the client's payment window here and decide whether or not
        -- to snapshot. For simplicity reasons, this is also shifted to the
        -- server (for now) as we do track payment windows there.
        pure ()
      (_, NotifyTx{}) ->
        pure ()
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
      *
      (opts ^. #slotLength)

  clientEventLoop :: TMVar m ClientState -> SlotNo -> [Event] -> m ()
  clientEventLoop !var !currentSlot = \case
    [] ->
      pure ()

    (e:q) | from e /= identifier ->
      clientEventLoop var currentSlot q

    (e:q) | slot e <= currentSlot -> do
      withTMVar_ var $ \st -> do
        when (st == Offline) $ do
          traceWith tracer (TraceClientWakeUp currentSlot)
          sendTo multiplexer serverId Connect
        Online <$ sendTo multiplexer serverId (msg e)
      clientEventLoop var currentSlot q

    (e:q) -> do
      withTMVar_ var $ \st ->
        Offline <$ when (st == Online) (sendTo multiplexer serverId Disconnect)
      threadDelay (opts ^. #slotLength)
      clientEventLoop var (currentSlot + 1) (e:q)

data UnexpectedClientMsg = UnexpectedClientMsg NodeId Msg
  deriving Show
instance Exception UnexpectedClientMsg

stepClient
  :: forall m. (Monad m)
  => PrepareOptions
  -> SlotNo
  -> ClientId
  -> StateT StdGen m [Event]
stepClient options currentSlot identifier = do
  pOnline <- state (randomR (1, 100))
  let online = pOnline % 100 <= onlineLikelihood
  pSubmit <- state (randomR (1, 100))
  let submit = online && (pSubmit % 100 <= submitLikelihood)
  recipient <- pickRecipient identifier (options ^. #numberOfClients)

  -- NOTE: The distribution is extrapolated from real mainchain data.
  amount <- fmap Ada $ state $ frequency
    [ (122, randomR (1, 10))
    , (144, randomR (10, 100))
    , (143, randomR (100, 1000))
    , ( 92, randomR (1000, 10000))
    , ( 41, randomR (10000, 100000))
    , ( 12, randomR (100000, 1000000))
    ]

  -- NOTE: The distribution is extrapolated from real mainchain data.
  txSize <- fmap Size $ state $ frequency
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
        [ ( online
          , Pull
          )
        , ( submit
          , NewTx (mockTx identifier currentSlot (lovelace amount) txSize [recipient])
          )
        ]
    , predicate
    ]
 where
  ClientOptions { onlineLikelihood, submitLikelihood } = options ^. #clientOptions

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
  } deriving (Generic, Show)

data SimulationSummary = SimulationSummary
  { numberOfClients :: !Integer
  , numberOfEvents :: !Integer
  , numberOfTransactions :: !NumberOfTransactions
  , averageTransaction :: !Ada
  , lastSlot :: !SlotNo
  } deriving (Generic, Show)

data NumberOfTransactions = NumberOfTransactions
  { total :: !Integer
  , belowPaymentWindow :: !Integer
  , belowHalfOfPaymentWindow :: !Integer
  , belowTenthOfPaymentWindow :: !Integer
  } deriving (Generic, Show)

summarizeEvents :: RunOptions -> [Event] -> SimulationSummary
summarizeEvents RunOptions{paymentWindow} events = SimulationSummary
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
    countIf predicate st get =
      if predicate then get st + 1 else get st
    asDouble = fromIntegral @_ @Double . unLovelace
  averageTransaction =
    ada $ Lovelace $ unLovelace volumeTotal `div` (numberOfTransactions ^. #belowPaymentWindow)
  lastSlot = last events ^. #slot

durationOf :: [Event] -> DiffTime -> DiffTime
durationOf events slotLength =
  slotLength * fromIntegral (unSlotNo $ succ $ last events ^. #slot)

getNumberOfClients :: [Event] -> Integer
getNumberOfClients =
  toInteger . getNodeId . from . maximumBy (\a b -> getNodeId (from a) `compare` getNodeId (from b))

data CouldntParseCsv = CouldntParseCsv FilePath
  deriving Show
instance Exception CouldntParseCsv

writeEvents :: FilePath -> [Event] -> IO ()
writeEvents filepath events = do
  TIO.writeFile filepath $ T.unlines $
    "slot,clientId,event,size,amount,recipients"
    : (eventToCsv <$> events)

readEventsThrow :: FilePath -> IO [Event]
readEventsThrow filepath = do
  text <- TIO.readFile filepath
  case traverse eventFromCsv . drop 1 . T.lines $ text of
    Nothing -> throwIO $ CouldntParseCsv filepath
    Just events -> pure events

eventToCsv :: Event -> Text
eventToCsv = \case
  -- slot,clientId,'pull'
  Event (SlotNo sl) (NodeId cl) Pull ->
    T.intercalate ","
      [ T.pack (show sl)
      , T.pack (show cl)
      , "pull"
      ]

  -- slot,clientId,new-tx,size,amount,recipients
  Event (SlotNo sl) (NodeId cl) (NewTx (MockTx _ (Size sz) (Lovelace am) rs)) ->
    T.intercalate ","
      [ T.pack (show sl)
      , T.pack (show cl)
      , "new-tx"
      , T.pack (show sz)
      , T.pack (show am)
      , T.intercalate " " (T.pack . show . getNodeId <$> rs)
      ]

  e ->
    error $ "eventToCsv: invalid event to serialize: " <> show e

eventFromCsv :: Text -> Maybe Event
eventFromCsv line =
  case T.splitOn "," line of
    -- slot,clientId,'pull'
    (sl: (cl: ("pull": _))) -> Event
        <$> readSlotNo sl
        <*> readClientId cl
        <*> pure Pull

    -- slot,clientId,new-tx,size,amount,recipients
    [ sl, cl, "new-tx", sz, am, rs ] -> Event
        <$> readSlotNo sl
        <*> readClientId cl
        <*> (NewTx <$> liftM5 mockTx
              (readClientId cl)
              (readSlotNo sl)
              (readAmount am)
              (readSize sz)
              (readRecipients rs)
            )
    _ ->
      Nothing
 where
  readClientId :: Text -> Maybe ClientId
  readClientId =
    fmap NodeId . readMay . T.unpack

  readSlotNo :: Text -> Maybe SlotNo
  readSlotNo =
    fmap SlotNo . readMay . T.unpack

  readAmount :: Text -> Maybe Lovelace
  readAmount =
    readMay . T.unpack

  readSize :: Text -> Maybe Size
  readSize =
    fmap Size . readMay . T.unpack

  readRecipients :: Text -> Maybe [ClientId]
  readRecipients = \case
    "" -> Just []
    ssv -> traverse readClientId (T.splitOn " " ssv)

--
-- Helpers
--

getRegion
  :: [AWSCenters]
  -> NodeId
  -> AWSCenters
getRegion regions (NodeId i) =
  regions !! (i `mod` length regions)

pickRecipient
  :: Monad m
  => ClientId
  -> Integer
  -> StateT StdGen m ClientId
pickRecipient (NodeId me) n = do
  i <- state (randomR (1, n))
  if i == fromIntegral me then
    pickRecipient (NodeId me) n
  else
    pure $ NodeId (fromIntegral i)

connectClient
  :: (MonadAsync m, MonadTimer m, MonadTime m)
  => Client m
  -> Server m -> m ()
connectClient client server =
  Multiplexer.connect
    ( channel (client ^. #region) (server ^. #region) )
    ( client ^. #identifier, client ^. #multiplexer )
    ( server ^. #identifier, server ^. #multiplexer )
