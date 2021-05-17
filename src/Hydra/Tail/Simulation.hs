{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Tail.Simulation where

import Prelude

import Control.Exception
    ( Exception )
import Control.Monad
    ( foldM, forM, forM_, liftM4, void )
import Control.Monad.Class.MonadAsync
    ( MonadAsync, async, concurrently_, forConcurrently_ )
import Control.Monad.Class.MonadSTM
    ( MonadSTM, atomically )
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
    ( StateT, evalStateT, execStateT, runStateT, state )
import Control.Tracer
    ( Tracer (..), contramap, traceWith )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Generics.Labels
    ()
import Data.List
    ( nubBy )
import Data.Map.Strict
    ( Map )
import Data.Ratio
    ( (%) )
import Data.Text
    ( Text )
import Data.Time.Clock
    ( DiffTime, picosecondsToDiffTime )
import GHC.Generics
    ( Generic )
import Safe
    ( readMay )
import System.Random
    ( StdGen, mkStdGen, randomR )

import qualified Control.Monad.IOSim as IOSim
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Hydra.Tail.Simulation.MockTx
    ( MockTx (..), defaultTxAmount, defaultTxSize, mockTx )
import Hydra.Tail.Simulation.Options
    ( ClientOptions (..)
    , NetworkCapacity (..)
    , PrepareOptions (..)
    , RunOptions (..)
    , ServerOptions (..)
    , kbitsPerSecond
    )
import Hydra.Tail.Simulation.SlotNo
    ( SlotNo (..) )
import Hydra.Tail.Simulation.Utils
    ( foldTraceEvents, forEach, modifyM, updateF, withLabel )
import HydraSim.Analyse
    ( diffTimeToSeconds )
import HydraSim.DelayedComp
    ( DelayedComp, delayedComp, runComp )
import HydraSim.Examples.Channels
    ( AWSCenters (..), channel )
import HydraSim.Multiplexer
    ( Multiplexer, getMessage, newMultiplexer, sendTo, startMultiplexer )
import HydraSim.Multiplexer.Trace
    ( TraceMultiplexer (..) )
import HydraSim.Sized
    ( Size (..), Sized (..) )
import HydraSim.Tx.Class
    ( Tx (..) )
import HydraSim.Types
    ( NodeId (..) )

import qualified HydraSim.Multiplexer as Multiplexer

--
-- Simulation
--

prepareSimulation :: MonadSTM m => PrepareOptions -> m [Event]
prepareSimulation PrepareOptions{clientOptions,numberOfClients,duration} = do
  let clientIds = [1..fromInteger numberOfClients]
  clients <- forM clientIds $ \clientId -> newClient clientId
  let getSubscribers = mkGetSubscribers clients
  let events = foldM
        (\st currentSlot -> (st <>) <$> forEach (stepClient clientOptions getSubscribers currentSlot))
        mempty
        [ i | i <- [ 0 .. duration ] ]
  evalStateT events (Map.fromList $ zip (view #identifier <$> clients) clients)

runSimulation :: RunOptions -> [Event] -> Trace ()
runSimulation RunOptions{serverOptions,slotLength} events = runSimTrace $ do
  let (serverId, clientIds) = (0, [1..fromInteger (getNumberOfClients events)])
  server <- newServer serverId clientIds serverOptions
  clients <- forM clientIds $ \clientId -> do
    client <- newClient clientId
    client <$ connectClient client server
  void $ async $ concurrently_
    (runServer trServer server)
    (forConcurrently_ clients (runClient trClient events serverId slotLength))
  threadDelay 1e99
 where
  tracer :: Tracer (IOSim a) TraceTailSimulation
  tracer = Tracer IOSim.traceM

  trClient :: Tracer (IOSim a) TraceClient
  trClient = contramap TraceClient tracer

  trServer :: Tracer (IOSim a) TraceServer
  trServer = contramap TraceServer tracer

data Analyze = Analyze
  { realThroughput :: Double
    -- ^ Throughput measured from confirmed transactions.
  , maxThroughput :: Double
    -- ^ Throughput measured from total transactions.
  } deriving (Generic, Show)

analyzeSimulation :: RunOptions -> [Event] -> Trace () -> Analyze
analyzeSimulation RunOptions{slotLength} events trace =
  let
    (count, realDuration) = foldTraceEvents fn (0, 0) trace
      where
        fn :: (ThreadLabel, Time, TraceTailSimulation) -> (Integer, DiffTime) -> (Integer, DiffTime)
        fn = \case
          (_threadLabel, Time t, TraceClient (TraceClientMultiplexer (MPRecvTrailing _nodeId AckTx{}))) ->
            (\(!n, !_) -> (n + 1, t))

          _ ->
            id

    realThroughput =
      fromIntegral count / diffTimeToSeconds realDuration
    maxThroughput =
      fromIntegral count / diffTimeToSeconds (durationOf events slotLength)
   in
    Analyze{realThroughput, maxThroughput}

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
  = NewTx !MockTx ![ClientId]
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

  --
  -- ↓↓↓ Server messages ↓↓↓
  --

  | NotifyTx !MockTx
  -- ^ The server will notify concerned clients with transactions they have subscribed to.
  -- How clients subscribe and how the server is keeping track of the subscription is currently
  -- out of scope and will be explored at a later stage.

  | AckTx !(TxRef MockTx)
  -- ^ The server replies to each client submitting a transaction with an acknowledgement.
  deriving (Generic, Show)

instance Sized Msg where
  size = \case
    NewTx tx clients ->
      sizeOfHeader + size tx + sizeOfAddress * fromIntegral (length clients)
    Pull ->
      sizeOfHeader
    Connect{} ->
      0
    Disconnect{} ->
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

data ClientState = Online | Offline
  deriving (Generic, Show)

data Server m = Server
  { multiplexer :: Multiplexer m Msg
  , identifier  :: ServerId
  , region :: AWSCenters
  , options :: ServerOptions
  , clients :: Map ClientId (ClientState, [Msg])
  } deriving (Generic)

newServer
  :: MonadSTM m
  => ServerId
  -> [ClientId]
  -> ServerOptions
  -> m (Server m)
newServer identifier clientIds options@ServerOptions{region,writeCapacity,readCapacity} = do
  multiplexer <- newMultiplexer
    "server"
    outboundBufferSize
    inboundBufferSize
    (capacity writeCapacity)
    (capacity readCapacity)
  return Server { multiplexer, identifier, region, options, clients }
 where
  outboundBufferSize = 1000
  inboundBufferSize = 1000
  clients = Map.fromList [ (clientId, (Offline, [])) | clientId <- clientIds ]

runServer
  :: forall m. (MonadAsync m, MonadTimer m, MonadThrow m)
  => Tracer m TraceServer
  -> Server m
  -> m ()
runServer tracer server0@Server{multiplexer} = do
  concurrently_
    (startMultiplexer (contramap TraceServerMultiplexer tracer) multiplexer)
    (withLabel "Main: Server" $ serverMain server0)
 where
  serverMain :: Server m -> m ()
  serverMain server@Server{clients} = do
    atomically (getMessage multiplexer) >>= \case
      (clientId, NewTx tx subscribers) -> do
        void $ runComp (txValidate Set.empty tx)
        clients' <- flip execStateT clients $ do
          forM_ subscribers $ \subscriber -> do
            lift $ runComp lookupSubscriber
            modifyM $ updateF clientId $ \case
              (Offline, mailbox) -> do
                let msg = NotifyTx tx
                traceWith tracer $ TraceServerStoreInMailbox clientId msg (length mailbox + 1)
                pure $ Just (Offline, msg:mailbox)
              same -> do
                Just same <$ sendTo multiplexer subscriber (NotifyTx tx)
        sendTo multiplexer clientId (AckTx $ txRef tx)
        serverMain $ server { clients = clients' }

      (clientId, Pull) -> do
        clients' <- updateF clientId (\case
          (st, mailbox) -> do
            mapM_ (sendTo multiplexer clientId) (reverse mailbox)
            pure $ Just (st, [])
          ) clients
        serverMain $ server { clients = clients' }

      (clientId, Connect) -> do
        let clients' = Map.update (\(_, mailbox) -> Just (Online, mailbox)) clientId clients
        serverMain $ server { clients = clients' }

      (clientId, Disconnect) -> do
        let clients' = Map.update (\(_, mailbox) -> Just (Offline, mailbox)) clientId clients
        serverMain $ server { clients = clients' }

      (clientId, msg) ->
        throwIO (UnexpectedServerMsg clientId msg)

lookupSubscriber :: DelayedComp ()
lookupSubscriber =
  delayedComp () (picosecondsToDiffTime 500*1e6) -- 500μs

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

data Client m = Client
  { multiplexer :: Multiplexer m Msg
  , identifier  :: ClientId
  , region :: AWSCenters
  , generator :: StdGen
  } deriving (Generic)

newClient :: MonadSTM m => ClientId -> m (Client m)
newClient identifier = do
  multiplexer <- newMultiplexer
    ("client-" <> show (getNodeId identifier))
    outboundBufferSize
    inboundBufferSize
    (capacity $ kbitsPerSecond 512)
    (capacity $ kbitsPerSecond 512)
  return Client { multiplexer, identifier, region, generator }
 where
  outboundBufferSize = 1000
  inboundBufferSize = 1000
  region = LondonAWS
  generator = mkStdGen (getNodeId identifier)

runClient
  :: forall m. (MonadAsync m, MonadTimer m, MonadThrow m)
  => Tracer m TraceClient
  -> [Event]
  -> ServerId
  -> DiffTime
  -> Client m
  -> m ()
runClient tracer events serverId slotLength Client{multiplexer, identifier} = do
  concurrently_
    (startMultiplexer (contramap TraceClientMultiplexer tracer) multiplexer)
    (withLabel ("Main: "<> show identifier) $ clientMain 0 events)
 where
  clientMain :: SlotNo -> [Event] -> m ()
  clientMain currentSlot = \case
    [] ->
      pure ()

    (e:q) | from e /= identifier ->
      clientMain currentSlot q

    (e:q) | slot e <= currentSlot -> do
      sendTo multiplexer serverId (msg e)
      clientMain currentSlot q

    (e:q) -> do
      threadDelay slotLength
      clientMain (currentSlot + 1) (e:q)

stepClient
  :: forall m. (Monad m)
  => ClientOptions
  -> (ClientId -> m [ClientId])
  -> SlotNo
  -> Client m
  -> m ([Event], Client m)
stepClient options getSubscribers currentSlot client@Client{identifier, generator} = do
  (events, generator') <- runStateT step generator
  pure (events, client { generator = generator' })
 where
  step :: StateT StdGen m [Event]
  step = do
    pOnline <- state (randomR (1, 100))
    let online = pOnline % 100 <= options ^. #onlineLikelihood
    pSubmit <- state (randomR (1, 100))
    let submit = online && (pSubmit % 100 <= options ^. #submitLikelihood)
    subscribers <- lift $ getSubscribers identifier
    pure
      [ Event currentSlot identifier msg
      | (predicate, msg) <-
          [ ( online
            , Pull
            )
          , ( submit
            , NewTx (mockTx identifier currentSlot defaultTxAmount defaultTxSize) subscribers
            )
          ]
      , predicate
      ]

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
  , lastSlot :: !SlotNo
  } deriving (Generic, Show)

summarizeEvents :: [Event] -> SimulationSummary
summarizeEvents events = SimulationSummary
  { numberOfClients
  , numberOfEvents
  , lastSlot
  }
 where
  numberOfEvents = toInteger (length events)
  numberOfClients = getNumberOfClients events
  lastSlot = last events ^. #slot

durationOf :: [Event] -> DiffTime -> DiffTime
durationOf events slotLength =
  slotLength * fromIntegral (unSlotNo $ last events ^. #slot)

getNumberOfClients :: [Event] -> Integer
getNumberOfClients =
  toInteger . length . nubBy (\a b -> from a == from b)

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
  Event (SlotNo sl) (NodeId cl) (NewTx (MockTx _ (Size sz) am) rs) ->
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
        <*> (NewTx
          <$> liftM4 mockTx (readClientId cl) (readSlotNo sl) (readAmount am) (readSize sz)
          <*> readRecipients rs
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

  readAmount :: Text -> Maybe Integer
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

connectClient
  :: (MonadAsync m, MonadTimer m, MonadTime m)
  => Client m
  -> Server m -> m ()
connectClient client server =
  Multiplexer.connect
    ( channel (client ^. #region) (server ^. #region) )
    ( client ^. #identifier, client ^. #multiplexer )
    ( server ^. #identifier, server ^. #multiplexer )

-- Simple strategy for now to get subscribers of a particular client;
-- at the moment, the next client in line is considered a subscriber.
mkGetSubscribers
  :: Applicative m
  => [Client m]
  -> ClientId
  -> m [ClientId]
mkGetSubscribers clients (NodeId sender) = do
  let subscriber = NodeId $ max 1 (succ sender `mod` (length clients + 1))
  pure [subscriber]
