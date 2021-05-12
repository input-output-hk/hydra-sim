{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Hydra.Tail.Simulation where

import Prelude

import Control.Exception
    ( Exception )
import Control.Monad
    ( forM, forM_, void )
import Control.Monad.Class.MonadAsync
    ( MonadAsync, async, concurrently_, forConcurrently_ )
import Control.Monad.Class.MonadSTM
    ( MonadSTM, atomically )
import Control.Monad.Class.MonadThrow
    ( MonadThrow, throwIO )
import Control.Monad.Class.MonadTime
    ( MonadTime )
import Control.Monad.Class.MonadTimer
    ( MonadTimer, threadDelay )
import Control.Monad.IOSim
    ( IOSim, ThreadLabel, Trace (..), runSimTrace )
import Control.Monad.Trans.State.Strict
    ( execStateT )
import Control.Tracer
    ( Tracer (..), contramap, traceWith )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Labels
    ()
import Data.Map.Strict
    ( Map, (!) )
import Data.Ratio
    ( (%) )
import Data.Time.Clock
    ( DiffTime )
import GHC.Generics
    ( Generic )
import System.Random
    ( StdGen, mkStdGen, randomR )

import qualified Control.Monad.IOSim as IOSim
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Hydra.Tail.Simulation.MockTx
    ( MockTx, mockTx )
import Hydra.Tail.Simulation.Options
    ( ClientOptions (..)
    , NetworkCapacity (..)
    , Options (..)
    , ServerOptions (..)
    )
import Hydra.Tail.Simulation.SlotNo
    ( SlotNo (..) )
import Hydra.Tail.Simulation.Utils
    ( foldTraceEvents, modifyM, updateF, withLabel )
import HydraSim.Analyse
    ( diffTimeToSeconds )
import HydraSim.DelayedComp
    ( runComp )
import HydraSim.Examples.Channels
    ( AWSCenters (..), channel )
import HydraSim.Multiplexer
    ( Multiplexer, getMessage, newMultiplexer, sendTo, startMultiplexer )
import HydraSim.Multiplexer.Trace
    ( TraceMultiplexer (..) )
import HydraSim.Sized
    ( Sized (..) )
import HydraSim.Tx.Class
    ( Tx (..) )
import HydraSim.Types
    ( NodeId (..) )

import qualified HydraSim.Multiplexer as Multiplexer

--
-- Simulation
--

runSimulation :: Options -> Trace ()
runSimulation Options{serverOptions,clientOptions,numberOfClients,slotLength,duration} = runSimTrace $ do
  let (serverId, clientIds) = (0, [1..fromInteger numberOfClients])
  server <- newServer serverId clientIds serverOptions
  clients <- forM clientIds $ \clientId -> do
    client <- newClient clientId clientOptions
    client <$ connectClient client server
  void $ async $ concurrently_
    (runServer trServer server)
    (forConcurrently_ clients (runClient trClient (mkGetSubscribers clients) serverId slotLength))
  threadDelay duration
 where
  tracer :: Tracer (IOSim a) TraceTailSimulation
  tracer = Tracer IOSim.traceM

  trClient :: Tracer (IOSim a) TraceClient
  trClient = contramap TraceClient tracer

  trServer :: Tracer (IOSim a) TraceServer
  trServer = contramap TraceServer tracer

data Event
  = ENewTx
  | EAckTx
  deriving (Generic, Eq, Ord, Enum, Bounded)

data Analyze = Analyze
  { totalTransactions :: Integer
    -- ^ Total transactions sent by all clients.
  , confirmedTransactions  :: Integer
    -- ^ Total transactions acknowledged by the server and fully received by clients.
  , realThroughput :: Double
    -- ^ Throughput measured from confirmed transactions.
  , maxThroughput :: Double
    -- ^ Throughput measured from total transactions.
  } deriving (Generic, Show)

analyzeSimulation :: Options -> Trace () -> Analyze
analyzeSimulation Options{duration} trace =
  let
    events = foldTraceEvents fn zero trace
      where
        zero :: Map Event Integer
        zero = Map.fromList ((,0) <$> [minBound .. maxBound])

        fn :: (ThreadLabel, TraceTailSimulation) -> Map Event Integer -> Map Event Integer
        fn = \case
          (_threadLabel, TraceClient (TraceClientMultiplexer (MPSendTrailing _nodeId NewTx{}))) ->
            Map.adjust (+1) ENewTx

          (_threadLabel, TraceClient (TraceClientMultiplexer (MPRecvTrailing _nodeId AckTx{}))) ->
            Map.adjust (+1) EAckTx

          _ ->
            id

    totalTransactions =
      events ! ENewTx
    confirmedTransactions =
      events ! EAckTx
    realThroughput =
      fromIntegral confirmedTransactions / diffTimeToSeconds duration
    maxThroughput =
      fromIntegral totalTransactions / diffTimeToSeconds duration
   in
    Analyze{totalTransactions, confirmedTransactions, realThroughput, maxThroughput}

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
  = NewTx MockTx [ClientId]
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

  | NotifyTx MockTx
  -- ^ The server will notify concerned clients with transactions they have subscribed to.
  -- How clients subscribe and how the server is keeping track of the subscription is currently
  -- out of scope and will be explored at a later stage.

  | AckTx (TxRef MockTx)
  -- ^ The server replies to each client submitting a transaction with an acknowledgement.
  deriving (Show)

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
          forM_ subscribers $ \subscriber -> modifyM $ updateF clientId $ \case
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
  , options :: ClientOptions
  , generator :: StdGen
  } deriving (Generic)

newClient :: MonadSTM m => ClientId -> ClientOptions -> m (Client m)
newClient identifier options@ClientOptions{regions,writeCapacity,readCapacity} = do
  multiplexer <- newMultiplexer
    ("client-" <> show (getNodeId identifier))
    outboundBufferSize
    inboundBufferSize
    (capacity writeCapacity)
    (capacity readCapacity)
  return Client { multiplexer, identifier, region, options, generator }
 where
  outboundBufferSize = 1000
  inboundBufferSize = 1000
  region = getRegion regions identifier
  generator = mkStdGen (getNodeId identifier)

runClient
  :: forall m. (MonadAsync m, MonadTimer m, MonadThrow m)
  => Tracer m TraceClient
  -> (ClientId -> m [ClientId])
  -> ServerId
  -> DiffTime
  -> Client m
  -> m ()
runClient tracer getSubscribers serverId slotLength Client{multiplexer, identifier, options, generator} =
  concurrently_
    (startMultiplexer (contramap TraceClientMultiplexer tracer) multiplexer)
    (withLabel ("Main: "<> show identifier) $ clientMain 0 generator)
 where
  clientMain :: SlotNo -> StdGen -> m ()
  clientMain currentSlot (randomR (1, 100) -> (pOnline, g))
    | (pOnline % 100) <= options ^. #onlineLikelyhood = do
        traceWith tracer $ TraceClientWakeUp currentSlot
        sendTo multiplexer serverId Pull
        let (pSubmit, g') = randomR (1, 100) g
        if (pSubmit % 100) <= options ^. #submitLikelyhood then do
          subscribers <- getSubscribers identifier
          let msg = NewTx (mockTx identifier currentSlot) subscribers
          sendTo multiplexer serverId msg
        else
          pure ()
        idle currentSlot g'

    | otherwise =
        idle currentSlot g

  idle :: SlotNo -> StdGen -> m ()
  idle currentSlot g =
    threadDelay slotLength >> clientMain (succ currentSlot) g

data TraceClient
  = TraceClientMultiplexer (TraceMultiplexer Msg)
  | TraceClientWakeUp SlotNo
  deriving (Show)

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
  let subscriber = NodeId $ max 1 (succ sender `mod` length clients)
  pure [subscriber]
