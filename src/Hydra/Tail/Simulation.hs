{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import Prelude

import Control.Exception
    ( Exception, throw )
import Control.Monad
    ( forM, forM_, forever, void )
import Control.Monad.Class.MonadAsync
    ( MonadAsync, async, concurrently_, forConcurrently_ )
import Control.Monad.Class.MonadFork
    ( MonadThread, labelThread, myThreadId )
import Control.Monad.Class.MonadSTM
    ( MonadSTM, atomically )
import Control.Monad.Class.MonadThrow
    ( MonadThrow, throwIO )
import Control.Monad.Class.MonadTime
    ( MonadTime )
import Control.Monad.Class.MonadTimer
    ( MonadTimer, threadDelay )
import Control.Monad.IOSim
    ( IOSim, ThreadLabel, Trace (..), TraceEvent (..), runSimTrace )
import Control.Tracer
    ( Tracer (..), contramap, traceWith )
import Crypto.Hash.MD5
    ( hash )
import Data.ByteString.Base16
    ( encodeBase16 )
import Data.Dynamic
    ( fromDynamic )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Labels
    ()
import Data.Ratio
    ( (%) )
import Data.Text
    ( Text )
import Data.Time.Clock
    ( DiffTime, picosecondsToDiffTime )
import GHC.Generics
    ( Generic )

import qualified Control.Monad.IOSim as IOSim
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import HydraSim.DelayedComp
    ( delayedComp, runComp )
import HydraSim.Examples.Channels
    ( AWSCenters (..), channel )
import HydraSim.Multiplexer
    ( Multiplexer, getMessage, newMultiplexer, sendTo, startMultiplexer )
import HydraSim.Multiplexer.Trace
    ( TraceMultiplexer )
import HydraSim.Sized
    ( Size (..), Sized (..) )
import HydraSim.Tx.Class
    ( Tx (..) )
import HydraSim.Types
    ( NodeId (..) )

import qualified Data.Set as Set
import qualified HydraSim.Multiplexer as Multiplexer

--
-- Simulation
--

data Options = Options
  { numberOfClients :: Integer
    -- ^ Total number of client
  , slotLength :: DiffTime
    -- ^ Slot length
  , duration :: DiffTime
    -- ^ How long to run the simulation (in simulation's time)
  , serverOptions :: ServerOptions
    -- ^ Options specific to the 'Server'
  , clientOptions :: ClientOptions
    -- ^ Options specific to each 'Client'
  } deriving (Generic)

data ServerOptions = ServerOptions
  { region :: AWSCenters
    -- ^ 'Server' region
  , readCapacity :: Size -> DiffTime
    -- ^ 'Server' network read capacity, in KBits/s
  , writeCapacity :: Size -> DiffTime
    -- ^ 'Server' network write capacity, in KBits/s
  } deriving (Generic)

data ClientOptions = ClientOptions
  { regions :: [AWSCenters]
    -- ^ Regions to spread each 'Client' across uniformly
  , readCapacity :: Size -> DiffTime
    -- ^ Each 'Client' network read capacity, in KBits/s
  , writeCapacity :: Size -> DiffTime
    -- ^ Each 'Client' network write capacity, in KBits/s
  , transactionRate :: Integer
    -- ^ Each 'Client' transaction rate, in transaction/slot
  , onlineLikelyhood  :: Rational
    -- ^ Likelyhood of an offline 'Client' to go online at the current slot.
  , offlineLikelyhood :: Rational
    -- ^ Likelyhood of an online 'Client' to go offline at the current slot.
  } deriving (Generic)

runSimulation :: Options -> Trace ()
runSimulation Options{serverOptions,clientOptions,numberOfClients,slotLength,duration} = runSimTrace $ do
  let serverId = 0
  server <- newServer serverId serverOptions
  clients <- forM [1..fromInteger numberOfClients] $ \clientId -> do
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

main :: IO ()
main = do
  let trace = runSimulation opts
  -- TODO: Analyze the trace
  mapM_ print (reverse $ foldTraceEvents (:) [] trace)
 where
  -- TODO: Get these from a command-line parser
  opts :: Options
  opts = Options
    { duration = 60
    , numberOfClients = 2
    , slotLength = 1

    , serverOptions = ServerOptions
      { region = LondonAWS
      , readCapacity = _KbitsPerSecond (1024*1024)
      , writeCapacity = _KbitsPerSecond (1024*1024)
      }

    , clientOptions = ClientOptions
      { regions = [LondonAWS]
      , readCapacity = _KbitsPerSecond 512
      , writeCapacity = _KbitsPerSecond 512
      , transactionRate = 1
      , onlineLikelyhood = 1%2
      , offlineLikelyhood = 1%2
      }
    }

--
-- Tail-Protocol
--

-- | Messages considered as part of the simplified Tail pre-protocol. We don't know exactly
-- what the Tail protocol hence we have a highly simplified view of it and reduce it to a
-- mere message broker between many producers and many consumers (the clients), linked together
-- via a single message broker (the server).
data Msg
  = NewTx MockTx ClientId [ClientId]
  -- ^ A new transaction, sent to some peer. The current behavior of this simulation
  -- consider that each client is only sending to one single peer. Later, we probably
  -- want to challenge this assumption by analyzing real transaction patterns from the
  -- main chain and model this behavior.

  | NotifyTx MockTx
  -- ^ The server will notify concerned clients with transactions they have subscribed to.
  -- How clients subscribe and how the server is keeping track of the subscription is currently
  -- out of scope and will be explored at a later stage.

  | AckTx (TxRef MockTx)
  -- ^ The server replies to each client submitting a transaction with an acknowledgement.
  deriving (Show)

instance Sized Msg where
  size = \case
    NewTx tx _sender clients ->
      sizeOfHeader + size tx + sizeOfAddress * fromIntegral (length clients)
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
  , options :: ServerOptions
  } deriving (Generic)

newServer
  :: MonadSTM m
  => ServerId
  -> ServerOptions
  -> m (Server m)
newServer identifier options@ServerOptions{region,writeCapacity,readCapacity} = do
  multiplexer <- newMultiplexer
    "server"
    outboundBufferSize
    inboundBufferSize
    writeCapacity
    readCapacity
  return Server { multiplexer, identifier, region, options }
 where
  outboundBufferSize = 1000
  inboundBufferSize = 1000

runServer
  :: forall m. (MonadAsync m, MonadTimer m, MonadThrow m)
  => Tracer m TraceServer
  -> Server m
  -> m ()
runServer tracer Server{multiplexer} = do
  concurrently_
    (startMultiplexer (contramap TraceServerMultiplexer tracer) multiplexer)
    (withLabel "Main: Server" serverMain)
 where
  serverMain :: m ()
  serverMain = forever $ do
    atomically (getMessage multiplexer) >>= \case
      (_, NewTx tx sender subscribers) -> do
        void $ runComp (txValidate Set.empty tx)
        forM_ subscribers $ \subscriber ->
          sendTo multiplexer subscriber (NotifyTx tx)
        sendTo multiplexer sender (AckTx $ txRef tx)

      (nodeId, msg) ->
        throwIO (UnexpectedServerMsg nodeId msg)

data TraceServer
  = TraceServerMultiplexer (TraceMultiplexer Msg)
  deriving (Show)

data ServerMain = ServerMain deriving Show
instance Exception ServerMain


data UnexpectedServerMsg = UnexpectedServerMsg NodeId Msg
  deriving Show
instance Exception UnexpectedServerMsg

--
-- Client
--

type ClientId = NodeId

data Client m = Client
  { multiplexer :: Multiplexer m Msg
  , identifier  :: ClientId
  , region :: AWSCenters
  , options :: ClientOptions
  } deriving (Generic)

newClient :: MonadSTM m => ClientId -> ClientOptions -> m (Client m)
newClient identifier options@ClientOptions{regions,writeCapacity,readCapacity} = do
  multiplexer <- newMultiplexer
    ("client-" <> show (getNodeId identifier))
    outboundBufferSize
    inboundBufferSize
    writeCapacity
    readCapacity
  return Client { multiplexer, identifier, region, options }
 where
  outboundBufferSize = 1000
  inboundBufferSize = 1000
  region = getRegion regions identifier

runClient
  :: forall m. (MonadAsync m, MonadTimer m, MonadThrow m)
  => Tracer m TraceClient
  -> (ClientId -> m [ClientId])
  -> ServerId
  -> DiffTime
  -> Client m
  -> m ()
runClient tracer getSubscribers serverId slotLength Client{multiplexer, identifier, options} =
  concurrently_
    (startMultiplexer (contramap TraceClientMultiplexer tracer) multiplexer)
    (withLabel ("Main: "<> show identifier) $ clientMain 0)
 where
  clientMain :: SlotNo -> m ()
  clientMain currentSlot = do
    traceWith tracer $ TraceClientWakeUp currentSlot
    let n = fromIntegral (options ^. #transactionRate)
    forM_ [1..n] $ \i -> do
      subscribers <- getSubscribers identifier
      let msg = NewTx (mockTx identifier currentSlot i) identifier subscribers
      sendTo multiplexer serverId msg
    threadDelay slotLength >> clientMain (succ currentSlot)

data TraceClient
  = TraceClientMultiplexer (TraceMultiplexer Msg)
  | TraceClientWakeUp SlotNo
  deriving (Show)

--
-- SlotNo
--

newtype SlotNo = SlotNo Integer
  deriving stock (Eq, Show)
  deriving (Num, Enum) via Integer

--
-- MockTx
--

data MockTx = MockTx
  { txId :: TxRef MockTx
  , numberOfOutputs :: Integer
  } deriving (Eq, Ord, Show)

instance Tx MockTx where
  newtype TxRef MockTx = TxRef Text
    deriving (Eq, Ord, Show)

  newtype TxInput MockTx = TxInput ()
    deriving (Eq, Ord, Show)

  txRef = txId
  txi _ = Set.empty
  txo _ = Set.empty

  txValidate _ = delayedComp True . validationTime
  txSort = id

instance Sized (TxRef MockTx) where
  size = const 32

instance Sized MockTx where
  size MockTx{numberOfOutputs} =
    150 + fromInteger (70 * numberOfOutputs)

-- TODO: Validation time should vary with the number of outputs?
validationTime
  :: MockTx
  -> DiffTime
validationTime =
  const (picosecondsToDiffTime 4 * 1e8)

mockTx
  :: ClientId
  -> SlotNo
  -> Int
  -> MockTx
mockTx clientId slotNo ix = MockTx
  { txId =
      -- NOTE: Arguably, we could want to keep this unobfuscasted
      -- for debugging purpose. Though putting these elements in
      -- the transaction 'body' might make more sense?
      (show clientId <> show slotNo <> show ix)
      & encodeBase16 . hash . T.encodeUtf8 . T.pack
      & TxRef
  , numberOfOutputs =
      1
  }

--
-- Helpers
--

getRegion
  :: [AWSCenters]
  -> ClientId
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

withLabel
  :: MonadThread m
  => String
  -> m ()
  -> m ()
withLabel lbl action = do
  myThreadId >>= (`labelThread` lbl)
  action

foldTraceEvents
  :: ((ThreadLabel, TraceTailSimulation) -> st -> st)
  -> st
  -> Trace a
  -> st
foldTraceEvents fn st = \case
  Trace _time threadId mThreadLabel (EventLog event) next ->
    let
      st' = case (fromDynamic event, mThreadLabel) of
        (Just traceSimulation, Nothing) ->
          error $ "unlabeled thread " <> show threadId <> " in " <> show traceSimulation
        (Just traceSimulation, Just threadLabel) ->
          fn (threadLabel, traceSimulation) st
        (Nothing, _) ->
          st
     in
      foldTraceEvents fn st' next
  Trace _time _threadId _threadLabel (EventThrow e) _next ->
    throw e
  Trace _time _threadId _threadLabel _event next ->
    foldTraceEvents fn st next
  TraceMainReturn{} ->
    st
  TraceMainException _ e _ ->
    throw e
  TraceDeadlock{} ->
    st

_KbitsPerSecond :: Integer -> (Size -> DiffTime)
_KbitsPerSecond rate (Size bytes) =
  fromIntegral bytes * fromRational (recip $ (1024 * toRational rate) / 8)
