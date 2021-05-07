{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import Prelude

import Control.Exception
    ( throw )
import Control.Monad
    ( forM, forever, replicateM_, void )
import Control.Monad.Class.MonadAsync
    ( MonadAsync, async, concurrently_, forConcurrently_ )
import Control.Monad.Class.MonadSTM
    ( MonadSTM )
import Control.Monad.Class.MonadThrow
    ( MonadThrow )
import Control.Monad.Class.MonadTime
    ( MonadTime )
import Control.Monad.Class.MonadTimer
    ( MonadTimer, threadDelay )
import Control.Monad.IOSim
    ( IOSim, ThreadLabel, Trace (..), TraceEvent (..), runSimTrace )
import Control.Tracer
    ( Tracer (..), contramap )
import Data.Dynamic
    ( fromDynamic )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Labels
    ()
import Data.Ratio
    ( (%) )
import Data.Time.Clock
    ( DiffTime )
import GHC.Generics
    ( Generic )

import qualified Control.Monad.IOSim as IOSim

import HydraSim.Examples.Channels
    ( AWSCenters (..), channel )
import HydraSim.Multiplexer
    ( Multiplexer, newMultiplexer, sendTo, startMultiplexer )
import HydraSim.Multiplexer.Trace
    ( TraceMultiplexer )
import HydraSim.Sized
    ( Size (..), Sized (..) )
import HydraSim.Types
    ( NodeId (..) )

import qualified HydraSim.Multiplexer as Multiplexer

--
-- Simulation
--

data Options = Options
  { numberOfClients :: Integer
    -- ^ Total number of client.
  , slotLength :: DiffTime
    -- ^ Slot length
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
runSimulation Options{serverOptions,clientOptions,numberOfClients,slotLength} = runSimTrace $ do
  let serverId = 0
  server <- newServer serverId serverOptions
  clients <- forM [1..fromInteger numberOfClients] $ \clientId -> do
    client <- newClient clientId clientOptions
    client <$ connectClient client server
  void $ async $ concurrently_
    (runServer trServer server)
    (forConcurrently_ clients (runClient trClient serverId slotLength))
  threadDelay 1e2
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
  mapM_ print (foldTraceEvents (:) [] trace)
 where
  -- TODO: Get these from a command-line parser
  opts :: Options
  opts = Options
    { numberOfClients = 2
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


data Msg = Msg
  deriving (Show)

instance Sized Msg where
  size = \case
    Msg -> 100

data TraceTailSimulation
  = TraceServer TraceServer
  | TraceClient TraceClient
  deriving (Show)

--
-- Client
--

data Client m = Client
  { multiplexer :: Multiplexer m Msg
  , identifier  :: NodeId
  , region :: AWSCenters
  , options :: ClientOptions
  } deriving (Generic)

newClient :: MonadSTM m => NodeId -> ClientOptions -> m (Client m)
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
  -> NodeId
  -> DiffTime
  -> Client m
  -> m ()
runClient tracer serverId slotLength Client{multiplexer, options} =
  concurrently_
    (startMultiplexer (contramap TraceClientMultiplexer tracer) multiplexer)
    clientMain
 where
  clientMain :: m ()
  clientMain = forever $ do
    let n = fromIntegral (options ^. #transactionRate)
    replicateM_ n $ sendTo multiplexer serverId Msg
    threadDelay slotLength

data TraceClient
  = TraceClientMultiplexer (TraceMultiplexer Msg)
  deriving (Show)

--
-- Server
--

data Server m = Server
  { multiplexer :: Multiplexer m Msg
  , identifier  :: NodeId
  , region :: AWSCenters
  , options :: ServerOptions
  } deriving (Generic)

newServer
  :: MonadSTM m
  => NodeId
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
  :: (MonadAsync m, MonadTimer m, MonadThrow m)
  => Tracer m TraceServer
  -> Server m
  -> m ()
runServer tracer Server{multiplexer} = do
  startMultiplexer (contramap TraceServerMultiplexer tracer) multiplexer

data TraceServer
  = TraceServerMultiplexer (TraceMultiplexer Msg)
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
