{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Prelude

import Control.Monad
    ( forM, void )
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
    ( IOSim, Trace (..), TraceEvent (..), runSimTrace )
import Control.Tracer
    ( Tracer (..), contramap )
import Data.Ratio
    ( (%) )
import Data.Time.Clock
    ( DiffTime )

import qualified Control.Monad.IOSim as IOSim

import HydraSim.Examples.Channels
    ( AWSCenters (..), channel )
import HydraSim.Multiplexer
    ( Multiplexer, newMultiplexer, startMultiplexer )
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
  , serverRegion :: AWSCenters
    -- ^ Server region, assuming it is in an AWS center
  , serverReadCapacity :: Size -> DiffTime
    -- ^ Server network read capacity, in KBits/s
  , serverWriteCapacity :: Size -> DiffTime
    -- ^ Server network write capacity, in KBits/s
  , clientTransactionRate :: Integer
    -- ^ Transaction rate, in transaction/slot for each client.
  , onlineLikelyhood  :: Rational
    -- ^ Likelyhood of an offline client to go online at the current slot.
  , offlineLikelyhood :: Rational
    -- ^ Likelyhood of an online client to go offline at the current slot.
  }

runSimulation :: Options -> Trace ()
runSimulation opts = runSimTrace $ do
  server <- newServer 0 serverRegion serverReadCapacity serverWriteCapacity
  clients <- forM [1..fromInteger numberOfClients] $ \clientId -> do
    client <- newClient clientId
    client <$ connectClient client server
  void $ async $ concurrently_
    (runServer (contramap TraceServer tracer) server)
    (forConcurrently_ clients (runClient (contramap TraceClient tracer)))
  threadDelay 1e6
 where
  tracer :: Tracer (IOSim a) TraceTailSimulation
  tracer = Tracer IOSim.traceM

  Options
    { serverReadCapacity
    , serverWriteCapacity
    , serverRegion
    , numberOfClients
    } = opts

main :: IO ()
main = do
  let trace = runSimulation opts
  mapM_ print (foldTraceEvents (:) [] trace)
 where
  -- TODO: Get these from a command-line parser
  opts :: Options
  opts = Options
    { numberOfClients = 2
    , serverRegion = LondonAWS
    , serverReadCapacity = _KbitsPerSecond (1024*1024)
    , serverWriteCapacity = _KbitsPerSecond (1024*1024)
    , clientTransactionRate = 1
    , onlineLikelyhood = 1%2
    , offlineLikelyhood = 1%2
    }

--
-- Tail-Protocol
--


data Msg = Msg

instance Sized Msg where
  size = \case
    Msg -> 100

data TraceTailSimulation
  = TraceServer TraceServer
  | TraceClient TraceClient

--
-- Client
--

data Client m = Client
  { multiplexer :: Multiplexer m Msg
  , identifier  :: NodeId
  , region :: AWSCenters
  }

newClient :: MonadSTM m => NodeId -> m (Client m)
newClient identifier = do
  multiplexer <- newMultiplexer
    ("client-" <> show (getNodeId identifier))
    outboundBufferSize
    inboundBufferSize
    (_KbitsPerSecond 512)
    (_KbitsPerSecond 512)
  return Client { multiplexer, identifier, region }
 where
  outboundBufferSize = 1000
  inboundBufferSize = 1000
  region = LondonAWS -- TODO: Make configurable.. somehow?

runClient
  :: (MonadAsync m, MonadTimer m, MonadThrow m)
  => Tracer m TraceClient
  -> Client m
  -> m ()
runClient tracer Client{multiplexer} =
  startMultiplexer (contramap TraceClientMultiplexer tracer) multiplexer

data TraceClient
  = TraceClientMultiplexer (TraceMultiplexer Msg)

--
-- Server
--

data Server m = Server
  { multiplexer :: Multiplexer m Msg
  , identifier  :: NodeId
  , region :: AWSCenters
  }

newServer
  :: MonadSTM m
  => NodeId
  -> AWSCenters
  -> (Size -> DiffTime)
  -> (Size -> DiffTime)
  -> m (Server m)
newServer identifier region writeCapacity readCapacity = do
  multiplexer <- newMultiplexer
    ("server-" <> show (getNodeId identifier))
    outboundBufferSize
    inboundBufferSize
    writeCapacity
    readCapacity
  return Server { multiplexer, identifier, region }
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

--
-- Helpers
--

connectClient
  :: (MonadAsync m, MonadTimer m, MonadTime m)
  => Client m
  -> Server m -> m ()
connectClient client server =
  Multiplexer.connect
    ( channel
        ((region :: Client m -> AWSCenters) client)
        ((region :: Server m -> AWSCenters) server)
    )
    ( (identifier :: Client m -> NodeId) client
    , (multiplexer :: Client m -> Multiplexer m Msg) client
    )
    ( (identifier :: Server m -> NodeId) server
    , (multiplexer :: Server m -> Multiplexer m Msg) server
    )

foldTraceEvents
  :: (TraceEvent -> st -> st)
  -> st
  -> Trace a
  -> st
foldTraceEvents fn st = \case
  Trace _time _threadId _threadLabel event next ->
    foldTraceEvents fn (fn event st) next
  TraceMainReturn{} ->
    st
  TraceMainException{} ->
    st
  TraceDeadlock{} ->
    st

_KbitsPerSecond :: Integer -> (Size -> DiffTime)
_KbitsPerSecond rate (Size bytes) =
  fromIntegral bytes * fromRational (recip $ (1024 * toRational rate) / 8)
