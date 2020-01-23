{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (throw)
import Control.Monad (void)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadTime
import Control.Monad.Class.MonadTimer
import Control.Monad.Class.MonadThrow
import Control.Monad.IOSim
import Control.Tracer
import Data.Dynamic
import Data.Time.Clock (picosecondsToDiffTime)
import HydraSim.Channel
import HydraSim.HeadNode
import HydraSim.MSig.Mock
import HydraSim.Sized
import HydraSim.Trace
import HydraSim.Tx.Mock
import HydraSim.Types
import System.Random (RandomGen, mkStdGen, split)

dynamicTracer :: Typeable a => Tracer (SimM s) a
dynamicTracer = Tracer traceM

data ShowDebugMessages =
    ShowDebugMessages
  | DontShowDebugMessages
  deriving Eq

selectTraceHydraEvents
  :: ShowDebugMessages
  -> Trace a
  -> [(Time, Maybe ThreadLabel, ThreadId (SimM s), TraceHydraEvent MockTx)]
selectTraceHydraEvents showDebugMessages = go
  where
    go (Trace t tid tlab (EventLog e) trace)
     | Just (x :: TraceHydraEvent MockTx) <- fromDynamic e    =
         case x of
           HydraDebug _ -> if showDebugMessages == ShowDebugMessages
                           then (t,tlab,tid,x) : go trace
                           else             go trace
           _ ->                 (t,tlab,tid,x) : go trace
    go (Trace _ _ _ _ trace)      =         go trace
    go (TraceMainException _ e _) = throw e
    go (TraceDeadlock      _   _) = [] -- expected result in many cases
    go (TraceMainReturn    _ _ _) = []

main :: IO ()
main = do
  let tracer = dynamicTracer
  let trace = runSimTrace (twoNodesExample tracer)
  -- putStrLn "full trace: "
  -- print trace
  putStrLn "trace of TraceProtocolEvent:"
  mapM_ print $ selectTraceHydraEvents DontShowDebugMessages trace


twoNodesExample :: (MonadTimer m, MonadSTM m, MonadSay m, MonadFork m, MonadAsync m,
                   MonadThrow m, MonadTime m)
  => Tracer m (TraceHydraEvent MockTx)
  -> m ()
twoNodesExample tracer = do
  node0 <- newNode (simpleNodeConf 2 0 10) (mBytePerSecond 10) (mBytePerSecond 10)
  node1 <- newNode (simpleNodeConf 2 1 10) (mBytePerSecond 10) (mBytePerSecond 10)
  connectNodes (delayedChannels prng) node0 node1
  void $ async $ concurrently (startNode tracer node0) (startNode tracer node1)
  threadDelay (millisecondsToDiffTime $ 1000 * 60 * 60) -- wait an hour
  traceState tracer node0
  traceState tracer node1
  where
    prng = mkStdGen 42

threeNodesExample :: (MonadTimer m, MonadSTM m, MonadSay m, MonadFork m, MonadAsync m,
                     MonadThrow m, MonadTime m)
  => Tracer m (TraceHydraEvent MockTx)
  -> m ()
threeNodesExample tracer = do
  node0 <- newNode (simpleNodeConf 3 0 100) (mBytePerSecond 10) (mBytePerSecond 10)
  node1 <- newNode (simpleNodeConf 3 1 100) (mBytePerSecond 10) (mBytePerSecond 10)
  node2 <- newNode (simpleNodeConf 3 2 100) (mBytePerSecond 10) (mBytePerSecond 10)
  connectNodes (delayedChannels prng1) node0 node1
  connectNodes (delayedChannels prng2) node0 node2
  connectNodes (delayedChannels prng3) node1 node2
  void $ async $ concurrently (startNode tracer node0) $
    concurrently (startNode tracer node1) (startNode tracer node2)
  threadDelay (millisecondsToDiffTime $ 1000 * 60 * 60) -- wait an hour
  traceState tracer node0
  traceState tracer node1
  traceState tracer node2
  where
    prng = mkStdGen 42
    (prng1, prng') = split prng
    (prng2, prng3) = split prng'

delayedChannels
  :: (MonadAsync m, MonadSTM m, MonadTimer m, MonadTime m,
      RandomGen prng, Show a)
  => prng -> m (Channel m a, Channel m a)
delayedChannels prng =
  createConnectedDelayChannels
  -- TODO: get realistic GV numbers
  (millisecondsToDiffTime 10, millisecondsToDiffTime 5)
  prng

simpleMsig :: MS MockTx
simpleMsig = MS {
  ms_sig_tx = ms_sign_delayed (millisecondsToDiffTime 2),
  ms_asig_tx = ms_asig_delayed (millisecondsToDiffTime 5),
  ms_verify_tx = ms_verify_delayed (millisecondsToDiffTime 7),

  ms_sig_sn = ms_sign_delayed (millisecondsToDiffTime 2),
  ms_asig_sn = ms_asig_delayed (millisecondsToDiffTime 5),
  ms_verify_sn = ms_verify_delayed (millisecondsToDiffTime 7)
}

-- | Node that sends a bunch of transactions.
--
-- Snapshots are created round-robin, every time there is at least one confirmed
-- transaction.
simpleNodeConf
  :: Int -- ^ Total number of nodes
  -> Int -- ^ This node number
  -> Int -- ^ Number of transactions to send
  -> NodeConf MockTx
simpleNodeConf n i ntx
  | n <= i = error "simpleNodeConf: Node index must be smaller than total number of nodes."
  | otherwise = NodeConf {
      hcNodeId = NodeId i,
      hcTxSendStrategy = SendTxs 2 txs,
      hcMSig = simpleMsig,
      hcLeaderFun = \(SnapN s) -> NodeId (s `mod` n),
      hcSnapshotStrategy = SnapAfterNTxs 1
      }
  where
    -- we make sure that each node sends txs with a unique id.
    txs = [MockTx (TxId $ n * j + i)
           (millisecondsToDiffTime 1) -- TODO: check realistic validation times
           200  -- TODO: check realistic tx sizes
          | j <- [0..ntx-1]]

millisecondsToDiffTime :: Integer -> DiffTime
millisecondsToDiffTime = picosecondsToDiffTime . (* 1000000000)

-- | Data rate of 'n' megabytes per second
mBytePerSecond
  :: Integer
  -- ^ @n@
  -> Size -> DiffTime
mBytePerSecond rate (Size b) = picosecondsToDiffTime $
  (fromIntegral b) * 1000 * 1000 * 1000 * 1000 `div` (1024 * 1024 * rate)
