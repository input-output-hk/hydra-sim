module Main where

import Control.Exception (throw)
import Control.Monad (void)
import Data.Dynamic
import Data.Time.Clock (picosecondsToDiffTime)

-- imports from io-sim, io-sim-classes, contra-tracer
import Control.Tracer
import Control.Monad.IOSim
import Control.Monad.Class.MonadTime
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadTimer
import Control.Monad.Class.MonadSay

-- imports from this package
import Channel
import HeadNode
import HeadNode.Types
import MSig.Mock
import Tx.Mock

dynamicTracer :: Typeable a => Tracer (SimM s) a
dynamicTracer = Tracer traceM

selectTraceHydraEvents
  :: Trace a -> [(Time, ThreadId (SimM s), TraceHydraEvent MockTx)]
selectTraceHydraEvents = go
  where
    go (Trace t tid _ (EventLog e) trace)
     | Just x <- fromDynamic e    = (t,tid,x) : go trace
    go (Trace _ _ _ _ trace)      =         go trace
    go (TraceMainException _ e _) = throw e
    go (TraceDeadlock      _   _) = [] -- expected result in many cases
    go (TraceMainReturn    _ _ _) = []

main :: IO ()
main = do
  let tracer = dynamicTracer
  let trace = runSimTrace (twoNodesExample tracer)
  putStrLn "full trace: "
  print trace
  putStrLn "trace of TraceProtocolEvent:"
  print $ selectTraceHydraEvents trace


twoNodesExample :: (MonadTimer m, MonadSTM m, MonadSay m, MonadFork m, MonadAsync m)
  => Tracer m (TraceHydraEvent MockTx)
  -> m ()
twoNodesExample tracer = do
  node0 <- newNode (NodeId 0) (SendSingleTx (MockTx (TxId 0) (millisecondsToDiffTime 1))) simpleMsig
  node1 <- newNode (NodeId 1) (SendSingleTx (MockTx (TxId 1) (millisecondsToDiffTime 1))) simpleMsig
  (cha, chb) <- createConnectedBoundedChannels 100
  addPeer node0 (NodeId 1) cha
  addPeer node1 (NodeId 0) chb
  void $ concurrently (startNode tracer node0) (startNode tracer node1)


threeNodesExample :: (MonadTimer m, MonadSTM m, MonadSay m, MonadFork m, MonadAsync m)
  => Tracer m (TraceHydraEvent MockTx)
  -> m ()
threeNodesExample tracer = do
  node0 <- newNode (NodeId 0) (SendSingleTx (MockTx (TxId 0) (millisecondsToDiffTime 1))) simpleMsig
  node1 <- newNode (NodeId 1) (SendSingleTx (MockTx (TxId 1) (millisecondsToDiffTime 1))) simpleMsig
  node2 <- newNode (NodeId 2) (SendSingleTx (MockTx (TxId 2) (millisecondsToDiffTime 1))) simpleMsig
  (ch01, ch10) <- createConnectedBoundedChannels 100
  (ch02, ch20) <- createConnectedBoundedChannels 100
  (ch12, ch21) <- createConnectedBoundedChannels 100
  addPeer node0 (NodeId 1) ch01
  addPeer node0 (NodeId 2) ch02
  addPeer node1 (NodeId 0) ch10
  addPeer node1 (NodeId 2) ch12
  addPeer node2 (NodeId 1) ch21
  addPeer node2 (NodeId 0) ch20
  void $ concurrently (startNode tracer node0) $
    concurrently (startNode tracer node1) (startNode tracer node2)

simpleMsig :: MS MockTx
simpleMsig = MS {
  ms_sig_tx = ms_sign_delayed (millisecondsToDiffTime 2),
  ms_asig_tx = ms_asig_delayed (millisecondsToDiffTime 5),
  ms_verify_tx = ms_verify_delayed (millisecondsToDiffTime 7)
  }

millisecondsToDiffTime :: Integer -> DiffTime
millisecondsToDiffTime = picosecondsToDiffTime . (* 1000000000)
