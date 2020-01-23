{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HydraSim.Analyse
  (ShowDebugMessages (..),
   TxConfirmed (..), SnConfirmed (..),
   dynamicTracer,
   analyseRun,
   nodesInTrace,
   eventsPerNode,
   confirmedTxs,
   confirmedSnapshots,
   txsInConfSnap,
   selectTraceHydraEvents)
where

import           Control.Exception (throw)
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadTime
import           Control.Monad.IOSim
import           Control.Tracer
import           Data.Dynamic
import           Data.List (intercalate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           HydraSim.Multiplexer.Trace
import           HydraSim.Trace
import           HydraSim.Tx.Class
import           HydraSim.Tx.Mock
import           HydraSim.Types

data ShowDebugMessages =
    ShowDebugMessages
  | DontShowDebugMessages
  deriving Eq

data HTrace s = HTrace {
  hTime :: Time,
  hNode :: ThreadLabel,
  hThread :: ThreadId (SimM s),
  hEv :: TraceHydraEvent MockTx
  } deriving Show

selectTraceHydraEvents
  :: ShowDebugMessages
  -> Trace a
  -> [HTrace s] -- [(Time, Maybe ThreadLabel, ThreadId (SimM s), TraceHydraEvent MockTx)]
selectTraceHydraEvents showDebugMessages = go
  where
    go (Trace t tid (Just tlab) (EventLog e) trace)
     | Just (x :: TraceHydraEvent MockTx) <- fromDynamic e    =
         case x of
           HydraDebug _ -> if showDebugMessages == ShowDebugMessages
                           then (HTrace t tlab tid x) : go trace
                           else             go trace
           _ ->                 (HTrace t tlab tid x) : go trace
    go (Trace _t tid Nothing (EventLog e) _trace)
      | Just (x :: TraceHydraEvent MockTx) <- fromDynamic e    =
          error $ "unlabeled thread " ++ show tid ++
          " in " ++ show x
    go (Trace _ _ _ _ trace)      =         go trace
    go (TraceMainException _ e _) = throw e
    go (TraceDeadlock      _   _) = [] -- expected result in many cases
    go (TraceMainReturn    _ _ _) = []

dynamicTracer :: Typeable a => Tracer (SimM s) a
dynamicTracer = Tracer traceM

analyseRun :: Typeable a
  => (forall s. Tracer (SimM s) a -> SimM s ()) -> IO ()
analyseRun run = do
  let fullTrace = runSimTrace (run dynamicTracer)
      trace = selectTraceHydraEvents DontShowDebugMessages fullTrace
      confTxs = confirmedTxs trace
      confSnaps = confirmedSnapshots trace
  -- putStrLn "trace of TraceProtocolEvent:"
  -- mapM_ print trace
  putStrLn "transaction confirmation times:"
  mapM_ print confTxs
  putStrLn "snapshot confirmation times:"
  mapM_ print confSnaps
  let totalTxs = length confTxs
      txsInSnaps = sum $ txsInConfSnap <$> confSnaps
  if (totalTxs == txsInSnaps)
    then putStrLn $ "All " ++ show totalTxs ++ " confirmed txs were included in snapshots"
    else putStrLn $ "Only " ++ show txsInSnaps ++ " of "
                 ++ show totalTxs ++ " confirmed txs were included in snapshots"
  putStrLn $ intercalate " "
    ["There were", show (length (filter (\tx -> case tx of
                                            TxConfirmed _ _ -> False
                                            TxUnconfirmed _ -> True
                                        ) confTxs)),
      "unconfirmed transactions."]
  putStrLn $ intercalate " "
    ["There were", show (length (filter (\tx -> case tx of
                                            SnConfirmed _ _ _ -> False
                                            SnUnconfirmed _ _ -> True
                                        ) confSnaps)),
      "unconfirmed snapshots."]

nodesInTrace :: [HTrace s] -> Set ThreadLabel
nodesInTrace = (Set.delete "main") . Set.fromList . map hNode

eventsPerNode :: [HTrace s] -> Map ThreadLabel ([HTrace s])
eventsPerNode ts =
  let nodes = Set.toList $ nodesInTrace ts
  in Map.fromList [ (node, filter (((==node) . hNode)) ts) | node <- nodes ]

data TxConfirmed =
  -- | Transaction, created at time t, got confirmed at time deltaT
    TxConfirmed Time DiffTime
  -- | Transaction, created at time t, was not confirmed
  | TxUnconfirmed Time
  deriving (Eq, Show)

data SnConfirmed =
  -- | Snapshot with n txs, created at time t, got confirmed at time deltaT
  SnConfirmed Int Time DiffTime
  -- | Snapshot with n txs, created at time t, was not confirmed
  | SnUnconfirmed Int Time
  deriving (Eq, Show)

txsInConfSnap :: SnConfirmed -> Int
txsInConfSnap (SnConfirmed n _ _) = n
txsInConfSnap (SnUnconfirmed _ _) = 0

confirmedTxs :: [HTrace s] -> [TxConfirmed]
confirmedTxs ts0 = go ts []
  where
    -- This is quadratic in the trace length, so let's reduce it to the events
    -- that are relevant before analysing
    ts = filter (\trace -> case hEv trace of
                    HydraMessage (MPSendSelf _node (New _tx)) -> True
                    HydraProtocol (TPTxConf _) -> True
                    _ -> False) ts0
    go [] acc = reverse acc
    go (trace:traces) acc = case hEv trace of
      HydraMessage (MPSendSelf _node (New tx)) ->
        let t = hTime trace
            node = hNode trace
        in case getConfTime node tx traces of
          Nothing -> go traces (TxUnconfirmed t:acc)
          Just t' -> go traces (TxConfirmed t (t' `diffTime` t):acc)
      _ -> go traces acc
    getConfTime _node _tx [] = Nothing
    getConfTime node tx (trace:traces)
      |    hEv trace == HydraProtocol (TPTxConf (txRef tx))
        && hNode trace == node =
          Just $ hTime trace
      | otherwise = getConfTime node tx traces

confirmedSnapshots :: [HTrace s] -> [SnConfirmed]
confirmedSnapshots ts0 = go ts []
  where
    ts = ts0 -- filter (\trace -> case hEv trace of
             --        HydraMessage (MPMulticast (SigReqSn _sn _txs)) -> True
             --        HydraProtocol (TPSnConf _) -> True
             --        _ -> False) ts0
    go [] acc = reverse acc
    go (trace:traces) acc = case hEv trace of
      HydraMessage (MPMulticast (SigReqSn sn txs)) ->
        let t = hTime trace
            node = hNode trace
            nTxs = length txs
        in case getConfTime node sn traces of
          Nothing -> go traces (SnUnconfirmed nTxs t:acc)
          Just t' -> go traces (SnConfirmed nTxs t (t' `diffTime` t):acc)
      _ -> go traces acc
    getConfTime _node _tx [] = Nothing
    getConfTime node sn (trace:traces)
      |    hEv trace == HydraProtocol (TPSnConf sn)
        && hNode trace == node =
          Just $ hTime trace
      | otherwise = getConfTime node sn traces
