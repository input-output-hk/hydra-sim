{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HydraSim.Analyse (
  ShowDebugMessages (..),
  TxConfirmed (..),
  SnConfirmed (..),
  dynamicTracer,
  analyseRun,
  nodesInTrace,
  eventsPerNode,
  confirmedTxs,
  confirmedSnapshots,
  txsInConfSnap,
  selectTraceHydraEvents,
  tps,
  diffTimeToSeconds,
  avgConfTime,
  avgSnapSize,
) where

import Control.Exception (throw)
import Control.Monad (when)
import Control.Monad.Class.MonadFork (MonadThread (ThreadId))
import Control.Monad.Class.MonadTime.SI (Time (..), addTime, diffTime)
import Control.Monad.IOSim (
  IOSim,
  SimEvent,
  SimEventType (EventLog),
  SimResult,
  ThreadLabel,
  Trace (
    SimTrace,
    TraceDeadlock,
    TraceMainException,
    TraceMainReturn
  ),
  traceM,
 )
import Control.Tracer (Tracer (Tracer), emit)
import Data.Dynamic (Typeable, fromDynamic)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds)
import HydraSim.Multiplexer.Trace (
  TraceMultiplexer (MPMulticast, MPSendSelf),
 )
import HydraSim.Trace (
  TraceHydraEvent (HydraDebug, HydraMessage, HydraProtocol),
 )
import HydraSim.Tx.Class (Tx (txRef))
import HydraSim.Tx.Mock (MockTx)
import HydraSim.Types (
  HeadProtocol (New, SigReqSn),
  TraceProtocolEvent (TPSnConf, TPTxConf),
 )
import Numeric.Natural (Natural)
import System.Console.ANSI (
  Color (Red),
  ColorIntensity (Vivid),
  ConsoleLayer (Foreground),
  SGR (Reset, SetColor),
  setSGR,
 )

data ShowDebugMessages
  = ShowDebugMessages
  | DontShowDebugMessages
  deriving (Eq)

data HTrace s = HTrace
  { hTime :: Time
  , hNode :: ThreadLabel
  , hThread :: ThreadId (IOSim s)
  , hEv :: TraceHydraEvent MockTx
  }
  deriving (Show)

selectTraceHydraEvents ::
  ShowDebugMessages ->
  Trace (SimResult s) SimEvent ->
  [HTrace s] -- [(Time, Maybe ThreadLabel, ThreadId (SimM s), TraceHydraEvent MockTx)]
selectTraceHydraEvents showDebugMessages = go
 where
  go (SimTrace t tid (Just tlab) (EventLog e) trace)
    | Just (x :: TraceHydraEvent MockTx) <- fromDynamic e =
        case x of
          HydraDebug _ ->
            if showDebugMessages == ShowDebugMessages
              then HTrace t tlab tid x : go trace
              else go trace
          _ -> HTrace t tlab tid x : go trace
  go (SimTrace _t tid Nothing (EventLog e) _trace)
    | Just (x :: TraceHydraEvent MockTx) <- fromDynamic e =
        error $
          "unlabeled thread "
            ++ show tid
            ++ " in "
            ++ show x
  go (SimTrace _ _ _ _ trace) = go trace
  go (TraceMainException _ e _) = throw e
  go (TraceDeadlock _ _) = [] -- expected result in many cases
  go TraceMainReturn{} = []
  go _ = []

dynamicTracer :: Typeable a => Tracer (IOSim s) a
dynamicTracer = Tracer $ emit traceM

analyseRun ::
  -- | verbosity level
  Natural ->
  Trace (SimResult s) SimEvent ->
  IO ([TxConfirmed], [SnConfirmed])
analyseRun verbosity fullTrace = do
  let trace =
        selectTraceHydraEvents
          (if verbosity < 4 then DontShowDebugMessages else ShowDebugMessages)
          fullTrace
      confTxs = confirmedTxs trace
      confSnaps = confirmedSnapshots trace
  when (verbosity >= 3) $ do
    putStrLn "trace of TraceProtocolEvent:"
    mapM_ print trace
  when (verbosity >= 2) $ do
    putStrLn "transaction confirmation times:"
    mapM_ print confTxs
    putStrLn "snapshot confirmation times:"
    mapM_ print confSnaps
  let totalTxs = length confTxs
      totalSnaps = length confSnaps
      txsInSnaps = sum $ txsInConfSnap <$> confSnaps
      nUnconfirmedTxs =
        length $
          filter
            ( \case
                TxConfirmed{} -> False
                TxUnconfirmed _ _ -> True
            )
            confTxs
      nUnconfirmedSnaps =
        length $
          filter
            ( \case
                SnConfirmed{} -> False
                SnUnconfirmed{} -> True
            )
            confSnaps
  when (totalTxs /= txsInSnaps) $
    warn $
      unwords
        ["Only", show txsInSnaps, "of", show totalTxs, " txs were included in snapshots"]
  when (nUnconfirmedTxs > 0) $
    warn $
      unwords
        ["There were", show nUnconfirmedTxs, "unconfirmed transactions."]
  when (nUnconfirmedSnaps > 0) $
    warn $
      unwords
        ["There were", show nUnconfirmedSnaps, "unconfirmed snapshots."]
  when (verbosity > 0) $ do
    putStrLn $ "Processed " ++ show totalTxs ++ " transactions."
    putStrLn $ "Made " ++ show totalSnaps ++ " snapshots."
    putStrLn $ "Transaction throughput (tx per second): " ++ show (tps confTxs)
    putStrLn $ "Snapshot throughput (tx in snaps per second): " ++ show (tpsSnap confSnaps)
    putStrLn $ "Average tx confirmation time: " ++ show (avgConfTime confTxs)
    putStrLn $ "Average snapshot size: " ++ show (avgSnapSize confSnaps)
  return (confTxs, confSnaps)
 where
  warn s = setSGR [SetColor Foreground Vivid Red] >> putStrLn s >> setSGR [Reset]

nodesInTrace :: [HTrace s] -> Set ThreadLabel
nodesInTrace = Set.delete "main" . Set.fromList . map hNode

eventsPerNode :: [HTrace s] -> Map ThreadLabel [HTrace s]
eventsPerNode ts =
  let nodes = Set.toList $ nodesInTrace ts
   in Map.fromList [(node, filter ((== node) . hNode) ts) | node <- nodes]

data TxConfirmed
  = -- | Transaction, created at time t, got confirmed at time deltaT
    TxConfirmed ThreadLabel Time DiffTime
  | -- | Transaction, created at time t, was not confirmed
    TxUnconfirmed ThreadLabel Time
  deriving (Eq, Show)

data SnConfirmed
  = -- | Snapshot with n txs, created at time t, got confirmed at time deltaT
    SnConfirmed ThreadLabel Int Time DiffTime
  | -- | Snapshot with n txs, created at time t, was not confirmed
    SnUnconfirmed ThreadLabel Int Time
  deriving (Eq, Show)

txsInConfSnap :: SnConfirmed -> Int
txsInConfSnap (SnConfirmed _ n _ _) = n
txsInConfSnap SnUnconfirmed{} = 0

tps :: [TxConfirmed] -> Double
tps txs0 = tps' (filterConfirmedTxs txs0)
 where
  tps' [] = 0
  tps' txs =
    let Time endTime = maximum [dt `addTime` t | TxConfirmed _ t dt <- txs]
     in fromIntegral (length txs) / diffTimeToSeconds endTime

-- |Computes the number of transactions per second confirmed via snapshots
tpsSnap :: [SnConfirmed] -> Double
tpsSnap sns0 = tps' (filterConfirmedSnaps sns0)
 where
  tps' [] = 0
  tps' sns =
    let Time endTime = maximum [dt `addTime` t | SnConfirmed _ _ t dt <- sns]
     in fromIntegral (sum $ map numTxSnapshot sns) / diffTimeToSeconds endTime

numTxSnapshot :: SnConfirmed -> Int
numTxSnapshot (SnConfirmed _ i _ _) = i
numTxSnapshot SnUnconfirmed{} = 0

avgConfTime :: [TxConfirmed] -> DiffTime
avgConfTime txs0 = average (map getConfTime txs)
 where
  getConfTime (TxConfirmed _ _ dt) = dt
  getConfTime (TxUnconfirmed _ _) = error "Unconfirmed tx in avgConfTime"
  txs = filterConfirmedTxs txs0

avgSnapSize :: [SnConfirmed] -> Double
avgSnapSize sns0 =
  average (map getSnapSize sns)
 where
  getSnapSize (SnConfirmed _ n _ _) = fromIntegral n
  getSnapSize SnUnconfirmed{} = error "Unconfirmed snapshot in avgSnapSize"
  sns = filterConfirmedSnaps sns0

filterConfirmedSnaps :: [SnConfirmed] -> [SnConfirmed]
filterConfirmedSnaps =
  filter
    ( \case
        SnConfirmed{} -> True
        SnUnconfirmed{} -> False
    )

average :: Fractional p => [p] -> p
average [] = 0
average l = sum l / fromIntegral (length l)

filterConfirmedTxs :: [TxConfirmed] -> [TxConfirmed]
filterConfirmedTxs =
  filter
    ( \case
        TxConfirmed{} -> True
        TxUnconfirmed _ _ -> False
    )

diffTimeToSeconds :: DiffTime -> Double
diffTimeToSeconds t = 1e-12 * fromInteger (diffTimeToPicoseconds t)

confirmedTxs :: [HTrace s] -> [TxConfirmed]
confirmedTxs ts0 = go ts []
 where
  -- This is quadratic in the trace length, so let's reduce it to the events
  -- that are relevant before analysing
  ts =
    filter
      ( \trace -> case hEv trace of
          HydraMessage (MPSendSelf _node (New _tx)) -> True
          HydraProtocol (TPTxConf _) -> True
          _ -> False
      )
      ts0
  go [] acc = reverse acc
  go (trace : traces) acc = case hEv trace of
    HydraMessage (MPSendSelf _node (New tx)) ->
      let t = hTime trace
          node = hNode trace
       in case getConfTime node tx traces of
            Nothing -> go traces (TxUnconfirmed node t : acc)
            Just t' -> go traces (TxConfirmed node t (t' `diffTime` t) : acc)
    _ -> go traces acc
  getConfTime _node _tx [] = Nothing
  getConfTime node tx (trace : traces)
    | hEv trace == HydraProtocol (TPTxConf (txRef tx))
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
  go (trace : traces) acc = case hEv trace of
    HydraMessage (MPMulticast (SigReqSn sn txs)) ->
      let t = hTime trace
          node = hNode trace
          nTxs = length txs
       in case getConfTime node sn traces of
            Nothing -> go traces (SnUnconfirmed node nTxs t : acc)
            Just t' -> go traces (SnConfirmed node nTxs t (t' `diffTime` t) : acc)
    _ -> go traces acc
  getConfTime _node _tx [] = Nothing
  getConfTime node sn (trace : traces)
    | hEv trace == HydraProtocol (TPSnConf sn)
        && hNode trace == node =
        Just $ hTime trace
    | otherwise = getConfTime node sn traces
