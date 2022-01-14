{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Hydra.Tail.Simulation.Analyze (
  Analyze (..),
  Transactions,
  Retries,
  Snapshots,
  NumberOfSubmittedTransactions,
  analyzeSimulation,
  mkAnalyze,
) where

import Prelude

import Control.Monad.Class.MonadTime (
  Time (..),
 )
import Control.Monad.IOSim (
  ThreadLabel,
  Trace (..),
 )
import Data.Functor (
  ($>),
 )
import Data.Generics.Labels ()
import Data.Map.Strict (
  Map,
 )
import qualified Data.Map.Strict as Map
import Data.Maybe (
  mapMaybe,
 )
import Data.Time.Clock (
  DiffTime,
 )
import GHC.Generics (
  Generic,
 )
import Hydra.Tail.Simulation (
  ClientId,
  Msg (..),
  TraceClient (..),
  TraceEventLoop (..),
  TraceServer (..),
  TraceTailSimulation (..),
 )
import Hydra.Tail.Simulation.MockTx (
  MockTx (..),
  TxRef (..),
 )
import Hydra.Tail.Simulation.Options (
  AnalyzeOptions (..),
 )
import Hydra.Tail.Simulation.PaymentWindow (
  Lovelace (..),
 )
import Hydra.Tail.Simulation.SlotNo (
  SlotNo (..),
 )
import Hydra.Tail.Simulation.Utils (
  foldTraceEvents,
 )
import HydraSim.Analyse (
  diffTimeToSeconds,
 )
import HydraSim.Multiplexer.Trace (
  TraceMultiplexer (..),
 )
import HydraSim.Tx.Class (
  Tx (..),
 )
import Quiet (
  Quiet (..),
 )

data Analyze = Analyze
  { numberOfSubmittedTransactions :: Int
  , -- | Number of confirmed transactions within the timespan of the simulation
    numberOfConfirmedTransactions :: Int
  , -- | Number of transactions that have been retried (counting only 1 if a transaction is retried multiple times)
    numberOfRetriedTransactions :: Int
  , numberOfRetriedConfirmedTransactions :: Int
  , -- | Total number of snapshots across ALL clients
    numberOfSnapshots :: Int
  , -- | Average time for a transaction to get 'confirmed'. This includes snapshotting when
    -- relevant.
    averageConfirmationTime :: Milliseconds
  , -- | How many confirmed transactions had been confirmed within one slot, 10 slots.
    percentConfirmedWithin1Slot :: Double
  , percentConfirmedWithin10Slots :: Double
  , -- | A measure of how frequent are snapshots on clients. A high coefficient means
    -- that clients usually "see" a lot of volume before it makes a snapshot. A coefficient
    -- close to 1 means that clients do snapshot for almost every transaction.
    rebalancingCoefficient :: Maybe Double
  }
  deriving (Generic, Show)

type Transactions = Map (TxRef MockTx) [DiffTime]
type Retries = Map (TxRef MockTx) Integer
type Snapshots = Map ClientId [Lovelace]
type NumberOfSubmittedTransactions = Int

newtype Milliseconds = Milliseconds Integer
  deriving stock (Eq, Ord, Generic)
  deriving (Show) via Quiet Milliseconds
  deriving (Read, Num, Enum) via Integer

analyzeSimulation ::
  forall m.
  Monad m =>
  AnalyzeOptions ->
  (SlotNo -> Maybe Analyze -> m ()) ->
  Trace () ->
  m (Transactions, Retries, Snapshots, NumberOfSubmittedTransactions)
analyzeSimulation options notify trace = do
  (confirmations, retries, snapshots, submittedTxs, _) <-
    let fn ::
          (ThreadLabel, Time, TraceTailSimulation) ->
          (Transactions, Retries, Snapshots, NumberOfSubmittedTransactions, SlotNo) ->
          m (Transactions, Retries, Snapshots, NumberOfSubmittedTransactions, SlotNo)
        fn = \case
          (_threadLabel, _, TraceServer (TraceServerMultiplexer (MPRecvTrailing _clientId NewTx{}))) ->
            (\(!m, !r, !sn, !rt, !sl) -> pure (m, r, sn, rt + 1, sl))
          (_threadLabel, _, TraceServer (TraceServerMultiplexer (MPRecvTrailing clientId SnapshotDone))) ->
            (\(!m, !r, !sn, !rt, !sl) -> pure (m, r, Map.adjust (0 :) clientId sn, rt, sl))
          (_threadLabel, _, TraceServer (TraceTransactionBlocked ref)) ->
            ( \(!m, !r, !sn, !rt, !sl) ->
                let countRetry = \case
                      Nothing -> Just 1
                      Just n -> Just (n + 1)
                 in pure
                      ( m
                      , Map.alter countRetry ref r
                      , sn
                      , rt
                      , sl
                      )
            )
          (_threadLabel, _, TraceClient clientId (TraceClientMultiplexer (MPRecvTrailing _nodeId (NotifyTx tx)))) ->
            ( \(!m, !r, !sn, !rt, !sl) -> do
                let countLovelace =
                      Just . \case
                        Just (h : q) -> (h + txAmount tx) : q
                        _ -> [txAmount tx]
                pure (m, r, Map.alter countLovelace clientId sn, rt, sl)
            )
          (_threadLabel, Time t, TraceClient clientId (TraceClientMultiplexer (MPRecvTrailing _nodeId (AckTx tx)))) ->
            ( \(!m, !r, !sn, !rt, !sl) -> do
                let countLovelace =
                      Just . \case
                        Just (h : q) -> (h + txAmount tx) : q
                        _ -> [txAmount tx]
                pure
                  ( Map.update (\ts -> Just (t : ts)) (txRef tx) m
                  , r
                  , Map.alter countLovelace clientId sn
                  , rt
                  , sl
                  )
            )
          (_threadLabel, Time t, TraceEventLoop (TraceEventLoopTxScheduled ref)) ->
            (\(!m, !r, !sn, !rt, !sl) -> pure (Map.insert ref [t] m, r, sn, rt, sl))
          (_threadLabel, _time, TraceEventLoop (TraceEventLoopTick sl')) ->
            ( \(!m, !r, !sn, !rt, !sl) ->
                if sl' > sl
                  then
                    if sl' /= 0 && unSlotNo sl' `mod` 60 == 0
                      then notify sl' (Just $ mkAnalyze options m r sn rt) $> (m, r, sn, rt, sl')
                      else notify sl' Nothing $> (m, r, sn, rt, sl')
                  else pure (m, r, sn, rt, sl)
            )
          _ ->
            pure
     in foldTraceEvents fn (mempty, mempty, mempty, 0, -1) trace
  pure (confirmations, retries, snapshots, submittedTxs)

mkAnalyze ::
  AnalyzeOptions ->
  Transactions ->
  Retries ->
  Snapshots ->
  NumberOfSubmittedTransactions ->
  Analyze
mkAnalyze AnalyzeOptions{discardEdges, paymentWindow} txs retries snapshots numberOfSubmittedTransactions =
  Analyze
    { numberOfConfirmedTransactions
    , numberOfSubmittedTransactions
    , numberOfRetriedTransactions
    , numberOfRetriedConfirmedTransactions
    , numberOfSnapshots
    , averageConfirmationTime
    , percentConfirmedWithin10Slots
    , percentConfirmedWithin1Slot
    , rebalancingCoefficient
    }
 where
  numberOfSnapshots = Map.foldl' (\total xs -> total + length xs - 1) 0 snapshots

  numberOfConfirmedTransactions = length confirmationTimes

  numberOfRetriedTransactions = Map.size retries

  numberOfRetriedConfirmedTransactions = Map.size (retries `Map.restrictKeys` Map.keysSet (Map.filter ((== 2) . length) txs))

  rebalancingCoefficient =
    let w = maybe 1e99 (fromIntegral . unLovelace) paymentWindow
        clients =
          -- NOTE: discarding the first element, because it corresponds to the "ongoing" rebalancing
          -- amount, which may be misleading because it may report a value just after a snapshot (e.g. 0)
          -- and skew the estimation towards a smaller value.
          Map.foldl'
            (\accum xs -> [fromIntegral x / w | Lovelace x <- tail xs] ++ accum)
            []
            snapshots
     in if null clients then Nothing else Just (sum clients / fromIntegral (length clients))

  confirmationTimes =
    mapMaybe (convertConfirmationTime . snd) . maybeDiscardEdges $ Map.toList txs

  maybeDiscardEdges xs = case discardEdges of
    Nothing -> xs
    Just n -> filter (\(TxRef{slot}, _) -> slot > n && slot < (maxSlot - n)) xs

  maxSlot = maximum $ map (\TxRef{slot} -> slot) $ Map.keys txs

  convertConfirmationTime = \case
    [end, start] -> Just . diffTimeToSeconds $ end - start
    _ -> Nothing

  averageConfirmationTime =
    Milliseconds $ round $ 1000 * totalConfirmationTime / fromIntegral numberOfConfirmedTransactions

  totalConfirmationTime = sum confirmationTimes

  percentConfirmedWithin1Slot =
    length (filter (< 1) confirmationTimes) `percentOf` numberOfConfirmedTransactions

  percentConfirmedWithin10Slots =
    length (filter (< 10) confirmationTimes) `percentOf` numberOfConfirmedTransactions

  percentOf a b = (fromIntegral a :: Double) / fromIntegral b
