{-|
Module HydraSim.Examples.Baselines
Description: Baseline scenarios for protocol performance
-}
module HydraSim.Examples.Baselines
  (Baseline (..),
   Concurrency (..),
   Scenario (..),
   tpsTotalBound,
   baselineTPS,
   findIntersection,
   minConfTime
  )
where

import HydraSim.Examples.Nodes
import HydraSim.Examples.Txs
import HydraSim.DelayedComp (unComp)
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds, picosecondsToDiffTime)
import HydraSim.Examples.Channels
import HydraSim.Tx.Mock
import HydraSim.Types
import HydraSim.Sized
import HydraSim.MSig.Mock
import qualified Data.Set as Set

data Scenario = FullTrust | HydraUnlimited deriving (Eq, Show, Read)
data Concurrency = FiniteConc Int | UnlimitedConc deriving (Eq, Show, Read)
type Bandwidth = Integer

-- | Type representing bounds on the transaction rate from different constraints.
data TPSBound = TPSBound {
  tpsCpuBound :: Double,
  tpsBandwidthBound :: Double,
  -- | We only have a bound from latency if we have finite concurrency
  -- (otherwise, we can amortise it completely)
  tpsMLatencyBound :: Maybe Double
  } deriving (Eq, Show)

-- | Get an overall bound on the transaction rate from a 'TPSBound'
tpsTotalBound :: TPSBound -> Double
tpsTotalBound tps = min (tpsConstantBound tps) (tpsBandwidthBound tps)

-- | The part of the bound that does not depend on bandwidth
tpsConstantBound :: TPSBound -> Double
tpsConstantBound (TPSBound cpu _bandwidth Nothing) = cpu
tpsConstantBound (TPSBound cpu _bandwidth (Just latency)) = min cpu latency

-- | Input data for calculating baselines
data Baseline = Baseline {
  blScenario :: Scenario,
  blConc :: Concurrency,
  -- | network capacity, in kbits/s
  blBandwidth :: Bandwidth,
  blLocations :: [AWSCenters],
  blAsigTimes :: (DiffTime, DiffTime, DiffTime),
  -- | We can also handle snapshots in baselines.
  --
  -- Note, however, that @SnapAfter n@ in a baseline is interpreted so that
  -- there will be a snapshot after every @n@ transactions. In general, this
  -- will be more than in a real run. In a run, a node will produce a snapshot
  -- once it has at least @n@ confirmed transactions, and is the current leader.
  blSnapshots :: SnapStrategy,
  blTxType :: Txs
  }


-- | Calculate bound on transaction rate for a given baseline
baselineTPS :: Baseline -> TPSBound
baselineTPS bl = TPSBound cpuBound bandwidthBound mlatencyBound
  where
    allNodes = fromIntegral $ length (blLocations bl)
    otherNodes = allNodes - 1
    (_signTime, _aggregateTime, verifySigTime) = blAsigTimes bl
    cpuBound = case blScenario bl of
      FullTrust ->
        perSecond $ validationTime bl
      HydraUnlimited ->
        perSecond $ validationTime bl + verifySigTime `multiplyDiffTime` (1 + snapFactor)
    bandwidthBound = perSecond $ otherNodes/allNodes * (
      (capacity bl (txMsgSize bl)) + ((capacity bl (snMsgSize bl)) `multiplyDiffTime` snapFactor))
    mlatencyBound = case blConc bl of
      UnlimitedConc -> Nothing
      FiniteConc conc -> Just $
        perSecond ((maximum . map snd $ networkDelays bl)*2/(fromIntegral conc * allNodes))
    snapFactor = case blSnapshots bl of
      NoSnapshots -> 0
      SnapAfter n -> 1 / (fromIntegral n :: Double)

-- | Evaluate TPS bound at suitable supporting points, to plot TPS over bandwidth.
--
-- Since the bound that depends on the bandwidth is linear, and the others are
-- constant, we can calculate the point of intersection.
findIntersection
  :: Baseline
  -> (Bandwidth, Bandwidth) -- ^ lower and upper limit
  -> [(Bandwidth, TPSBound)]
  -- ^ list of evaluation points. Includes lower and upper limit, and
  -- intersection point.
findIntersection bl (lower, upper)
  | lower > upper = findIntersection bl (upper, lower)
  | otherwise =
    let (lowerTPS, upperTPS) = (lowerBl, upperBl)
    in if tpsBandwidthBound lowerBl < tpsConstantBound lowerBl
          && tpsBandwidthBound upperBl > tpsConstantBound upperBl
       then let intersection = tpsConstantBound lowerBl * (fromIntegral lower) / tpsBandwidthBound lowerBl
            in [(lower, lowerTPS), (round intersection, (toBl $ round intersection)), (upper, upperTPS)]
       else [(lower, lowerTPS), (upper, upperTPS)]
  where toBl bw = baselineTPS $ bl {blBandwidth = bw}
        lowerBl = toBl lower
        upperBl = toBl upper

-- | Minimal confirmation time for a transaction.
--
-- We get this by just adding the times for all actions that need to be
-- performed for a transaction to be conofirmed, assuming that every resource is
-- immediately available.
minConfTime :: Baseline -> [(AWSCenters, DiffTime)]
minConfTime bl =
  [ (region, sum [
        validationTime bl, -- at the issuer
        capacity bl $ reqTxSize bl,
        networkDelay,
        validationTime bl, -- at each other node (in parallel)
        signTime,
        capacity bl $ ackTxSize bl,
        networkDelay,
        aggregateTime,
        verifySigTime])
  | (region, networkDelay) <- networkDelays bl]
  where
    (signTime, aggregateTime, verifySigTime) = case blScenario bl of
      FullTrust -> (0,0,0) -- no multisig in full trust model
      HydraUnlimited -> blAsigTimes bl

capacity :: Baseline -> (Size -> DiffTime)
capacity bl = kBitsPerSecond $ blBandwidth bl

ackTxSize :: Baseline -> Size
ackTxSize bl = case blScenario bl of
  FullTrust ->
      size (NewSn :: HeadProtocol MockTx) + size (TxId 0)
  HydraUnlimited ->
      size $ SigAckTx (mtxRef $ sampleTx bl) (sampleSig bl)

txMsgSize :: Baseline -> Size
txMsgSize bl = sum [
    reqTxSize bl,
    ackTxSize bl,
    confTxSize bl
  ]

snMsgSize :: Baseline -> Size
snMsgSize bl = case (blScenario bl, blSnapshots bl) of
  (HydraUnlimited, SnapAfter n) -> sum . map size $ [
    SigReqSn noSnapN (Set.fromList [TxId i | i <- [0..n-1]]),
    SigAckSn noSnapN (sampleSig bl),
    SigConfSn noSnapN (sampleASig bl)
    ]
  _ -> 0

reqTxSize :: Baseline -> Size
reqTxSize bl = size $ SigReqTx (sampleTx bl)

confTxSize :: Baseline -> Size
confTxSize bl = size $ SigConfTx (TxId 0) (sampleASig bl)

sampleSig :: Baseline -> Sig
sampleSig bl = unComp (ms_sig_tx (simpleMsig $ blAsigTimes bl) (SKey 0) (sampleTx bl))

sampleASig :: Baseline -> ASig
sampleASig bl = unComp (ms_asig_tx (simpleMsig $ blAsigTimes bl) (sampleTx bl) Set.empty Set.empty)

validationTime :: Baseline -> DiffTime
validationTime bl = mtxValidationDelay $ sampleTx bl

sampleTx :: Baseline -> MockTx
sampleTx bl = case blTxType bl of
  Simple -> cardanoTx (TxId 0) 2
  Plutus -> plutusTx (TxId 0)

networkDelays :: Baseline -> [(AWSCenters, DiffTime)]
networkDelays bl = [ (y, maximum
                       [ getSOrError x y
                       | x <- blLocations bl
                       ])
                   | y <- blLocations bl]

perSecond :: DiffTime -> Double
perSecond t = 1e12 / fromIntegral (diffTimeToPicoseconds t)

multiplyDiffTime :: DiffTime -> Double -> DiffTime
multiplyDiffTime t x = picosecondsToDiffTime . round $
  x * fromIntegral (diffTimeToPicoseconds t)
