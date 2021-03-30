{-# LANGUAGE PatternGuards #-}

{- |
Module HydraSim.Examples.Baselines
Description: Baseline scenarios for protocol performance
-}
module HydraSim.Examples.Baselines (
    Baseline (..),
    Concurrency (..),
    Scenario (..),
    tpsTotalBound,
    baselineTPS,
    findIntersection,
    minConfTime,
) where

import qualified Data.Set as Set
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds, picosecondsToDiffTime)
import HydraSim.DelayedComp (unComp)
import HydraSim.Examples.Channels
import HydraSim.Examples.Nodes
import HydraSim.Examples.Txs
import HydraSim.MSig.Mock
import HydraSim.Sized
import HydraSim.Tx.Mock
import HydraSim.Types

data Scenario = FullTrust | HydraUnlimited | SpritesUnlimited deriving (Eq, Show, Read)
data Concurrency = FiniteConc Int | UnlimitedConc deriving (Eq, Show, Read)
type Bandwidth = Integer

-- | Type representing bounds on the transaction rate from different constraints.
data TPSBound = TPSBound
    { tpsCpuBound :: Double
    , tpsBandwidthBound :: Double
    , -- | We only have a bound from latency if we have finite concurrency
      -- (otherwise, we can amortise it completely)
      tpsMLatencyBound :: Maybe Double
    }
    deriving (Eq, Show)

-- | Get an overall bound on the transaction rate from a 'TPSBound'
tpsTotalBound :: TPSBound -> Double
tpsTotalBound tps = min (tpsConstantBound tps) (tpsBandwidthBound tps)

-- | The part of the bound that does not depend on bandwidth
tpsConstantBound :: TPSBound -> Double
tpsConstantBound (TPSBound cpu _bandwidth Nothing) = cpu
tpsConstantBound (TPSBound cpu _bandwidth (Just latency)) = min cpu latency

-- | Input data for calculating baselines
data Baseline = Baseline
    { blScenario :: Scenario
    , blConc :: Concurrency
    , -- | network capacity, in kbits/s
      blBandwidth :: Bandwidth
    , blLocations :: [AWSCenters]
    , blAsigTimes :: (DiffTime, DiffTime, DiffTime)
    , -- | We can also handle snapshots in baselines.
      --
      -- Note, however, that @SnapAfter n@ in a baseline is interpreted so that
      -- there will be a snapshot after every @n@ transactions. In general, this
      -- will be more than in a real run. In a run, a node will produce a snapshot
      -- once it has at least @n@ confirmed transactions, and is the current leader.
      blSnapshots :: SnapStrategy
    , blTxType :: Txs
    }

-- | Calculate bound on transaction rate for a given baseline
baselineTPS :: Baseline -> TPSBound
baselineTPS bl = TPSBound cpuBound bandwidthBound mlatencyBound
  where
    allNodes = length (blLocations bl)
    otherNodes = allNodes - 1
    allNodesT = fromIntegral allNodes :: DiffTime
    otherNodesT = fromIntegral otherNodes :: DiffTime
    (_signTime, _aggregateTime, verifySigTime) = blAsigTimes bl
    cpuBound = case blScenario bl of
        FullTrust ->
            perSecond $ validationTime bl
        HydraUnlimited ->
            perSecond $ validationTime bl + verifySigTime `multiplyDiffTime` (1 + snapFactor)
        SpritesUnlimited ->
            let sigTime = case blConc bl of
                    FiniteConc conc -> verifySigTime `multiplyDiffTime` (1 / (fromIntegral (length (blLocations bl)) * fromIntegral conc))
                    UnlimitedConc -> 0
             in perSecond $ validationTime bl + sigTime
    bandwidthBound = case blScenario bl of
        scen
            | scen `elem` [FullTrust, HydraUnlimited] ->
                perSecond $
                    otherNodesT / allNodesT
                        * ( capacity bl (txMsgSize bl) + (capacity bl (snMsgSize bl) `multiplyDiffTime` snapFactor)
                          )
        SpritesUnlimited ->
            minimum $
                perSecond
                    <$> [ capacity
                            bl
                            ( round ((fromIntegral otherNodes / fromIntegral allNodes :: Double) * fromIntegral (reqTxSize bl))
                                + fromIntegral otherNodes * ackNTxSizePerMessage bl
                            )
                        , capacity bl (fromIntegral otherNodes * (reqNTxSizePerMessage bl + confNTxSizePerMessage bl))
                        ]
        _ -> error "Not implemented"
    mlatencyBound = case (blConc bl, blScenario bl) of
        (UnlimitedConc, _scen) -> Nothing
        (FiniteConc conc, scen)
            | scen `elem` [FullTrust, HydraUnlimited] ->
                Just $
                    perSecond ((maximum . map snd $ networkDelays bl) * 2 / (fromIntegral conc * allNodesT))
            | scen == SpritesUnlimited ->
                Just $
                    perSecond ((maximum . map snd $ networkDelays bl) * 4 / (fromIntegral conc * allNodesT))
        _ -> error "Not implemented"
    snapFactor = case blSnapshots bl of
        NoSnapshots -> 0
        SnapAfter n -> 1 / (fromIntegral n :: Double)

{- | Evaluate TPS bound at suitable supporting points, to plot TPS over bandwidth.

 Since the bound that depends on the bandwidth is linear, and the others are
 constant, we can calculate the point of intersection.
-}
findIntersection ::
    Baseline ->
    -- | lower and upper limit
    (Bandwidth, Bandwidth) ->
    -- | list of evaluation points. Includes lower and upper limit, and
    -- intersection point.
    [(Bandwidth, TPSBound)]
findIntersection bl (lower, upper)
    | lower > upper = findIntersection bl (upper, lower)
    | otherwise =
        let (lowerTPS, upperTPS) = (lowerBl, upperBl)
         in if tpsBandwidthBound lowerBl < tpsConstantBound lowerBl
                && tpsBandwidthBound upperBl > tpsConstantBound upperBl
                then
                    let intersection = tpsConstantBound lowerBl * fromIntegral lower / tpsBandwidthBound lowerBl
                     in [(lower, lowerTPS), (round intersection, toBl $ round intersection), (upper, upperTPS)]
                else [(lower, lowerTPS), (upper, upperTPS)]
  where
    toBl bw = baselineTPS $ bl{blBandwidth = bw}
    lowerBl = toBl lower
    upperBl = toBl upper

{- | Minimal confirmation time for a transaction.

 We get this by just adding the times for all actions that need to be
 performed for a transaction to be conofirmed, assuming that every resource is
 immediately available.
-}
minConfTime :: Baseline -> [(AWSCenters, DiffTime)]
minConfTime bl =
    [ ( region
      , sum
            [ validationTime bl -- at the issuer
            , capacity bl $ reqTxSize bl
            , networkDelay
            , validationTime bl -- at each other node (in parallel)
            , signTime
            , capacity bl $ ackTxSize bl
            , networkDelay
            , aggregateTime
            , verifySigTime
            ]
      )
    | (region, networkDelay) <- networkDelays bl
    ]
  where
    (signTime, aggregateTime, verifySigTime) = case blScenario bl of
        FullTrust -> (0, 0, 0) -- no multisig in full trust model
        HydraUnlimited -> blAsigTimes bl
        _ -> error "Not implemented"

capacity :: Baseline -> (Size -> DiffTime)
capacity bl = kBitsPerSecond $ blBandwidth bl

ackTxSize :: Baseline -> Size
ackTxSize bl = case blScenario bl of
    FullTrust ->
        size (NewSn :: HeadProtocol MockTx) + size (TxId 0)
    HydraUnlimited ->
        size $ SigAckTx (mtxRef $ sampleTx bl) (sampleSig bl)
    _ -> error "Not implemented"

txMsgSize :: Baseline -> Size
txMsgSize bl =
    sum
        [ reqTxSize bl
        , ackTxSize bl
        , confTxSize bl
        ]

snMsgSize :: Baseline -> Size
snMsgSize bl = case (blScenario bl, blSnapshots bl) of
    (HydraUnlimited, SnapAfter n) ->
        sum . map size $
            [ SigReqSn noSnapN (Set.fromList [TxId i | i <- [0 .. n -1]])
            , SigAckSn noSnapN (sampleSig bl)
            , SigConfSn noSnapN (sampleASig bl)
            ]
    _ -> 0

reqTxSize :: Baseline -> Size
reqTxSize bl = size $ SigReqTx (sampleTx bl)

confTxSize :: Baseline -> Size
confTxSize bl = size (SigConfTx (TxId 0) (sampleASig bl))

-- In the Sprites Unlimited baseline, we do req, ack, and conf in batches. As
-- batrch size, we take the number of nodes times the transaction concurrency
-- per node. We need to provide the sizes of the involved messages. Of course,
-- they are only finite for finite concurrency. For infinite concurrency, the
-- message size itself will diverge, but the ratio of message size/transaction
-- will still be finite. So instead of providing the raw message size, we
-- provide the message size _per transaction_ (in the limit of inifinite batch
-- size).

confNTxSizePerMessage :: Baseline -> Size
confNTxSizePerMessage bl = case blConc bl of
    FiniteConc _conc ->
        perMessage bl $
            size (SigConfTx (TxId 0) (sampleASig bl))
                + fromIntegral (batchSize bl - 1) * size (TxId 0)
    UnlimitedConc -> size (TxId 0)

reqNTxSizePerMessage :: Baseline -> Size
reqNTxSizePerMessage bl = case blConc bl of
    FiniteConc _conc ->
        perMessage bl $
            size (SigReqTx (sampleTx bl))
                + fromIntegral (batchSize bl - 1) * size (sampleTx bl)
    UnlimitedConc -> size $ sampleTx bl

ackNTxSizePerMessage :: Baseline -> Size
ackNTxSizePerMessage bl = case blConc bl of
    FiniteConc _conc ->
        perMessage bl $
            size (SigAckTx (TxId 0) (sampleSig bl))
                + fromIntegral (batchSize bl - 1) * size (TxId 0)
    UnlimitedConc -> size $ TxId 0

perMessage :: Baseline -> Size -> Size
perMessage bl s =
    round $ (fromIntegral s :: Double) / fromIntegral (batchSize bl)

sampleSig :: Baseline -> Sig
sampleSig bl = unComp (ms_sig_tx (simpleMsig $ blAsigTimes bl) (SKey 0) (sampleTx bl))

sampleASig :: Baseline -> ASig
sampleASig bl = unComp (ms_asig_tx (simpleMsig $ blAsigTimes bl) (sampleTx bl) Set.empty Set.empty)

batchSize :: Baseline -> Int
batchSize bl = case blConc bl of
    FiniteConc conc -> conc * length (blLocations bl)
    UnlimitedConc -> error "batchSize undefined for infinite transaction concurrency."

validationTime :: Baseline -> DiffTime
validationTime bl = mtxValidationDelay $ sampleTx bl

sampleTx :: Baseline -> MockTx
sampleTx bl = case blTxType bl of
    Simple -> cardanoTx (TxId 0) 2
    Plutus -> plutusTx (TxId 0)

networkDelays :: Baseline -> [(AWSCenters, DiffTime)]
networkDelays bl =
    [ ( y
      , maximum
            [ getSOrError x y
            | x <- blLocations bl
            ]
      )
    | y <- blLocations bl
    ]

perSecond :: DiffTime -> Double
perSecond t = 1e12 / fromIntegral (diffTimeToPicoseconds t)

multiplyDiffTime :: DiffTime -> Double -> DiffTime
multiplyDiffTime t x =
    picosecondsToDiffTime . round $
        x * fromIntegral (diffTimeToPicoseconds t)
