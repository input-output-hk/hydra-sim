module HydraSim.Examples.Nodes
  where

import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime
import Control.Monad.Class.MonadTimer
import Control.Tracer
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds)
import HydraSim.DelayedComp (unComp)
import HydraSim.Examples.Channels
import HydraSim.Examples.Txs
import HydraSim.HeadNode
import HydraSim.MSig.Mock
import HydraSim.Sized
import HydraSim.Trace
import HydraSim.Tx.Mock
import HydraSim.Types

data Txs = Plutus
         | Simple
         deriving (Show, Read)

data NodeSpec = NodeSpec {
  nodeRegion :: AWSCenters,
  -- | in- and outbound network capacity of this node, in kbits/s
  nodeNetworkCapacity :: Integer,
  nodeTxs :: Txs,
  nodeTxNumber :: Int,
  nodeTxConcurrency :: Int,
  nodeSnapStrategy :: SnapStrategy,
  nodeASigTime :: DiffTime
  } deriving Show

-- | The maximal performance of the system we could expect (ignoring checkpointing)
--
-- The minimal confirmation time for a transaction is a sum of
--
-- - 2x validation time (once at the node that's sending the tx, once at every
--   other node)
--
-- - the time it takes for both the @SigReqTx@ and @SigAckTx@ messages to pass
--   through a networking interface
--
-- - the time it takes to sign a transaction, aggregate signatures, and confirm
-- - an aggregate signature
--
-- - the longest round-trip time to any other node
--
-- The longest round-trip time to any of its peers will in general be different
-- for each node, so we get a different minimal confirmation time per node.
--
--
-- The transaction throughput is limited by two factors:
--
-- - the network bandwidth: we cannot process transactions faster than
--   continuously sending the largest message involved in tx confirmation.
--
-- - CPU power: assuming the protocol is executed sequentially (which we could
--   relax), we achieve maximal throughput when all nodes are continuously busy
--   with validating txs, signing, aggregating signatures, and verifying
--   aggregate signatures. Since the computational load differs between a node
--   that's submitting txs and nodes signing them, and all nodes have both
--   roles, we take the weighted average.
--
-- Whichever limit is lower determines the maximal throughput we could achieve.
performanceLimit :: [NodeSpec] -> ([(AWSCenters, DiffTime)], Double)
performanceLimit nodeSpecs = (minConfTime, maxTPS)
  where
    minConfTime = [(region, sum [roundTrip, squeezeThrough, 3*asigTime, 2 * validationTime])
                  | (region, roundTrip) <- roundTrips]
    maxTPS = min throughputBound cpuBound
    throughputBound = minimum
                      [  perSecond (((cap reqMsgSize) + otherNodes * (cap ackMsgSize)) / allNodes)
                      | cap <- capacities ]
    cpuBound = perSecond (((validationTime + 3*asigTime) + otherNodes * (validationTime + 2*asigTime))/allNodes)
    roundTrips = [ (y, 2 * maximum
                   [ getSOrError x y
                   | x <- nodeRegion <$> nodeSpecs
                   ])
                 | y <- nodeRegion <$> nodeSpecs]
    squeezeThrough = maximum
      [ cap reqMsgSize + cap ackMsgSize
      | cap <- capacities ]
    capacities = kBitsPerSecond . nodeNetworkCapacity <$> nodeSpecs
    sampleTx = case nodeTxs (head nodeSpecs) of
       Simple -> cardanoTx (TxId 0) 2
       Plutus -> plutusTx (TxId 0)
    reqMsgSize = size $ SigReqTx sampleTx
    ackMsgSize = size $ SigAckTx (mtxRef sampleTx) sampleSig
    sampleSig = unComp (ms_sig_tx (simpleMsig asigTime) (SKey 0) sampleTx)
    asigTime = maximum (nodeASigTime <$> nodeSpecs)
    validationTime = mtxValidationDelay sampleTx
    allNodes = fromIntegral $ length nodeSpecs
    otherNodes = fromIntegral $ length nodeSpecs - 1

perSecond :: DiffTime -> Double
perSecond t = 1e12 / fromIntegral (diffTimeToPicoseconds t)

runNodes
  :: ( MonadTimer m, MonadSTM m, MonadSay m, MonadFork m, MonadAsync m,
       MonadThrow m, MonadTime m)
  => [NodeSpec]
  -> Tracer m (TraceHydraEvent MockTx)
  -> m ()
runNodes nodeSpecs tracer = do
  nodes <- mapM (uncurry createNode) (zip [0..] nodeSpecs)
  connectAllNodes nodes
  mapM_ (\(_nspec, node) -> async (startNode tracer node)) nodes
  threadDelay $ 3.14e7
  where
  nNodes = length nodeSpecs
  sendStrategy i nspec =
    let txFun = (case nodeTxs nspec of
                   Plutus -> plutusTx
                   Simple -> flip cardanoTx 2)
    in SendTxs (nodeTxConcurrency nspec) $
       [ txFun (TxId $ nNodes * j + i) | j <- [0..(nodeTxNumber nspec)-1]]
  createNode i nspec = do
    let 
        nodeConf = NodeConf {
          hcNodeId = NodeId i,
          hcTxSendStrategy = sendStrategy i nspec,
          hcMSig = simpleMsig (nodeASigTime nspec),
          hcLeaderFun = \(SnapN s) -> NodeId (s `mod` nNodes),
          hcSnapshotStrategy = nodeSnapStrategy nspec
          }
        rate = kBitsPerSecond (nodeNetworkCapacity nspec)
    node <- newNode nodeConf rate rate
    return (nspec, node)
  connectAllNodes [] = return ()
  connectAllNodes (node:nodes) = do
    mapM_ (connect node) nodes
    connectAllNodes nodes
  connect (nspec, node) (nspec', node') =
    connectNodes (channel (nodeRegion nspec) (nodeRegion nspec')) node node'

kBitsPerSecond :: Integer -> (Size -> DiffTime)
kBitsPerSecond rate = \(Size bytes) -> (fromIntegral bytes) * (fromRational $ recip $ (1024 * toRational rate ) / 8)

-- TODO: This needs to be checked!
simpleMsig :: DiffTime -> MS MockTx
simpleMsig t = MS {
  ms_sig_tx = ms_sign_delayed t,
  ms_asig_tx = ms_asig_delayed t,
  ms_verify_tx = ms_verify_delayed t,

  ms_sig_sn = ms_sign_delayed t,
  ms_asig_sn = ms_asig_delayed t,
  ms_verify_sn = ms_verify_delayed t
}
