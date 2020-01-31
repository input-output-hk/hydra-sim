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
  nodeASigTime :: (DiffTime, DiffTime, DiffTime)
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
-- - the network bandwidth: we hit a limit as soon as any one networking
--   interface is running out of capacity. When @n@ nodes each send
--   transactions, each node will
--
--   1. Send @n-1@ 'SigReqTx' messages, and receive @n-1@ 'SigAckTx' messages,
--   to get one transaction confirmed
--
--   2. Receive @n-1@ 'SigReqTx' messages, and send @n-1@ 'SigAckTx' messages,
--   to confirm the @n-1@ transactions from other nodes.
--
--   After one such step, @n@ transactions will be confirmed. Every node will
--   have sent and received @n-1@ messages of both 'SigReqTx' and 'SigAckTx'.
--
-- - CPU power: For each transaction, the main thread of each node will have to
--   validate the transaction, and verify the aggregate signature (signing and
--   aggregating signatures is done in spawned threads, which is safe since it
--   does not have to access any state). This puts a limit on the transaction
--   rate a node can process.
--
-- Whichever limit is lower determines the maximal throughput we could achieve.
performanceLimit :: [NodeSpec] -> ([(AWSCenters, DiffTime)], Double)
performanceLimit nodeSpecs = (minConfTime, maxTPS)
  where
    minConfTime = [ (region, sum [
                        validationTime, -- at the issuer
                        maximum [cap reqMsgSize | cap <- capacities],
                        networkDelay,
                        validationTime, -- at each other node (in parallel)
                        signTime,
                        maximum [cap ackMsgSize | cap <- capacities],
                        networkDelay,
                        aggregateTime,
                        verifySigTime])
                  | (region, networkDelay) <- networkDelays]
    maxTPS = min throughputBound cpuBound
    throughputBound = minimum
                      [ (perSecond (otherNodes * (cap (ackMsgSize + reqMsgSize)) / allNodes))
                      | cap <- capacities ]
    cpuBound = perSecond $ validationTime + verifySigTime
    networkDelays = [ (y, maximum
                        [ getSOrError x y
                        | x <- nodeRegion <$> nodeSpecs
                        ])
                    | y <- nodeRegion <$> nodeSpecs]
    capacities = kBitsPerSecond . nodeNetworkCapacity <$> nodeSpecs
    sampleTx = case nodeTxs (head nodeSpecs) of
       Simple -> cardanoTx (TxId 0) 2
       Plutus -> plutusTx (TxId 0)
    reqMsgSize = size $ SigReqTx sampleTx
    ackMsgSize = size $ SigAckTx (mtxRef sampleTx) sampleSig
    sampleSig = unComp (ms_sig_tx (simpleMsig asigTime) (SKey 0) sampleTx)
    asigTime@(signTime, aggregateTime, verifySigTime) = getMaxes (nodeASigTime <$> nodeSpecs)
    getMaxes = foldl (\(a,b,c) (a',b',c') -> (max a a', max b b', max c c')) (0, 0, 0)
    validationTime = mtxValidationDelay sampleTx
    allNodes = fromIntegral $ length nodeSpecs
    otherNodes = allNodes - 1

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
simpleMsig :: (DiffTime, DiffTime, DiffTime) -> MS MockTx
simpleMsig (t1, t2, t3) = MS {
  ms_sig_tx = ms_sign_delayed t1,
  ms_asig_tx = ms_asig_delayed t2,
  ms_verify_tx = ms_verify_delayed t3,

  ms_sig_sn = ms_sign_delayed t1,
  ms_asig_sn = ms_asig_delayed t2,
  ms_verify_sn = ms_verify_delayed t3
}
