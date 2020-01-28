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
import Data.Time.Clock (DiffTime)
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
  nodeTxConcurrency :: Int
  } deriving Show

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
          hcMSig = simpleMsig,
          hcLeaderFun = \(SnapN s) -> NodeId (s `mod` nNodes),
          hcSnapshotStrategy = SnapAfterNTxs 1
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
simpleMsig :: MS MockTx
simpleMsig = MS {
  ms_sig_tx = ms_sign_delayed 0.0005,
  ms_asig_tx = ms_asig_delayed 0.0005,
  ms_verify_tx = ms_verify_delayed 0.0005,

  ms_sig_sn = ms_sign_delayed 0.0005,
  ms_asig_sn = ms_asig_delayed 0.0005,
  ms_verify_sn = ms_verify_delayed 0.0005
}
