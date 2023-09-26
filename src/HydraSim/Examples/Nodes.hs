module HydraSim.Examples.Nodes where

import Control.Monad (zipWithM)
import Control.Monad.Class.MonadAsync (MonadAsync (async))
import Control.Monad.Class.MonadThrow (MonadThrow)
import Control.Monad.Class.MonadTimer (
  MonadTimer,
 )
import Control.Monad.Class.MonadTimer.SI (
  MonadDelay (..),
 )
import Control.Tracer (Tracer)
import Data.Time (DiffTime)
import HydraSim.Examples.Channels (AWSCenters, channel)
import HydraSim.Examples.Txs (cardanoTx, plutusTx)
import HydraSim.HeadNode (connectNodes, newNode, startNode)
import qualified HydraSim.HeadNode.CoordinatedProtocolHandler as CoordinatedProtocolHandler
import qualified HydraSim.HeadNode.SimpleProtocolHandler as SimpleProtocolHandler
import HydraSim.MSig.Mock (
  ms_asig_delayed,
  ms_sign_delayed,
  ms_verify_delayed,
 )
import HydraSim.Sized (Size (Size))
import HydraSim.Trace (TraceHydraEvent)
import HydraSim.Tx.Mock (MockTx, TxRef (TxId))
import HydraSim.Types (
  MS (..),
  NodeConf (
    NodeConf,
    hcLeaderFun,
    hcMSig,
    hcNodeId,
    hcProtocolHandler,
    hcSnapshotStrategy,
    hcTxSendStrategy
  ),
  NodeId (NodeId),
  ProtocolFlavor (..),
  SnapN (SnapN),
  SnapStrategy,
  TxSendStrategy (SendTxs),
 )

data Txs
  = Plutus
  | Simple
  deriving (Show, Read)

data NodeSpec = NodeSpec
  { nodeRegion :: AWSCenters
  , nodeNetworkCapacity :: Integer
  -- ^ in- and outbound network capacity of this node, in kbits/s
  , nodeTxs :: Txs
  , nodeTxNumber :: Int
  , nodeTxConcurrency :: Int
  , nodeSnapStrategy :: SnapStrategy
  , nodeASigTime :: (DiffTime, DiffTime, DiffTime)
  , nodeHeadProtocolFlavor :: ProtocolFlavor
  }
  deriving (Show)

runNodes ::
  ( MonadTimer m
  , MonadAsync m
  , MonadThrow m
  , MonadDelay m
  ) =>
  [NodeSpec] ->
  Tracer m (TraceHydraEvent MockTx) ->
  m ()
runNodes nodeSpecs tracer = do
  nodes <- zipWithM createNode [0 ..] nodeSpecs
  connectAllNodes nodes
  mapM_ (\(_nspec, node) -> async (startNode tracer node)) nodes
  threadDelay 3.14e7
 where
  nNodes = length nodeSpecs
  sendStrategy i nspec =
    let txFun =
          ( case nodeTxs nspec of
              Plutus -> plutusTx
              Simple -> flip cardanoTx 2
          )
     in SendTxs (nodeTxConcurrency nspec) $
          [txFun (TxId $ nNodes * j + i) | j <- [0 .. nodeTxNumber nspec - 1]]
  selectProtocol nspec =
    case nodeHeadProtocolFlavor nspec of
      CoordinatedVanilla -> CoordinatedProtocolHandler.handleMessage
      Vanilla -> SimpleProtocolHandler.handleMessage
  createNode i nspec = do
    let nodeConf =
          NodeConf
            { hcNodeId = NodeId i
            , hcTxSendStrategy = sendStrategy i nspec
            , hcMSig = simpleMsig (nodeASigTime nspec)
            , hcLeaderFun = \(SnapN s) -> NodeId (s `mod` nNodes)
            , hcSnapshotStrategy = nodeSnapStrategy nspec
            , hcProtocolHandler = selectProtocol nspec
            }
        rate = kBitsPerSecond (nodeNetworkCapacity nspec)
    node <- newNode nodeConf rate rate
    return (nspec, node)
  connectAllNodes [] = return ()
  connectAllNodes (node : nodes) = do
    mapM_ (connect node) nodes
    connectAllNodes nodes
  connect (nspec, node) (nspec', node') =
    connectNodes (channel (nodeRegion nspec) (nodeRegion nspec')) node node'

kBitsPerSecond :: Integer -> (Size -> DiffTime)
kBitsPerSecond rate (Size bytes) = fromIntegral bytes * fromRational (recip $ (1024 * toRational rate) / 8)

simpleMsig :: (DiffTime, DiffTime, DiffTime) -> MS MockTx
simpleMsig (t1, t2, t3) =
  MS
    { ms_sig_tx = ms_sign_delayed t1
    , ms_asig_tx = ms_asig_delayed t2
    , ms_verify_tx = ms_verify_delayed t3
    , ms_sig_sn = ms_sign_delayed t1
    , ms_asig_sn = ms_asig_delayed t2
    , ms_verify_sn = ms_verify_delayed t3
    }
