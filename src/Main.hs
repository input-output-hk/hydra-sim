module Main where

import Control.Monad.Class.MonadAsync
import Control.Monad.IOSim
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadTimer
import Control.Monad.Class.MonadSay

import Control.Monad (void)
import Data.Time.Clock (secondsToDiffTime)
import qualified Data.Map.Strict as Map

import Channel
import HeadNode
import HeadProtocol

main :: IO ()
main = do
  printTraceEventsSay $ runSimTrace twoNodesExample

twoNodesExample :: (MonadTimer m, MonadSTM m, MonadSay m, MonadFork m, MonadAsync m) => m ()
twoNodesExample = do
  (cha, chb) <- createConnectedBoundedChannels 100
  let node0 = HeadNode {
        hnId = NodeId 0,
        hnChannels = Map.fromList [(NodeId 1, cha)],
        hnTxSendStrategy = SendSingleTx (Tx (TxId 0) (secondsToDiffTime 1))
        }
      node1 = HeadNode {
        hnId = NodeId 1,
        hnChannels = Map.fromList [(NodeId 0, chb)],
        hnTxSendStrategy = SendSingleTx (Tx (TxId 1) (secondsToDiffTime 1))
        }
  void $ concurrently (startNode node0) (startNode node1)
