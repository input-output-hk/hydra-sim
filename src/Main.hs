module Main where

import Control.Monad.Class.MonadAsync
import Control.Monad.IOSim
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadTimer
import Control.Monad.Class.MonadSay

import Control.Monad (void)
import Data.Time.Clock (secondsToDiffTime)

import Channel
import HeadNode
import HeadProtocol

main :: IO ()
main = do
  printTraceEventsSay $ runSimTrace twoNodesExample

twoNodesExample :: (MonadTimer m, MonadSTM m, MonadSay m, MonadFork m, MonadAsync m) => m ()
twoNodesExample = do
  node0 <- newNode (NodeId 0) (SendSingleTx (Tx (TxId 0) (secondsToDiffTime 1)))
  node1 <- newNode (NodeId 1) (SendSingleTx (Tx (TxId 1) (secondsToDiffTime 1)))
  (cha, chb) <- createConnectedBoundedChannels 100
  addPeer node0 (NodeId 1) cha
  addPeer node1 (NodeId 0) chb
  void $ concurrently (startNode node0) (startNode node1)
