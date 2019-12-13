{-# LANGUAGE LambdaCase #-}
module HeadNode where

import Control.Monad (forever, forM_, void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadTimer

import Channel
import HeadProtocol

data TxSendStrategy =
    SendNoTx
  | SendSingleTx Tx
  deriving (Show, Eq)

data HeadNode m = HeadNode {
  hnId :: NodeId,
  hnChannels :: Map NodeId (Channel m HeadProtocol),
  hnTxSendStrategy :: TxSendStrategy
  }

startNode :: (MonadSTM m, MonadSay m, MonadTimer m, MonadAsync m) =>
  HeadNode m -> m ()
startNode hn = void $ concurrently (listener hn) (txSender hn)

listener :: (MonadSTM m, MonadSay m, MonadTimer m, MonadAsync m) =>
  HeadNode m -> m ()
listener hn = forever $ do
  let readFromCh (nodeId, ch) = async $ (,) <$> pure nodeId <*> recv ch
  readActions <- mapM readFromCh (Map.toList (hnChannels hn))
  snd <$> waitAnyCancel readActions >>= \case
    (nodeId, Just (RequestTxSignature tx)) -> do
      say $ concat ["Node ", show (hnId hn), " received ", show tx]
      threadDelay (txValidationTime tx)
      say $ concat ["Node ", show (hnId hn), " validated ", show tx]
      case Map.lookup nodeId (hnChannels hn) of
        Just ch -> send ch $ ProvideTxSignature (tx, hnId hn)
        Nothing -> error "This should not happen."
    (_, Just (ProvideTxSignature (tx, nodeId))) ->
      say $ concat ["got confirmation for ", show tx, " from ", show nodeId]
    (nodeId, Nothing) -> say $ concat ["read Nothing from ", show nodeId]

txSender :: (MonadAsync m, MonadSay m) =>
  HeadNode m -> m ()
txSender hn = case (hnTxSendStrategy hn) of
  SendSingleTx tx ->  forM_ (Map.toList $ hnChannels hn) $ \(nodeId, ch) -> do
    say $ concat ["sending ", show tx, " from ", show (hnId hn), " to ", show nodeId]
    send ch $ RequestTxSignature tx
  SendNoTx -> return ()
