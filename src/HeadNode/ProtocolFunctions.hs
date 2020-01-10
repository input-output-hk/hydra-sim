module HeadNode.ProtocolFunctions
  ( maxTxos, txObj
  ) where

-- import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import HeadNode.Types
import Tx.Class


maxTxos :: Tx tx => Map (TxRef tx) (TxO tx) -> Map (TxRef tx) (TxO tx)
maxTxos txobs =
  let predicate = (\txob -> noMemberSuchThat txobs $ \txob' -> (txRef . txoTx $ txob) `Set.member` txoT txob')
  in Map.filter predicate txobs
  where
    noMemberSuchThat :: Map k v -> (v -> Bool) -> Bool
    noMemberSuchThat xs predicate = Map.null $ Map.filter predicate xs

txObj :: Tx tx => Map (TxRef tx) (TxO tx) -> NodeId -> tx -> TxO tx
txObj txsConf k tx = TxO {
  txoIssuer = k,
  txoTx = tx,
  txoS = Set.empty,
  txoSigma = Nothing,
  txoT =
      let txobs = Map.filter
            (\txob -> not . Set.null $ (txo . txoTx $ txob) `Set.intersection` txi tx)
            txsConf
      in Map.keysSet $ maxTxos txobs
  }
