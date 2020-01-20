module HydraSim.ProtocolFunctions
  ( maxTxos,
    txObj,
    snObj,
    reach
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           HydraSim.Tx.Class
import           HydraSim.Types


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

snObj
  :: Tx tx
  => HState tx
  -> SnapN
  -> Set (TxInput tx)
  -> Set (TxRef tx)
  -> Snap tx
snObj hs s o t = Snap {
  snos = s,
  snoO = applyValidTxs o (txObMapToTxs $ reach (hsTxsConf hs) t),
  snoT = t,
  snoS = Set.empty,
  snoSigma = Nothing
  }
  where
    txObMapToTxs = Map.elems . fmap txoTx

reachOb
  :: Tx tx
  => Map (TxRef tx) (TxO tx)
  -> Map (TxRef tx) (TxO tx)
  -> Map (TxRef tx) (TxO tx)
reachOb tb tobs
  | Map.null tobs = Map.empty
  | otherwise =
    let referencedTxRefs = Set.unions (txoT <$> tobs)
        referencedTxs = Map.filterWithKey
                        (\txref _ -> txref `Set.member` referencedTxRefs
                          && not (txref `Set.member` Map.keysSet tobs))
                        tb
        r = reachOb tb referencedTxs
    in (tobs `Map.intersection` tb) `Map.union` r

reach
  :: Tx tx
  => Map (TxRef tx) (TxO tx)
  -> Set (TxRef tx)
  -> Map (TxRef tx) (TxO tx)
reach tb trefs = reachOb tb $
  Map.filterWithKey (\txref _ -> txref `Set.member` trefs) tb

