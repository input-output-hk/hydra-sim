module HeadNode.Handler
  ( handleMessage
  ) where

import Control.Monad (forever, forM_, void)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- imports from io-sim, io-sim-classes, contra-tracer

import Control.Monad.Class.MonadTimer

-- imports from this package

import DelayedComp
import HeadNode.ProtocolFunctions
import HeadNode.Types
import MSig.Mock
import Tx.Class


-- | This is the actual logic of the protocol
handleMessage
  :: (MonadTimer m, Tx tx)
  => NodeConf tx
  -> NodeId
  -> HStateTransformer m tx

handleMessage conf _peer s (New tx)
  | isValid = DecApply
    (validComp >> pure s)
    (TPTxNew (txRef tx) (hcNodeId conf))
    (return (Multicast (SigReqTx tx)))
  | otherwise =
    DecInvalid
    (void validComp)
    ("Transaction " ++ show (txRef tx) ++ " is not valid in the confirmed UTxO")
  where
    validComp = txValidate (hsUTxOConf s) tx
    isValid = unComp validComp

handleMessage conf peer s (SigReqTx tx)
  | isValid = DecApply
    (do void validComp
        pure $ s {
          hsTxsSig = Map.insert (txRef tx) (txObj (hsTxsConf s) peer tx) (hsTxsSig s),
          hsUTxOSig = hsUTxOSig s `txApplyValid` tx
          })
    (TPTxSig (txRef tx) (hcNodeId conf))
    (do sigma <- (ms_sig_tx . hcMSig $ conf) (hsSK s) tx
        return $ SendTo peer (SigAckTx (txRef tx) sigma))
  | otherwise = DecWait (void validComp)
  where
    validComp = txValidate (hsUTxOConf s) tx
    isValid = unComp validComp

handleMessage conf peer s (SigAckTx txref sig)
  -- assert that all the requirements are fulfilled
  | Map.notMember txref (hsTxsSig s) =
    DecInvalid (return ()) $ "Transaction " ++ show txref ++ " not found."
  | txoIssuer txob /= (hcNodeId conf) =
    DecInvalid (return ()) $ "Transaction " ++ show txref ++ " not issued by " ++ show (hcNodeId conf)
  | sig `Set.member` txoS txob =
    DecInvalid (return ()) $ "Transition " ++ show txref ++ " already signed with " ++ show sig
  | otherwise = DecApply
    (pure $ s { hsTxsSig = Map.insert txref txob' (hsTxsSig s) })
    (TPTxAck txref peer)
    (if Set.size (txoS txob') < Set.size (hsVKs s)
     then pure SendNothing
     else do
        asig <- (ms_asig_tx . hcMSig $ conf) (txoTx txob') (hsVKs s) (txoS txob')
        return $ Multicast (SigConfTx txref asig)
        )
  where
    txob = (hsTxsSig s) Map.! txref
    txob' = txob { txoS = sig `Set.insert` txoS txob}

handleMessage conf _peer s (SigConfTx txref asig)
  | Map.notMember txref (hsTxsSig s) =
    DecInvalid (return ()) $ "Transaction " ++ show txref ++ " not found."
  | not isValid = DecInvalid (void validComp)
    ("Invalid aggregate signature " ++ show asig ++ ", " ++ show avk ++ " in SigConfTx")
  | otherwise = DecApply
    (do
        _ <- validComp -- this is only to wait, we already guarded against the
                       -- signature being invalid
        let txsSig = Map.insert txref txob' (hsTxsSig s)
        pure $ s {
          hsUTxOConf = hsUTxOConf s `txApplyValid` txoTx txob,
          hsTxsSig = txsSig,
          hsTxsConf = txsSig
          })
    (TPTxConf txref)
    (pure SendNothing)
  where
    avk = ms_avk $ hsVKs s
    txob = (hsTxsSig s) Map.! txref
    txob' = txob { txoSigma = Just asig }
    validComp = (ms_verify_tx . hcMSig $ conf) avk (txoTx txob) asig
    isValid = unComp validComp

handleMessage conf _peer s NewSn
  | (hcLeaderFun conf) snapN /= (hcNodeId conf) =
    DecInvalid (return ()) $ (show (hcNodeId conf)) ++ " Cannot create snaphot " ++ show snapN
  | otherwise = DecApply
    (pure s)
    (TPSnNew snapN (hcNodeId conf))
    (pure $ Multicast (SigReqSn snapN txSet))
  where
    snapN = nextSn (hsSnapNConf s)
    txSet = Map.keysSet $ maxTxos (hsTxsConf s)

handleMessage conf peer s (SigReqSn snapN txRefs)
  | snapN /= nextSn lastSn =
    DecInvalid (return ()) $ ("Did not expec snapshot " ++ show snapN ++ ", last was " ++ show lastSn)
  | (hcLeaderFun conf) snapN /= peer =
    DecInvalid (return ()) $ (show peer ++ " should not create snapshot " ++ show snapN)
  | hsSnapNConf s /= hsSnapNSig s = DecWait (return ())
  | not (txRefs `Set.isSubsetOf` Map.keysSet (hsTxsConf s)) = DecWait (return ())
  | otherwise = DecApply
    (return s')
    (TPSnSig snapN (hcNodeId conf))
    (do sig <- (ms_sig_sn . hcMSig $ conf) (hsSK s) (snapN, snoO (hsSnapSig s'))
        return $ SendTo peer (SigAckSn snapN sig))
  where
    lastSn = hsSnapNSig s
    s' = s { hsSnapNSig = snapN,
             hsSnapSig = snObj s snapN (snoO $ hsSnapConf s) txRefs
           }

handleMessage conf peer s (SigAckSn snapN sig)
  | snapN /= (hsSnapNSig s) = DecInvalid (return ())
    ("Got signature for snapshot " ++ show snapN
     ++ " when signed snapshot was " ++ show (hsSnapNSig s))
  | (hcLeaderFun conf) snapN /= hcNodeId conf = DecInvalid (return ())
    ("Received signature for snapshot " ++ show snapN
     ++ " which was not ours to create")
  | (sig `Set.member` snoS snob) = DecInvalid (return ())
    ("Received signature " ++ show sig ++ " for snapshot "
     ++ show snapN ++ ", which we already had.")
  | otherwise = DecApply
    (return $ s { hsSnapSig = snob' })
    (TPSnAck snapN peer)
    (if Set.size (snoS snob') < Set.size (hsVKs s)
     then pure SendNothing
     else do
        asig <- (ms_asig_sn . hcMSig $ conf) (snapN, snoO snob') (hsVKs s) (snoS snob')
        return $ Multicast (SigConfSn snapN asig))
  where
    snob = hsSnapSig s
    snob' = snob {snoS = sig `Set.insert` snoS snob}

handleMessage conf _peer s (SigConfSn snapN asig)
  | snapN /= (hsSnapNSig s) = DecInvalid (return ())
    ("Can't confirm snapshot " ++ show snapN
      ++ " when last signed snapshot is " ++ show (hsSnapNSig s))
  | (hsSnapNSig s) == (hsSnapNConf s) = DecInvalid (return ())
    ("Trying to confirm " ++ show snapN ++ " but it is leady confirmed.")
  | not isValid = DecInvalid (void validComp)
    ("Invalid aggregate signature " ++ show asig ++ ", " ++ show avk ++ " in SigConfSn")
  | otherwise = DecApply
    (do void validComp
        return s')
    (TPSnConf snapN)
    (pure SendNothing)
  where
    avk = ms_avk $ hsVKs s
    snob = hsSnapSig s
    snob' = snob { snoSigma = Just asig }
    validComp = (ms_verify_sn . hcMSig $ conf) avk (snapN, snoO snob) asig
    isValid = unComp validComp
    s' = s {
      hsSnapNConf = snapN,
      hsSnapSig = snob',
      hsSnapConf = snob,
      hsTxsConf = (hsTxsConf s) Map.\\ reach (hsTxsConf s) (snoT snob'),
      hsTxsSig = (hsTxsSig s) Map.\\ reach (hsTxsSig s) (snoT snob')
      }
