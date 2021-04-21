{- | Coordinated version of "Vanilla" Hydra Head protocol.

In this version:

* The nodes do not confirm individual transactions, only snapshots
-}
module HydraSim.HeadNode.CoordinatedProtocolHandler (
    handleMessage,
) where

import Control.Monad (void, when)
import Data.Functor (($>))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import HydraSim.DelayedComp
import HydraSim.MSig.Mock
import HydraSim.ProtocolFunctions
import HydraSim.Tx.Class
import HydraSim.Types

-- | This is the actual logic of the protocol
handleMessage ::
    Tx tx => ProtocolHandler tx
handleMessage conf _peer s (New tx)
    | isValid =
        -- Transaction is valid w.r.t to confirmed UTXO set, we broadcast it to
        -- all nodes.
        DecApply
            (validComp $> s)
            (TPTxNew (txRef tx) (hcNodeId conf))
            (return (Multicast (SigReqTx tx)))
    | otherwise =
        DecInvalid
            (void validComp)
            ("Transaction " ++ show (txRef tx) ++ " is not valid in the confirmed UTxO")
  where
    validComp = hsUTxOConf s `txValidate` tx
    isValid = unComp validComp
handleMessage conf peer s (SigReqTx tx)
    | isValid =
        DecApply
            -- we do not need to validate the transaction if the message is from us; in
            -- that case, we just validated it.
            ( do
                when (peer /= hcNodeId conf) $ void validComp
                pure $
                    s
                        { hsTxsSig = Map.insert (txRef tx) (txObj (hsTxsSig s) peer tx) (hsTxsSig s)
                        -- we insert the tx in the signed tx map but actually we don't need to sign it
                        }
            )
            (TPTxSig (txRef tx) (hcNodeId conf))
            (pure SendNothing)
    | otherwise = DecWait (void validComp) -- TODO do we need to wait for validating here?
  where
    validComp = txValidate (hsUTxOConf s) tx
    isValid = unComp validComp
handleMessage conf _peer s NewSn
    | hcLeaderFun conf snapN /= hcNodeId conf =
        DecInvalid (return ()) $ show (hcNodeId conf) ++ " Cannot create snaphot " ++ show snapN
    | otherwise =
        DecApply
            (pure s)
            (TPSnNew snapN (hcNodeId conf))
            (pure $ Multicast (SigReqSn snapN txSet))
  where
    snapN = nextSn (hsSnapNConf s)
    txSet = Map.keysSet $ maxTxos (hsTxsSig s) -- We snapshot maximal 'tip' of available txs all the (valid) transactions  we have seen so far
handleMessage conf peer s (SigReqSn snapN txRefs)
    | snapN /= nextSn lastSn =
        DecInvalid (return ()) ("Did not expec snapshot " ++ show snapN ++ ", last was " ++ show lastSn)
    | hcLeaderFun conf snapN /= peer =
        DecInvalid (return ()) (show peer ++ " should not create snapshot " ++ show snapN)
    | hsSnapNConf s /= hsSnapNSig s = DecWait (return ())
    -- Check that the transactions in the snapshot are ones we have seen, else wait for them to
    -- be announced
    | not (txRefs `Set.isSubsetOf` Map.keysSet (hsTxsSig s)) = DecWait (return ())
    | otherwise =
        DecApply
            (return s')
            (TPSnSig snapN (hcNodeId conf))
            ( do
                sig <- (ms_sig_sn . hcMSig $ conf) (hsSK s) (snapN, snoO (hsSnapSig s'))
                return $ SendTo peer (SigAckSn snapN sig)
            )
  where
    lastSn = hsSnapNSig s
    s' =
        s
            { hsSnapNSig = snapN
            , hsSnapSig = snObj s snapN (snoO $ hsSnapConf s) txRefs
            }
handleMessage conf peer s (SigAckSn snapN sig)
    | snapN /= hsSnapNSig s =
        DecInvalid
            (return ())
            ( "Got signature for snapshot " ++ show snapN
                ++ " when signed snapshot was "
                ++ show (hsSnapNSig s)
            )
    | hcLeaderFun conf snapN /= hcNodeId conf =
        DecInvalid
            (return ())
            ( "Received signature for snapshot " ++ show snapN
                ++ " which was not ours to create"
            )
    | sig `Set.member` snoS snob =
        DecInvalid
            (return ())
            ( "Received signature " ++ show sig ++ " for snapshot "
                ++ show snapN
                ++ ", which we already had."
            )
    | otherwise =
        DecApply
            (return $ s{hsSnapSig = snob'})
            (TPSnAck snapN peer)
            ( if Set.size (snoS snob') < Set.size (hsVKs s)
                then pure SendNothing
                else do
                    asig <- (ms_asig_sn . hcMSig $ conf) (snapN, snoO snob') (hsVKs s) (snoS snob')
                    return $ Multicast (SigConfSn snapN asig)
            )
  where
    snob = hsSnapSig s
    snob' = snob{snoS = sig `Set.insert` snoS snob}
handleMessage conf _peer s (SigConfSn snapN asig)
    | snapN /= hsSnapNSig s =
        DecInvalid
            (return ())
            ( "Can't confirm snapshot " ++ show snapN
                ++ " when last signed snapshot is "
                ++ show (hsSnapNSig s)
            )
    | hsSnapNSig s == hsSnapNConf s =
        DecInvalid
            (return ())
            ("Trying to confirm " ++ show snapN ++ " but it is leady confirmed.")
    | not isValid =
        DecInvalid
            (void validComp)
            ("Invalid aggregate signature " ++ show asig ++ ", " ++ show avk ++ " in SigConfSn")
    | otherwise =
        DecApply
            ( do
                void validComp
                return s'
            )
            (TPSnConf snapN)
            (pure SendNothing)
  where
    avk = ms_avk $ hsVKs s
    snob = hsSnapSig s
    snob' = snob{snoSigma = Just asig}
    validComp = (ms_verify_sn . hcMSig $ conf) avk (snapN, snoO snob) asig
    isValid = unComp validComp
    s' =
        s
            { hsSnapNConf = snapN
            , hsSnapSig = snob'
            , hsSnapConf = snob
            , hsTxsSig = hsTxsSig s Map.\\ reach (hsTxsSig s) (snoT snob')
            }
handleMessage _ _ _ msg =
    DecInvalid
        (return ())
        ("Trying to  handle message " ++ show msg ++ " but it is unsupported by coordinated vanilla protocol.")
