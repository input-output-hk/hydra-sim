-- |
-- Module: MSig.Mock
-- Description: Mock multi signatures
--
-- A mock implementation of the multi signature scheme in section 2.1 in the Hydra paper.
--
-- We choose a simple representation, where private and public keys, as well as
-- signature, are just numbers, and a signature by a key is considered valid iff
-- the numbers are the same (and extend this to sets of keys). Obviously, this is
-- not safe in any way, but for simulations, it is just fine, and makes for
-- readable traces.
--
-- We use 'Delayedcomp' to account for time that real cryptographic primitives
-- might take.
module HydraSim.MSig.Mock
  ( VKey (..),
    SKey (..),
    Sig,
    AVKey,
    ASKey,
    ASig,
    ms_avk,
    ms_sign_delayed,
    ms_asig_delayed,
    ms_verify_delayed,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock (DiffTime)
import HydraSim.DelayedComp
import HydraSim.Sized

newtype VKey = VKey Int deriving (Eq, Ord, Show)

newtype SKey = SKey Int deriving (Eq, Ord, Show)

newtype Sig = Sig Int deriving (Eq, Ord, Show)

instance Sized Sig where
  size _ = 66

newtype AVKey = AVKey (Set VKey) deriving (Eq, Ord, Show)

newtype ASKey = ASKey (Set SKey) deriving (Eq, Ord, Show)

newtype ASig = ASig (Set Sig) deriving (Eq, Ord, Show)

instance Sized ASig where
  size _ = 66

ms_avk :: Set VKey -> AVKey
ms_avk = AVKey

ms_sign_delayed :: DiffTime -> SKey -> a -> DelayedComp Sig
ms_sign_delayed delay (SKey key) _message = delayedComp (Sig key) delay

ms_asig_delayed :: DiffTime -> a -> Set VKey -> Set Sig -> DelayedComp ASig
ms_asig_delayed delay _message _vkeys sigs = delayedComp (ASig sigs) delay

ms_verify_delayed :: DiffTime -> AVKey -> a -> ASig -> DelayedComp Bool
ms_verify_delayed delay (AVKey vkeys) _message (ASig sigs) =
  let missingSigs = Set.filter (\(VKey i) -> not $ (Sig i) `Set.member` sigs) vkeys
      isValid = Set.null missingSigs
   in delayedComp isValid delay
