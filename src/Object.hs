{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Object where

import Data.Kind (Type)
import Data.Time.Clock (DiffTime)

data ObjectValidity =
  ObjectValid
  | ObjectInvalid String
  deriving (Eq, Show)

-- | An Object in Hydra.
--
-- An oject can be either a transaction or a snapshot.
class (Show (ORef o), Eq (ORef o), Ord (ORef o)) => Object o where
  -- | Reference to the object.
  -- For (real) transactions this will be a hash. For snapshots, it will be the sequence number.
  type ORef o :: Type
  oRef :: o -> ORef o

  -- | Context that is needed to validate an object
  data OValidationContext o :: Type
  -- | Validate an object.
  --
  -- In addition to the validity, this function can also return a 'DiffTime', to
  -- artificially delay the thread that performed the validation if we are
  -- mocking the validation.
  oValidate :: OValidationContext o -> o -> (ObjectValidity, Maybe DiffTime)
