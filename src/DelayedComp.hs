{-# LANGUAGE DeriveFunctor #-}
module DelayedComp
  ( DelayedComp,

    delayedComp, promptComp,
    runComp
  ) where

import Control.Monad (liftM2)
import Data.Time.Clock (DiffTime)


import Control.Monad.Class.MonadTimer


-- | A computation that might take a non-neglible amount of time.
data DelayedComp a = DelayedComp
  { -- | The valueproduced by the computation
    unComp :: a,
    -- | In simulations, we allow for an explicit delay to replace an actual
    -- computation. This allows us to mock things like scripts and cryptographic
    -- primitives.
    compDelay :: Maybe DiffTime
  }
  deriving (Show, Functor)

instance Applicative DelayedComp where
  pure = promptComp
  (DelayedComp f d) <*> (DelayedComp x d') = DelayedComp (f x) (liftM2 (+) d d')

instance Monad DelayedComp where
  (DelayedComp x d) >>= f =
    let (DelayedComp x' d') = f x
    in DelayedComp x' (liftM2 (+) d d')

-- | A computation that produces a given value after a given time.
delayedComp :: a -> DiffTime -> DelayedComp a
delayedComp x d = DelayedComp x (Just d)

-- | A computation that produces a given value immediately.
promptComp :: a -> DelayedComp a
promptComp x = DelayedComp x Nothing

-- | Run a computation (i.e., wait for the proper time, and yield the result).
runComp :: MonadTimer m => DelayedComp a -> m a
runComp (DelayedComp x (Just d)) = threadDelay d >> pure x
runComp (DelayedComp x Nothing) = pure x
