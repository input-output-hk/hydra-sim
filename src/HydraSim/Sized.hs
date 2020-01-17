module HydraSim.Sized
  ( Sized (..)
  ) where

-- | Data that has a certain size in bytes.
--
-- Used to model delay when sending messages over a network.
class Sized a where
  size :: a -> Int

instance Sized Int where size _ = 8
