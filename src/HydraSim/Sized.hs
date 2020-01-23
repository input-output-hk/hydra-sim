{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HydraSim.Sized
  ( Size (..), Sized (..)
  ) where

newtype Size = Size { unSize :: Int}
  deriving (Num, Eq, Ord, Show, Real, Enum, Integral)

-- | Data that has a certain size in bytes.
--
-- Used to model delay when sending messages over a network.
class Sized a where
  size :: a -> Size 

instance Sized Int where size _ = Size 8
