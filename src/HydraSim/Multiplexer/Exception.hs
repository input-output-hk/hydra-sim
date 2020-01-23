module HydraSim.Multiplexer.Exception
  ( MultiplexerException (..)
  ) where

import HydraSim.Types (NodeId)
import Control.Exception (Exception)

data MultiplexerException =
    MissingChannel NodeId
    deriving Show
instance Exception MultiplexerException
