module HydraSim.Multiplexer.Exception (
  MultiplexerException (..),
) where

import Control.Exception (Exception)
import HydraSim.Types (NodeId)

data MultiplexerException
  = MissingChannel NodeId
  deriving (Show)
instance Exception MultiplexerException
