module HydraSim.Multiplexer.Trace
  ( TraceMultiplexer (..)
  ) where

import HydraSim.Types (NodeId)
import HydraSim.Sized

data TraceMultiplexer a =
    MPSendLeading NodeId Size
  | MPSendTrailing NodeId a
  | MPRecvLeading NodeId Size
  | MPRecvIdling
  | MPRecvTrailing NodeId a
  deriving (Eq, Show)
