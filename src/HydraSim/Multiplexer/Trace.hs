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
  | MPSendSelf NodeId a
  | MPReenqueue NodeId a
  | MPMulticast a
  deriving (Eq, Show)
