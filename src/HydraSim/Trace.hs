module HydraSim.Trace
  (
    TraceHydraEvent (..)
  ) where

import HydraSim.Multiplexer.Trace
import HydraSim.Types

-- | Traces in the simulation
data TraceHydraEvent tx =
    HydraMessage (TraceMultiplexer (HeadProtocol tx))
  | HydraProtocol (TraceProtocolEvent tx)
  | HydraDebug String
  | HydraState (HState tx)
  deriving (Show, Eq)
