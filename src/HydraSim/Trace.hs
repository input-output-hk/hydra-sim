module HydraSim.Trace
  (
    TraceHydraEvent (..),
    TraceMessagingEvent (..)
  ) where

import HydraSim.Multiplexer.Trace
import HydraSim.Types

-- | Traces in the simulation
data TraceHydraEvent tx =
    HydraMessage (TraceMessagingEvent tx)
  | HydraProtocol (TraceProtocolEvent tx)
  | HydraDebug String
  | HydraMultiplexer (TraceMultiplexer (HeadProtocol tx))
  deriving (Eq, Show)

-- | Tracing messages that are sent/received between nodes.
data TraceMessagingEvent tx =
    TraceMessageSent NodeId (HeadProtocol tx)
  | TraceMessageMulticast (HeadProtocol tx)
  | TraceMessageClient (HeadProtocol tx)
  | TraceMessageReceived NodeId (HeadProtocol tx)
  | TraceMessageRequeued (HeadProtocol tx)
  deriving (Eq, Show)

