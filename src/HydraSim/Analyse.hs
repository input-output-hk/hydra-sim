{-# LANGUAGE ScopedTypeVariables #-}
module HydraSim.Analyse
  (ShowDebugMessages (..),
   selectTraceHydraEvents)
where

import Control.Exception (throw)
import Control.Monad (void)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadTime
import Control.Monad.Class.MonadTimer
import Control.Monad.Class.MonadThrow
import Control.Monad.IOSim
import Control.Tracer
import Data.Dynamic
import Data.Time.Clock (picosecondsToDiffTime)
import HydraSim.Channel
import HydraSim.HeadNode
import HydraSim.MSig.Mock
import HydraSim.Sized
import HydraSim.Trace
import HydraSim.Tx.Mock

import HydraSim.Types
import System.Random (RandomGen, mkStdGen, split)

data ShowDebugMessages =
    ShowDebugMessages
  | DontShowDebugMessages
  deriving Eq

data HTrace s = HTrace {
  hTime :: Time,
  hNode :: ThreadLabel,
  hThread :: ThreadId (SimM s),
  hEv :: TraceHydraEvent MockTx
  } deriving Show

selectTraceHydraEvents
  :: ShowDebugMessages
  -> Trace a
  -> [HTrace s] -- [(Time, Maybe ThreadLabel, ThreadId (SimM s), TraceHydraEvent MockTx)]
selectTraceHydraEvents showDebugMessages = go
  where
    go (Trace t tid (Just tlab) (EventLog e) trace)
     | Just (x :: TraceHydraEvent MockTx) <- fromDynamic e    =
         case x of
           HydraDebug _ -> if showDebugMessages == ShowDebugMessages
                           then (HTrace t tlab tid x) : go trace
                           else             go trace
           _ ->                 (HTrace t tlab tid x) : go trace
    go (Trace _t tid Nothing (EventLog e) _trace)
      | Just (x :: TraceHydraEvent MockTx) <- fromDynamic e    =
          error $ "unlabeled thread " ++ show tid ++
          " in " ++ show x
    go (Trace _ _ _ _ trace)      =         go trace
    go (TraceMainException _ e _) = throw e
    go (TraceDeadlock      _   _) = [] -- expected result in many cases
    go (TraceMainReturn    _ _ _) = []

