{-# LANGUAGE TypeApplications #-}

module Hydra.Tail.Simulation.Utils
  ( withLabel
  , modifyM
  , updateF
  , foldTraceEvents
  ) where

import Control.Exception
    ( throw )
import Control.Monad.Class.MonadFork
    ( MonadThread, labelThread, myThreadId )
import Control.Monad.Class.MonadTime
    ( Time )
import Control.Monad.IOSim
    ( ThreadLabel, Trace (..), TraceEvent (..) )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.State.Strict
    ( StateT, get, put )
import Data.Dynamic
    ( fromDynamic )
import Data.Generics.Labels
    ()
import Data.Map.Strict
    ( Map )
import Data.Typeable
    ( Typeable )

import qualified Data.Map.Strict as Map

withLabel
  :: MonadThread m
  => String
  -> m ()
  -> m ()
withLabel lbl action = do
  myThreadId >>= (`labelThread` lbl)
  action

modifyM
  :: Monad m
  => (s -> m s)
  -> StateT s m ()
modifyM fn = do
  get >>= lift . fn >>= put

updateF
  :: (Ord k, Applicative f)
  => k
  -> (v -> f (Maybe v))
  -> Map k v
  -> f (Map k v)
updateF k fn =
  Map.alterF (\case
    Nothing ->
      error "updateF: index out of range"
    Just v ->
      fn v
  ) k

foldTraceEvents
  :: forall a b st. (Typeable b, Show b)
  => ((ThreadLabel, Time, b) -> st -> st)
  -> st
  -> Trace a
  -> st
foldTraceEvents fn st = \case
  Trace time threadId mThreadLabel (EventLog event) next ->
    let
      st' = case (fromDynamic @b event, mThreadLabel) of
        (Just b, Nothing) ->
          error $ "unlabeled thread " <> show threadId <> " in " <> show b
        (Just b, Just threadLabel) ->
          fn (threadLabel, time, b) st
        (Nothing, _) ->
          st
     in
      foldTraceEvents fn st' next
  Trace _time _threadId _threadLabel (EventThrow e) _next ->
    throw e
  Trace _time _threadId _threadLabel _event next ->
    foldTraceEvents fn st next
  TraceMainReturn{} ->
    st
  TraceMainException _ e _ ->
    throw e
  TraceDeadlock{} ->
    st
