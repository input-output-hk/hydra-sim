{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Tail.Simulation.Utils (
    withLabel,
    modifyM,
    updateF,
    foldTraceEvents,
    withTMVar,
    withTMVar_,
    frequency,
) where

import Control.Exception (throw)
import Control.Monad.Class.MonadFork (
    MonadThread,
    labelThread,
    myThreadId,
 )
import Control.Monad.Class.MonadSTM (
    MonadSTM,
    TMVar,
    atomically,
    putTMVar,
    takeTMVar,
 )
import Control.Monad.Class.MonadTime (Time)
import Control.Monad.IOSim (ThreadLabel, Trace (..), TraceEvent (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (
    StateT,
    get,
    put,
    runState,
    state,
 )
import Data.Dynamic (fromDynamic)
import Data.Generics.Labels ()
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import System.Random (RandomGen, randomR)

withLabel ::
    MonadThread m =>
    String ->
    m () ->
    m ()
withLabel lbl action = do
    myThreadId >>= (`labelThread` lbl)
    action

modifyM ::
    Monad m =>
    (s -> m s) ->
    StateT s m ()
modifyM fn = do
    get >>= lift . fn >>= put

updateF ::
    (Ord k, Applicative f) =>
    k ->
    (v -> f (Maybe v)) ->
    Map k v ->
    f (Map k v)
updateF k fn =
    Map.alterF
        ( \case
            Nothing ->
                error "updateF: index out of range"
            Just v ->
                fn v
        )
        k

foldTraceEvents ::
    forall a b m st.
    (Typeable b, Show b, Monad m) =>
    ((ThreadLabel, Time, b) -> st -> m st) ->
    st ->
    Trace a ->
    m st
foldTraceEvents fn !st = \case
    Trace time threadId mThreadLabel (EventLog event) next -> do
        st' <- case (fromDynamic @b event, mThreadLabel) of
            (Just b, Nothing) ->
                error $ "unlabeled thread " <> show threadId <> " in " <> show b
            (Just b, Just threadLabel) ->
                fn (threadLabel, time, b) st
            (Nothing, _) ->
                pure st
        foldTraceEvents fn st' next
    Trace _time _threadId _threadLabel (EventThrow e) _next ->
        throw e
    Trace _time _threadId _threadLabel _event next ->
        foldTraceEvents fn st next
    TraceMainReturn{} ->
        pure st
    TraceMainException _ e _ ->
        throw e
    TraceDeadlock{} ->
        pure st

{- | Pick a generator from a list of generator given some weights.

 >>> frequency [(1, randomR (1, 42)), (10, randomR (42, 1337))] g
 (129, g')

 In the example above, the second generator of the list has a weigh 10x bigger than the first one, so
 it has a much greater chance to run.
-}
frequency ::
    RandomGen g =>
    [(Int, g -> (a, g))] ->
    g ->
    (a, g)
frequency generators = runState $ do
    let total = sum (fst <$> generators)
    p <- state $ randomR (1, total)
    state $ pick p generators
  where
    pick _ [] = error "frequency: empty list of generators"
    pick p ((w, gen) : rest)
        | p <= w = gen
        | otherwise = pick (p - w) rest

withTMVar_ ::
    MonadSTM m =>
    TMVar m a ->
    (a -> m a) ->
    m ()
withTMVar_ var action =
    withTMVar var (fmap ((),) . action)

withTMVar ::
    MonadSTM m =>
    TMVar m a ->
    (a -> m (result, a)) ->
    m result
withTMVar var action = do
    a <- atomically (takeTMVar var)
    (result, a') <- action a
    atomically (putTMVar var a')
    return result
