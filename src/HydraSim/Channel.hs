{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HydraSim.Channel (
    Channel (..),
    mvarsAsChannel,
    createConnectedChannels,
    createConnectedBoundedChannels,
    createConnectedDelayChannels,
    createConnectedConstDelayChannels,
) where

import Control.Monad (forever, void, when)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadTime
import Control.Monad.Class.MonadTimer
import Data.Time.Clock (
    diffTimeToPicoseconds,
    picosecondsToDiffTime,
 )
import Numeric.Natural
import System.Random (Random (..), RandomGen (..))

data Channel m a = Channel
    { send :: a -> m ()
    , recv :: STM m a
    }

{- | Make a 'Channel' from a pair of 'TMVar's, one for reading and one for
 writing.
-}
mvarsAsChannel ::
    MonadSTM m =>
    TMVar m a ->
    TMVar m a ->
    Channel m a
mvarsAsChannel bufferRead bufferWrite =
    Channel{send, recv}
  where
    send x = atomically (putTMVar bufferWrite x)
    recv = takeTMVar bufferRead

{- | Create a pair of channels that are connected via one-place buffers.

 This is primarily useful for testing protocols.
-}
createConnectedChannels :: MonadSTM m => m (Channel m a, Channel m a)
createConnectedChannels = do
    -- Create two TMVars to act as the channel buffer (one for each direction)
    -- and use them to make both ends of a bidirectional channel
    bufferA <- newEmptyTMVarIO
    bufferB <- newEmptyTMVarIO

    return
        ( mvarsAsChannel bufferB bufferA
        , mvarsAsChannel bufferA bufferB
        )

{- | Create a pair of channels that are connected via a bounded queue.

 This variant /blocks/ when 'send' would exceed the maximum buffer size.
 Use this variant when you want the environment rather than the 'Peer' to
 limit the pipelining.

 This is primarily useful for testing protocols.
-}
createConnectedBoundedChannels ::
    MonadSTM m =>
    Natural ->
    m (Channel m a, Channel m a)
createConnectedBoundedChannels sz = do
    -- Create two TBQueues to act as the channel buffers (one for each
    -- direction) and use them to make both ends of a bidirectional channel
    bufferA <- atomically $ newTBQueue sz
    bufferB <- atomically $ newTBQueue sz

    return
        ( queuesAsChannel bufferB bufferA
        , queuesAsChannel bufferA bufferB
        )
  where
    queuesAsChannel bufferRead bufferWrite =
        Channel{send, recv}
      where
        send x = atomically (writeTBQueue bufferWrite x)
        recv = readTBQueue bufferRead

{- | Create a pair of channels that are connected via buffers with delays.

 This is an approximation of asynchronous network connections where
 there is delay across the connection.

 The buffer is unbounded.

 The delays are modeled in a simplistic "GV" style:

 * The G is the minimum latency for a 0 sized message.
 * The V is the variance in latency, which is assumed to be uniform in the
   range @0..v@.

 The sender is not delayed -- this has to be modeled separately (see
 'HydraSim.Multiplexer'). The receiver is delayed until the arrival time which
 is G + V.

 Message ordering is enforced via an intermediate queue. Note that this might
 cause messages to arrive later than after G + V.

 Note that this implementation does not handle the delays correctly if there
 are multiple writers or multiple readers.

 This is primarily useful for testing protocols.
-}
createConnectedDelayChannels ::
    forall m prng a.
    (MonadAsync m, MonadTime m, MonadTimer m, RandomGen prng) =>
    -- | GV
    (DiffTime, DiffTime) ->
    -- | PRNG for sampling V
    prng ->
    m (Channel m a, Channel m a)
createConnectedDelayChannels (g, v) prng0 = do
    let (prngA, prngB) = split prng0
    -- For each direction, create:
    --  - an intermediate  TQueue for the messages and their arrival time
    --  - a TQueue for the messages
    --  - a TVar prng for the sampling from v
    bufferA <- atomically $ (,,) <$> newTQueue <*> newTQueue <*> newTVar prngA
    bufferB <- atomically $ (,,) <$> newTQueue <*> newTQueue <*> newTVar prngB

    void . async $ propagate bufferA
    void . async $ propagate bufferB

    return
        ( asChannel bufferB bufferA
        , asChannel bufferA bufferB
        )
  where
    asChannel ::
        (TQueue m (Time, a), TQueue m a, TVar m prng) ->
        (TQueue m (Time, a), TQueue m a, TVar m prng) ->
        Channel m a
    asChannel
        (_rPrebuffer, rBuffer, _rPRNG)
        (wPrebuffer, _wBuffer, wPRNG) =
            Channel{send, recv}
          where
            -- send does not immediately write to the message buffer. Instead, it
            -- writes the message, together with its calculated arrival time, into
            -- an intermediate buffer.
            --
            -- 'propagate' will move it from the intermediate buffer to the message
            -- buffer, but not before the arrival time.
            send x = do
                now <- getMonotonicTime
                atomically $ do
                    prng <- readTVar wPRNG
                    let (vsample, prng') = randomR (0, diffTimeToPicoseconds v) prng
                        delay :: DiffTime
                        delay = g + picosecondsToDiffTime vsample
                        arrive :: Time
                        arrive = delay `addTime` now
                    writeTVar wPRNG prng'
                    writeTQueue wPrebuffer (arrive, x)

            recv = readTQueue rBuffer

    -- this takes a message from the intermediate buffer, and puts it into the
    -- message buffer. If the arrival time of the message is in the future, it
    -- delays until the arrival time before that.
    propagate :: (TQueue m (Time, a), TQueue m a, TVar m prng) -> m ()
    propagate (prebuffer, buffer, _prng) = forever $ do
        (arrive, x) <- atomically $ readTQueue prebuffer
        now <- getMonotonicTime
        let delay = arrive `diffTime` now
        when (delay > 0) (threadDelay delay)
        atomically $ writeTQueue buffer x

{- | A special case of 'createConnectedDelayChannels', without variance.

 In the 'HydraSim.Multiplexer', we introduce the variance through the
 contention in the networking interfaces, instead of it being a property of
 the network between nodes.
-}
createConnectedConstDelayChannels ::
    forall m a.
    (MonadAsync m, MonadTime m, MonadTimer m) =>
    -- | G
    DiffTime ->
    m (Channel m a, Channel m a)
createConnectedConstDelayChannels g = do
    -- For each direction, create:
    --  - an intermediate  TQueue for the messages and their arrival time
    --  - a TQueue for the messages
    bufferA <- atomically $ (,) <$> newTQueue <*> newTQueue
    bufferB <- atomically $ (,) <$> newTQueue <*> newTQueue

    void . async $ propagate bufferA
    void . async $ propagate bufferB

    return
        ( asChannel bufferB bufferA
        , asChannel bufferA bufferB
        )
  where
    asChannel ::
        (TQueue m (Time, a), TQueue m a) ->
        (TQueue m (Time, a), TQueue m a) ->
        Channel m a
    asChannel
        (_rPrebuffer, rBuffer)
        (wPrebuffer, _wBuffer) =
            Channel{send, recv}
          where
            -- send does not immediately write to the message buffer. Instead, it
            -- writes the message, together with its calculated arrival time, into
            -- an intermediate buffer.
            --
            -- 'propagate' will move it from the intermediate buffer to the message
            -- buffer, but not before the arrival time.
            send x = do
                now <- getMonotonicTime
                atomically $
                    let arrive = g `addTime` now
                     in writeTQueue wPrebuffer (arrive, x)

            recv = readTQueue rBuffer

    -- this takes a message from the intermediate buffer, and puts it into the
    -- message buffer. If the arrival time of the message is in the future, it
    -- delays until the arrival time before that.
    propagate :: (TQueue m (Time, a), TQueue m a) -> m ()
    propagate (prebuffer, buffer) = forever $ do
        (arrive, x) <- atomically $ readTQueue prebuffer
        now <- getMonotonicTime
        let delay = arrive `diffTime` now
        when (delay > 0) (threadDelay delay)
        atomically $ writeTQueue buffer x
