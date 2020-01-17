{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NamedFieldPuns #-}
module HydraSim.Channel
  ( Channel (..),
    mvarsAsChannel,
    createConnectedChannels,
    createConnectedBoundedChannels,
    createConnectedDelayChannels
  ) where

import Control.Monad (when)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadTime
import Control.Monad.Class.MonadTimer
import Data.Time.Clock (DiffTime,
                         diffTimeToPicoseconds,
                         picosecondsToDiffTime)
import HydraSim.Sized
import Numeric.Natural
import System.Random (RandomGen(..), Random(..))

data Channel m a = Channel {
  send :: a -> m (),
  recv :: m (Maybe a)
  }

-- | Make a 'Channel' from a pair of 'TMVar's, one for reading and one for
-- writing.
--
mvarsAsChannel :: MonadSTM m
               => TMVar m a
               -> TMVar m a
               -> Channel m a 
mvarsAsChannel bufferRead bufferWrite =
    Channel{send, recv}
  where
    send x = atomically (putTMVar bufferWrite x)
    recv   = atomically (Just <$> takeTMVar bufferRead)


-- | Create a pair of channels that are connected via one-place buffers.
--
-- This is primarily useful for testing protocols.
--
createConnectedChannels :: MonadSTM m => m (Channel m a, Channel m a)
createConnectedChannels = do
    -- Create two TMVars to act as the channel buffer (one for each direction)
    -- and use them to make both ends of a bidirectional channel
    bufferA <- atomically $ newEmptyTMVar
    bufferB <- atomically $ newEmptyTMVar

    return (mvarsAsChannel bufferB bufferA,
            mvarsAsChannel bufferA bufferB)

-- | Create a pair of channels that are connected via a bounded queue.
--
-- This variant /blocks/ when 'send' would exceed the maximum buffer size.
-- Use this variant when you want the environment rather than the 'Peer' to
-- limit the pipelining.
--
-- This is primarily useful for testing protocols.
--
createConnectedBoundedChannels :: MonadSTM m
                               => Natural -> m (Channel m a, Channel m a)
createConnectedBoundedChannels sz = do
    -- Create two TBQueues to act as the channel buffers (one for each
    -- direction) and use them to make both ends of a bidirectional channel
    bufferA <- atomically $ newTBQueue sz
    bufferB <- atomically $ newTBQueue sz

    return (queuesAsChannel bufferB bufferA,
            queuesAsChannel bufferA bufferB)
  where
    queuesAsChannel bufferRead bufferWrite =
        Channel{send, recv}
      where
        send x = atomically (writeTBQueue bufferWrite x)
        recv   = atomically (Just <$> readTBQueue bufferRead)

-- | Create a pair of channels that are connected via buffers with delays.
--
-- This is a crude approximation of asynchronous network connections where
-- there is delay across the connection, and a limit on in-flight data.
--
-- The buffer size is bounded. It /blocks/ when 'send' would exceed the maximum
-- buffer size. The size per channel element is provided by a function, so this
-- can be size in bytes or a fixed size (e.g. 1 per element), so the max size
-- should be interpreted accordingly.
--
-- The delays are modeled in a simplistic "GSV" style:
--
-- * The G is the minimum latency for a 0 sized message.
-- * The S is the time per message size unit.
-- * The V is the variance in latency, which is assumed to be uniform in the
--   range @0..v@.
--
-- The sender is delayed by S. The receiver is delayed until the arrival time
-- which is G + S + V.
--
-- Note that this implementation does not handle the delays correctly if there
-- are multiple writers or multiple readers.
--
-- This is primarily useful for testing protocols.
--
-- Taken from a PR for the typed-protocols library by Duncan.
createConnectedDelayChannels
  :: forall m prng a.
     (MonadSTM m, MonadTime m, MonadTimer m,
      RandomGen prng,
      Sized a)
  => Int          -- ^ Max size of data in-flight
  -> (DiffTime, DiffTime, DiffTime) -- ^ GSV
  -> prng         -- ^ PRNG for sampling V
  -> m (Channel m a, Channel m a)
createConnectedDelayChannels maxsize (g, s, v) prng0 = do
    let (prngA, prngB) = split prng0
    -- For each direction, create:
    --  - a TQueue for the messages
    --  - a TVar Int to track the size in-flight
    --  - a TVar prng for the sampling from v
    bufferA <- atomically $ (,,) <$> newTQueue <*> newTVar 0 <*> newTVar prngA
    bufferB <- atomically $ (,,) <$> newTQueue <*> newTVar 0 <*> newTVar prngB

    return (asChannel bufferB bufferA,
            asChannel bufferA bufferB)
  where
    asChannel (rBuffer, rSize, _rPRNG)
              (wBuffer, wSize,  wPRNG) =
        Channel{send, recv}
      where
        send x = do
          atomically $ do
            sz <- readTVar wSize
            let !sz' = sz + size x
            check (sz' <= maxsize)
            writeTVar wSize sz'
          threadDelay (s * fromIntegral (size x))
          now <- getMonotonicTime
          atomically $ do
            prng <- readTVar wPRNG
            let (vsample, prng') = randomR (0, diffTimeToPicoseconds v) prng
                delay :: DiffTime
                delay = g + picosecondsToDiffTime vsample
                arrive = delay `addTime` now
            writeTVar wPRNG prng'
            writeTQueue wBuffer (arrive, x)

        recv = do
          (arrive, x) <- atomically $ readTQueue rBuffer
          now <- getMonotonicTime
          let delay = arrive `diffTime` now
          when (delay > 0) (threadDelay delay)
          atomically $ do
            sz <- readTVar rSize
            let !sz' = sz - size x
            writeTVar rSize sz'
          return (Just x)
