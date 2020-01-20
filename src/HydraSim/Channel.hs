{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NamedFieldPuns #-}
module HydraSim.Channel
  ( Channel (..),
    mvarsAsChannel,
    createConnectedChannels,
    createConnectedBoundedChannels,
    createConnectedBoundedDelayedChannels,
    createConnectedBoundedVariantDelayedChannels
  ) where

import Control.Monad (void)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadTimer
import Data.Time.Clock (DiffTime,
                        diffTimeToPicoseconds,
                        picosecondsToDiffTime)
import Numeric.Natural
import System.Random (RandomGen(..), Random(..))

data Channel m a = Channel {
  send :: a -> m (),
  recv :: STM m a
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
    recv   = takeTMVar bufferRead


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
        recv   = readTBQueue bufferRead

-- | Like 'createConnectedBoundedChannels', but messages will appear only after
-- a delay.
createConnectedBoundedDelayedChannels
  :: ( MonadSTM m, MonadTimer m, MonadAsync m)
  => Natural -> DiffTime -> m (Channel m a, Channel m a)
createConnectedBoundedDelayedChannels sz deltaT = do
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
        send x = void $ async $
          do
          threadDelay deltaT
          atomically (writeTBQueue bufferWrite x)
        recv   = readTBQueue bufferRead

-- | Like 'createConnectedBoundedDelayedChannels', but the delay will not be constant.
--
-- Instead, there will be a constant delay @g@, plus a random delay, taken
-- uniformly from @[0..v]@
createConnectedBoundedVariantDelayedChannels
  :: ( MonadSTM m, MonadTimer m, MonadAsync m
     , RandomGen prng)
  => Natural -> (DiffTime, DiffTime) -> prng -> m (Channel m a, Channel m a)
createConnectedBoundedVariantDelayedChannels sz (g, v) prng0 = do
    let (prngA, prngB) = split prng0

    -- For each direction, we will create a 'TBQueue' for the messages, and a
    -- 'TVar' for the pseudo random number generators.
    bufferA <- atomically $ (,) <$> newTBQueue sz <*> newTVar prngA
    bufferB <- atomically $ (,) <$> newTBQueue sz <*> newTVar prngB

    return (asChannel bufferB bufferA,
            asChannel bufferA bufferB)
  where
    asChannel (rBuffer, _rPRNG) (wBuffer, wPRNG) =
        Channel{send, recv}
      where
        send x = void $ async $ do
          prng <- atomically (readTVar wPRNG)
          let (vsample, prng') = randomR (0, diffTimeToPicoseconds v) prng
          atomically (writeTVar wPRNG prng')
          threadDelay (g + picosecondsToDiffTime vsample)
          atomically (writeTBQueue wBuffer x)
        recv   = readTBQueue rBuffer
