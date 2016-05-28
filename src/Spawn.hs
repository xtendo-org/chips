module Spawn
    ( spawn
    , Result
    , spawnTry
    , parMapIO
    , iParMapIO
    ) where

import Control.Concurrent
import Control.Exception

type Result a = Either SomeException a

spawnTry :: IO a -> IO (IO (Result a))
spawnTry m = do
    v <- newEmptyMVar
    _ <- mask $ \restore -> forkIO (try (restore m) >>= putMVar v)
    return (readMVar v)

spawn :: IO a -> IO (IO a)
spawn m = do
    r <- spawnTry m
    return (r >>= either throwIO return)

parMapIO :: (a -> IO b) -> [a] -> IO [b]
parMapIO f xs = mapM (spawn . f) xs >>= sequence

-- Parallel mapIO with interval. Instead of bursting out all threads at once,
-- this function will let one by one, each sleeping for a different span of
-- time, to avoid resource burst and hitting the rate limit.
iParMapIO :: Int -> (a -> IO b) -> [a] -> IO [b]
iParMapIO
    interval -- The interval
    f -- The IO action
    xs -- The list of arguments to be passed to each thread
    = mapM (spawn . delay f) (zip xs [interval, interval * 2 ..]) >>= sequence
  where
    delay action (arg, wait) = threadDelay wait *> action arg
