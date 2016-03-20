module Spawn
    ( spawn
    , Result
    , spawnTry
    , parMapIO
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
