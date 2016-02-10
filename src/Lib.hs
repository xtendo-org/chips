module Lib
    ( app
    ) where

import Data.Monoid

import System.Exit
import System.Environment

import qualified Config as C

getHomePath :: IO FilePath
getHomePath = lookupEnv "HOME" >>= maybe (die msgNoHome) return
  where
    msgNoHome = "Environmental variable HOME is missing."

app :: IO ()
app = do
    conf <- (<> "/.fplug/fplug.yaml") <$> getHomePath >>= C.decode
    print (C.gitURLs conf)
