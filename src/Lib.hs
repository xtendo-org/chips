module Lib
    ( app
    ) where

import Data.Monoid

import System.Exit
import System.Environment

import qualified Config as C
import Args

getHomePath :: IO FilePath
getHomePath = lookupEnv "HOME" >>= maybe (die msgNoHome) return
  where
    msgNoHome = "Environmental variable HOME is missing."

app :: IO ()
app = do
    cmd <- getCmd
    case cmd of
        CmdInit -> die "init not implemented yet"
        CmdInstall _ -> die "install not implemented yet"
        CmdRemove _ -> die "remove not implemented yet"
        CmdUpgrade -> runUpgrade
        CmdClean -> die "clean not implemented yet"

runUpgrade :: IO ()
runUpgrade = do
    conf <- (<> "/.fplug/fplug.yaml") <$> getHomePath >>= C.decode
    print (C.gitURLs conf)
