-- The functions provided by this module is already implemented in the latest
-- version of the "directory" package, but it can't be used with the current
-- version of Stack. See https://github.com/fpco/stackage/issues/1244
module Lib.Directory
    ( module System.Directory
    , getAppDirectory
    ) where

import System.Directory
import System.FilePath

getAppDirectory :: FilePath -> IO FilePath
getAppDirectory app = fmap (</> ".config" </> app) getHomeDirectory
