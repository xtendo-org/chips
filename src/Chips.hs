-- chips-specific codes
module Chips where

import Paths_chips
import Data.Version

import Data.List

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B

import Utility

chipsVer :: ByteString
chipsVer = toByteString $
    mconcat $ intersperse "." $ map B.intDec $ versionBranch version
