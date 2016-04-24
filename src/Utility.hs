module Utility where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy as LB (toStrict)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B

import System.IO

bPutStr :: Builder -> IO ()
bPutStr = B.hPutBuilder stdout

lPutStr :: [Builder] -> IO ()
lPutStr = bPutStr . mconcat

toByteString :: Builder -> ByteString
toByteString = LB.toStrict . B.toLazyByteString

byteString :: String -> ByteString
byteString = toByteString . B.stringUtf8
