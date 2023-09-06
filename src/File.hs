module File
  ( basename
  , copyFile
  ) where

import Control.Monad
import Control.Exception

import System.IO

import Foreign.Marshal.Alloc (allocaBytes)
import System.Posix.ByteString

import qualified Data.ByteString as B

copyFile :: RawFilePath -> RawFilePath -> IO ()
copyFile srcPath tgtPath = do
    bracket ropen hClose $ \ hi ->
        bracket topen hClose $ \ ho ->
            allocaBytes bufferSize $ copyContents hi ho
    rename tmpPath tgtPath
  where
    ropen = openFd srcPath ReadOnly defaultFlags >>= fdToHandle
    topen = createFile tmpPath stdFileMode >>= fdToHandle
    tmpPath = tgtPath <> ".copyFile.tmp"
    copyContents hi ho buffer = do
        count <- hGetBuf hi buffer bufferSize
        when (count > 0) $ do
            hPutBuf ho buffer count
            copyContents hi ho buffer

-- Buffer size for file copy
bufferSize :: Int
bufferSize = 4096

defaultFlags :: OpenFileFlags
defaultFlags = OpenFileFlags
    { append = False
    , creat = Just stdFileMode
    , exclusive = False
    , noctty = True
    , nonBlock = False
    , trunc = False
    , nofollow = False
    , cloexec = False
    , directory = False
    , sync = False
    }

basename :: RawFilePath -> RawFilePath
basename = snd . B.spanEnd (0x2F /=)
