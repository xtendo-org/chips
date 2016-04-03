module Utility where

import Control.Monad

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B

import System.Exit
import System.Process
import System.IO
import System.IO.Error

import Lib.Directory

-- utility

silentCall :: String -> [String] -> IO ExitCode
silentCall cmd args = do
    (_, _, _, procH) <- createProcess (proc cmd args)
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
    waitForProcess procH

readProcessB :: String -> [String] -> IO ByteString
readProcessB cmd args = do
    (_, Just outH, _, procH) <- createProcess (proc cmd args)
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
    readB <- B.hGetContents outH
    _ <- waitForProcess procH
    return readB

bPutStr :: Builder -> IO ()
bPutStr = B.hPutBuilder stdout

lPutStr :: [Builder] -> IO ()
lPutStr = bPutStr . mconcat

-- A function that "tries" to remove a file.
-- If the file does not exist, nothing happens.
tryRemoveFile :: FilePath -> IO ()
tryRemoveFile path = catchIOError (removeFile path) $
    \ e -> unless (isDoesNotExistError e) $ ioError e
