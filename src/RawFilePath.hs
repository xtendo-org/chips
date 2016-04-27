-- The 'unix' module provides 'RawFilePath'-variants of all functions, but
-- higher-level wrappers of it such as 'directory' or 'process' doesn't.
-- This module provides it.
--
module RawFilePath
    ( RawFilePath
    , callProcess
    , callProcessSilent
    , readProcess
    , getDirectoryContents
    , getDirectoryContentsSuffix
    , copyFile
    , getHomeDirectory
    , getAppDirectory
    , fileExist
    , changeWorkingDirectory
    , tryRemoveFile
    , (</>)
    ) where

import Data.Monoid
import Control.Monad
import Control.Exception

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import System.IO
import System.IO.Error
import System.Exit (ExitCode(..), exitFailure)

import Foreign.Marshal.Alloc (allocaBytes)
import System.Posix.ByteString

infixr 5  </>
(</>) :: RawFilePath -> RawFilePath -> RawFilePath
a </> b = mconcat [a, "/", b]

callProcess :: RawFilePath -> [ByteString] -> IO ()
callProcess cmd args = do
    pid <- forkProcess $ executeFile cmd True args Nothing
    getProcessStatus True False pid >>= \ mstatus -> case mstatus of
        Just status -> case status of
            Exited exitCode -> case exitCode of
                ExitSuccess -> return ()
                ExitFailure _ -> die cmd
            _ -> die cmd
        Nothing -> die cmd

callProcessSilent :: RawFilePath -> [ByteString] -> IO ExitCode
callProcessSilent cmd args = do
    pid <- forkProcess $ do
        closeFd stdOutput
        closeFd stdError
        executeFile cmd True args Nothing
    getProcessStatus True False pid >>= \ mstatus -> case mstatus of
        Just status -> case status of
            Exited exitCode -> return exitCode
            _ -> die cmd
        Nothing -> die cmd

readProcess
    :: RawFilePath -> [ByteString]
    -> IO (Either ByteString ByteString)
readProcess cmd args = do
    (fd0, fd1) <- createPipe
    (efd0, efd1) <- createPipe
    pid <- forkProcess $ do
        closeFd fd0
        closeFd stdOutput
        void $ dupTo fd1 stdOutput
        closeFd efd0
        closeFd stdError
        void $ dupTo efd1 stdError
        executeFile cmd True args Nothing
    closeFd fd1
    closeFd efd1
    content <- fdToHandle fd0 >>= B.hGetContents
    getProcessStatus True False pid >>= \ mstatus -> case mstatus of
        Just status -> case status of
            Exited exitCode -> case exitCode of
                ExitSuccess -> return $ Right content
                ExitFailure _ -> fmap Left $
                    fdToHandle efd0 >>= B.hGetContents
            _ -> die cmd
        Nothing -> die cmd

getDirectoryContents :: RawFilePath -> IO [RawFilePath]
getDirectoryContents dirPath = bracket open close repeatRead
  where
    open = openDirStream dirPath
    close = closeDirStream
    repeatRead stream = do
        d <- readDirStream stream
        if B.length d == 0 then return [] else do
            rest <- repeatRead stream
            return $ d : rest

getDirectoryContentsSuffix :: RawFilePath -> ByteString -> IO [RawFilePath]
getDirectoryContentsSuffix dirPath suffix = bracket open close repeatRead
  where
    open = openDirStream dirPath
    close = closeDirStream
    repeatRead stream = do
        d <- readDirStream stream
        if B.length d == 0 then return [] else do
            rest <- repeatRead stream
            return $ (if suffix `B.isSuffixOf` d then (d :) else id) rest

defaultFlags :: OpenFileFlags
defaultFlags = OpenFileFlags
    { append = False
    , exclusive = False
    , noctty = True
    , nonBlock = False
    , trunc = False
    }

-- Buffer size for file copy
bufferSize :: Int
bufferSize = 4096

copyFile :: RawFilePath -> RawFilePath -> IO ()
copyFile srcPath tgtPath = do
    bracket ropen hClose $ \ hi ->
        bracket topen hClose $ \ ho ->
            allocaBytes bufferSize $ copyContents hi ho
    rename tmpPath tgtPath
  where
    ropen = openFd srcPath ReadOnly Nothing defaultFlags >>= fdToHandle
    topen = createFile tmpPath stdFileMode >>= fdToHandle
    tmpPath = tgtPath <> ".copyFile.tmp"
    copyContents hi ho buffer = do
        count <- hGetBuf hi buffer bufferSize
        when (count > 0) $ do
            hPutBuf ho buffer count
            copyContents hi ho buffer

-- A function that "tries" to remove a file.
-- If the file does not exist, nothing happens.
tryRemoveFile :: RawFilePath -> IO ()
tryRemoveFile path = catchIOError (removeLink path) $
    \ e -> unless (isDoesNotExistError e) $ ioError e

getHomeDirectory :: IO RawFilePath
getHomeDirectory = getEnv "HOME" >>= maybe (die noHomeErrMsg) return

getAppDirectory :: RawFilePath -> IO RawFilePath
getAppDirectory app = fmap (</> ".config" </> app) getHomeDirectory

die :: ByteString -> IO a
die msg = B.putStr ("Error: " <> msg) *> exitFailure

noHomeErrMsg :: ByteString
noHomeErrMsg = "This application requires the $HOME environment variable.\n"
