module Lib
    ( app
    ) where

import Data.Monoid
import Control.Monad

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Base64.URL as B64

import System.Exit
import System.Directory
import System.Process
import System.IO
import System.FilePath

import qualified Config as C
import Args
import Git

data Plugin = Plugin ByteString | Theme

-- Session configuration container. In other words,
-- things that are set up in the beginning of execution and hardly change
data Session = Session
    { fplugPath :: FilePath
    }

-- main execution functions

app :: IO ()
app = do
    fpath <- getAppUserDataDirectory "fplug"
    getCmd >>= runCmd Session
        { fplugPath = fpath
        }

runCmd :: Session -> Cmd -> IO ()
runCmd session cmd = case cmd of
    CmdInit -> die "init not implemented yet"
    CmdInstall _ -> runInstall session
    CmdRemove _ -> die "remove not implemented yet"
    CmdUpgrade -> die "install not implemented yet"
    CmdClean -> die "clean not implemented yet"

runInstall :: Session -> IO ()
runInstall Session{..} = do
    conf <- C.decode (fplugPath </> "fplug.yaml")
    pluginsExist <- doesDirectoryExist pluginsDir
    unless pluginsExist $ createDirectoryIfMissing True pluginsDir
    setCurrentDirectory pluginsDir
    initPaths <- forM (C.gitURLs conf) $ \ url -> do
        let dir = dirB url
        pluginDirExist <- if not pluginsExist
            then return False
            else doesDirectoryExist $ B.unpack dir
        if pluginDirExist then
            bPutStr $ B.byteString dir <> " is already installed.\n"
        else do
            bPutStr $ "Installing " <> B.byteString dir <> "... "
            cloned <- silentCall
                "git" ["clone", "--depth=1", T.unpack url, B.unpack dir]
            case cloned of
                ExitSuccess -> B.putStrLn "Done."
                _ -> B.putStrLn "Fail."
        doesFileExist (B.unpack dir </> "init.fish") >>= \case
            True -> return (Plugin dir)
            False -> return Theme
    withFile (fplugPath </> "build.fish") WriteMode $ \handle ->
        B.hPutBuilder handle $ mconcat $ sourcePaths initPaths
  where
    pluginsDir = fplugPath </> "plugins"
    dirB url = maybe (B64.encode $ T.encodeUtf8 url) T.encodeUtf8
        (gitDir url)
    sourcePaths [] = []
    sourcePaths (p : ps) = case p of
        Plugin dir -> pathExpand dir : sourcePaths ps
        Theme -> sourcePaths ps
      where
        pathExpand dir = mconcat
            [ "source "
            , B.stringUtf8 $ pluginsDir </> B.unpack dir </> "init.fish"
            , "\n"
            ]

-- utility

silentCall :: String -> [String] -> IO ExitCode
silentCall cmd args = do
    (_, _, _, procH) <- createProcess (proc cmd args)
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
    waitForProcess procH

bPutStr :: Builder -> IO ()
bPutStr = B.hPutBuilder stdout
