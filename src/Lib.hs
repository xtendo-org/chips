module Lib
    ( app
    ) where

import Data.List
import Data.Maybe
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
import System.Environment
import System.Directory
import System.Process
import System.IO
import System.FilePath

import Spawn (parMapIO)

import qualified Config as C
import Git

data Plugin = Plugin ByteString | Theme

-- Session configuration container. In other words,
-- things that are set up in the beginning of execution and hardly change
data Session = Session
    { chipsPath :: FilePath
    , chipsConf :: C.Config
    }

-- main execution functions

app :: IO ()
app = do
    -- Primitive command line argument processing to check -h or --help
    args <- getArgs
    B.putStrLn greetMsg
    fpath <- getAppUserDataDirectory "chips"
    -- TODO: if plugin.yaml does not exist, create one with the template,
    -- and add "source" in config.fish
    conf <- C.decode (fpath </> "plugin.yaml")
    runSync Session
        { chipsPath = fpath
        , chipsConf = conf
        }

runSync :: Session -> IO ()
runSync Session{..} = do
    pluginsExist <- doesDirectoryExist pluginsDir
    unless pluginsExist $ createDirectoryIfMissing True pluginsDir
    setCurrentDirectory pluginsDir
    -- Loop over each entry of plugin.yaml
    plugResults <- parMapIO (installPlug pluginsExist) $ C.gitURLs chipsConf
    withFile buildPath WriteMode $ \handle ->
        B.hPutBuilder handle $ mconcat $ sourcePaths plugResults
    bPutStr $ "Build result saved at " <> B.stringUtf8 buildPath <> "\n"
  where
    pluginsDir = chipsPath </> "dist"
    buildPath = chipsPath </> "build.fish"
    installPlug pluginsExist url = do
        pluginDirExist <- if not pluginsExist
            then return False
            else doesDirectoryExist $ B.unpack dir
        if pluginDirExist then
            bPutStr $ B.byteString dir <> " is already installed.\n"
            -- TODO: do the update here
        else do
            let builderDir = B.byteString dir
            bPutStr $ "Installing " <> builderDir <> "...\n"
            cloned <- silentCall
                "git" ["clone", "--depth=1", T.unpack url, B.unpack dir]
            case cloned of
                ExitSuccess -> bPutStr $ "Installed " <> builderDir <> ".\n"
                _ -> B.putStrLn "Fail."
        doesFileExist (B.unpack dir </> "init.fish") >>= \case
            True -> return (Plugin dir)
            False -> return Theme
      where
        dir = maybe (B64.encode $ T.encodeUtf8 url) T.encodeUtf8 (gitDir url)
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

greetMsg :: ByteString
greetMsg = "\
  \chips: fish plugin manager\
\\n=========================="
