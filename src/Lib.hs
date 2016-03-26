module Lib
    ( app
    ) where

import Data.Maybe
import Data.Monoid
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Base64.URL as B64

import System.Exit
import System.Process
import System.IO
import System.FilePath

import Lib.Directory
import Spawn (parMapIO)

import qualified Config as C
import Git

data Plugin = Plugin
    { plugInit :: Maybe FilePath
    , plugPrompt :: Maybe FilePath
    , plugRightPrompt :: Maybe FilePath
    }

-- Session configuration container. In other words,
-- things that are set up in the beginning of execution and hardly change
data Session = Session
    { chipsPath :: FilePath
    , chipsConf :: C.Config
    }

-- main execution functions

app :: IO ()
app = do
    B.putStr greetMsg
    fpath <- getAppDirectory "chips"
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
    withFile buildPath WriteMode $ \handle -> B.hPutBuilder handle $
        mconcat $ map sourceInit $ mapMaybe plugInit plugResults
    bPutStr $ "Build result saved at " <> B.stringUtf8 buildPath <> "\n"
  where
    pluginsDir = chipsPath </> "dist"
    buildPath = chipsPath </> "build.fish"
    sourceInit initPath = mconcat
        [ "source "
        , B.stringUtf8 $ pluginsDir </> initPath
        , "\n"
        ]

installPlug :: Bool -> Text -> IO Plugin
installPlug pluginsExist url = do
    plugDirExists <- if not pluginsExist
        then return False
        else doesDirectoryExist dir
    if plugDirExists then
        bPutStr $ builderDir <> " is already installed.\n"
        -- TODO: do the update here
    else do
        bPutStr $ "Installing " <> builderDir <> "...\n"
        cloned <- silentCall
            "git" ["clone", "--depth=1", T.unpack url, dir]
        bPutStr $ (<> builderDir <> ".\n") $ case cloned of
            ExitSuccess -> "Installed "
            _ -> "Failed installing "
    initExists <- doesFileExist initFishPath
    promptExists <- doesFileExist promptPath
    rightExists <- doesFileExist rightPath
    return Plugin
        { plugInit = if initExists then Just initFishPath else Nothing
        , plugPrompt = if promptExists then Just promptPath else Nothing
        , plugRightPrompt = if rightExists then Just rightPath else Nothing
        }
  where
    dir :: FilePath
    dir = B.unpack $
        maybe (B64.encode $ T.encodeUtf8 url) T.encodeUtf8 (gitDir url)
    builderDir :: Builder
    builderDir = B.stringUtf8 dir
    initFishPath = dir </> "init.fish"
    promptPath = dir </> "fish_prompt.fish"
    rightPath = dir </> "fish_right_prompt.fish"

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
greetMsg = "chips: fish plugin manager\n\n"
