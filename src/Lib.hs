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
    { fishPath :: FilePath
    , chipsPath :: FilePath
    , chipsConf :: C.Config
    }

-- main execution functions

app :: IO ()
app = do
    B.putStr greetMsg
    fPath <- getAppDirectory "fish"
    cPath <- getAppDirectory "chips"
    -- TODO: if plugin.yaml does not exist, create one with the template,
    -- and add "source" in config.fish
    conf <- C.decode (cPath </> "plugin.yaml")
    runSync Session
        { chipsPath = cPath
        , chipsConf = conf
        , fishPath = fPath
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
    maybe (return ()) (`copyFileReport` fishPromptPath) $
        listToMaybe $ mapMaybe plugPrompt plugResults
    maybe (return ()) (`copyFileReport` fishRightPath) $
        listToMaybe $ mapMaybe plugRightPrompt plugResults
  where
    pluginsDir = chipsPath </> "dist"
    buildPath = chipsPath </> "build.fish"
    fishPromptPath = fishPath </> "functions" </> "fish_prompt.fish"
    fishRightPath = fishPath </> "functions" </> "fish_right_prompt.fish"
    sourceInit initPath = mconcat
        [ "source "
        , B.stringUtf8 $ pluginsDir </> initPath
        , "\n"
        ]
    copyFileReport :: FilePath -> FilePath -> IO ()
    copyFileReport x y = x `copyFile` y >>
        lPutStr ["Copied ", B.stringUtf8 x, " to ", B.stringUtf8 y, "\n"]

installPlug :: Bool -> Text -> IO Plugin
installPlug pluginsExist url = do
    plugDirExists <- if not pluginsExist
        then return False
        else doesDirectoryExist dir
    if plugDirExists then
        lPutStr [builderDir, " is already installed.\n"]
        -- TODO: do the update here
    else do
        lPutStr ["Installing ", builderDir, "...\n"]
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

lPutStr :: [Builder] -> IO ()
lPutStr = bPutStr . mconcat

greetMsg :: ByteString
greetMsg = "chips: fish plugin manager\n\n"
