module Lib
    ( app
    ) where

import Paths_chips
import Data.Version

import Data.List
import Data.Maybe
import Data.Monoid
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Base64.URL as B64

import System.Exit
import System.Process
import System.IO
import System.IO.Error
import System.FilePath

import Lib.Directory
import Spawn (parMapIO)

import qualified Config as C
import Git

data Plugin = Plugin
    { plugInit :: Maybe FilePath
    , plugPrompt :: Maybe FilePath
    , plugRight :: Maybe FilePath
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
    bPutStr greetMsg
    fPath <- getAppDirectory "fish"
    cPath <- getAppDirectory "chips"
    -- TODO: if plugin.yaml does not exist, create one with the template,
    -- and add "source" in config.fish
    let yamlPath = cPath </> "plugin.yaml"
    doesFileExist yamlPath >>= \ yamlExists -> if yamlExists then do
        conf <- C.decode yamlPath
        runSync Session
            { chipsPath = cPath
            , chipsConf = conf
            , fishPath = fPath
            }
    else do
        createDirectoryIfMissing True cPath
        T.writeFile yamlPath C.templateConfig
        lPutStr
            [ "chips has just created the default plugin configuration at: "
            , B.stringUtf8 yamlPath
            , "\nPlease edit the file and run chips again.\n"
            ]

runSync :: Session -> IO ()
runSync Session{..} = do
    pluginsExist <- doesDirectoryExist pluginsDir
    unless pluginsExist $ createDirectoryIfMissing True pluginsDir
    setCurrentDirectory pluginsDir

    -- Concurrently deal with each entry of plugin.yaml
    plugResults <- fmap catMaybes $
        parMapIO (dealPlug pluginsExist) $ C.gitURLs chipsConf

    -- Register or unregister themes
    createDirectoryIfMissing True functionsDir
    maybe (tryRemoveFile fishPromptPath) (`copyFileReport` fishPromptPath) $
        listToMaybe $ mapMaybe plugPrompt plugResults
    maybe (tryRemoveFile fishRightPath) (`copyFileReport` fishRightPath) $
        listToMaybe $ mapMaybe plugRight plugResults

    -- Create build.fish for plugins with init.fish
    withFile buildPath WriteMode $ \handle -> B.hPutBuilder handle $
        mconcat $ map sourceInit $ mapMaybe plugInit plugResults
    lPutStr ["Build result saved at ", B.stringUtf8 buildPath, "\n"]

    -- Add build.fish to config.fish
    configFish <- B.readFile configFishPath
    when (snd (B.breakSubstring sourceLine configFish) == "") $ do
        B.appendFile configFishPath sourceLine
        lPutStr ["Added to ", B.stringUtf8 configFishPath, "\n"]

  where
    configFishPath = fishPath </> "config.fish"
    pluginsDir = chipsPath </> "dist"
    buildPath = chipsPath </> "build.fish"
    functionsDir = fishPath </> "functions"
    fishPromptPath = functionsDir </> "fish_prompt.fish"
    fishRightPath = functionsDir </> "fish_right_prompt.fish"
    sourceInit initPath = mconcat
        [ "source "
        , B.stringUtf8 $ pluginsDir </> initPath
        , "\n"
        ]
    copyFileReport :: FilePath -> FilePath -> IO ()
    copyFileReport x y = x `copyFile` y >>
        lPutStr ["Copied ", B.stringUtf8 x, " to ", B.stringUtf8 y, "\n"]

dealPlug :: Bool -> Text -> IO (Maybe Plugin)
dealPlug pluginsExist url = do
    plugDirExists <- if not pluginsExist
        then return False
        else doesDirectoryExist dir
    if plugDirExists then do
        lPutStr ["Checking update for ", bDir, "...\n"]
        fetchResult <- silentCall "git" ["-C", dir, "fetch", "origin"]
        if fetchResult == ExitSuccess then do
            local <- readProcessB "git" ["-C", dir, "rev-parse", "@{0}"]
            remote <- readProcessB "git" ["-C", dir, "rev-parse", "@{u}"]
            if local == remote
            then lPutStr [bDir, " is already up to date.\n"] >> successWork
            else do
                resetResult <- silentCall "git"
                    ["-C", dir, "reset", "--hard", "origin/master"]
                if resetResult == ExitSuccess
                then lPutStr [bDir, " is updated.\n"] >> successWork
                else lPutStr ["Failed updating ", bDir] >> return Nothing
        else do
            lPutStr ["Failed checking update for ", bDir, ".\n"]
            return Nothing
    else do
        lPutStr ["Installing ", bDir, "...\n"]
        cloneResult <- silentCall "git"
            ["clone", "--depth=1", T.unpack url, dir]
        if cloneResult == ExitSuccess then
            lPutStr ["Installed ", bDir, ".\n"] >> successWork
        else
            lPutStr ["Failed installing ", bDir, ".\n"] >> return Nothing
  where
    dir :: FilePath
    dir = B.unpack $
        maybe (B64.encode $ T.encodeUtf8 url) T.encodeUtf8 (gitDir url)
    bDir :: Builder
    bDir = B.stringUtf8 dir
    initFishPath = dir </> "init.fish"
    promptPath = dir </> "fish_prompt.fish"
    rightPath = dir </> "fish_right_prompt.fish"
    successWork = do
        initExists <- doesFileExist initFishPath
        promptExists <- doesFileExist promptPath
        rightExists <- doesFileExist rightPath
        return $ Just Plugin
            { plugInit = if initExists then Just initFishPath else Nothing
            , plugPrompt = if promptExists then Just promptPath else Nothing
            , plugRight = if rightExists then Just rightPath else Nothing
            }

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

-- constant

greetMsg :: Builder
greetMsg = "chips: fish plugin manager\nversion " <> ver <> "\n"
  where
    ver :: Builder
    ver = mconcat $ intersperse "." $ map B.intDec $ versionBranch version

sourceLine :: ByteString
sourceLine = "\n\
\if [ -e ~/.config/chips/build.fish ] ;\
\ source ~/.config/chips/build.fish ; end\n"
