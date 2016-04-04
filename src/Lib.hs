module Lib
    ( app
    ) where

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
import System.IO
import System.FilePath
import System.Environment (getExecutablePath)
import System.Posix.ByteString (RawFilePath)

import Lib.Directory
import Spawn
import Utility
import qualified Config as C
import Git
import SelfUpdate
import Platform

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
    , binPath :: RawFilePath
    }

-- main execution functions

app :: IO ()
app = do
    bPutStr greetMsg
    fPath <- getAppDirectory "fish"
    cPath <- getAppDirectory "chips"
    execPath <- (byteString <$> getExecutablePath) >>= \ p ->
        if "/ghc" `B.isSuffixOf` p
        then (<> "/.local/bin/chips") . byteString <$> getHomeDirectory
        else return p
    let yamlPath = cPath </> "plugin.yaml"
    doesFileExist yamlPath >>= \ yamlExists -> if yamlExists then do
        conf <- C.decode yamlPath
        runSync Session
            { chipsPath = cPath
            , chipsConf = conf
            , fishPath = fPath
            , binPath = execPath
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

    -- Begin updating chips itself
    waitUpdate <- spawn $ do
        bPutStr "Checking update for chips...\n"
        selfUpdate "kinoru/chips" ("chips_" <> platform) binPath

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
    withFile buildPath WriteMode $ \h -> B.hPutBuilder h $ mconcat $
        [ "alias chips \""
        , B.byteString binPath
        , "; exec fish\"\n"
        ] <> map sourceInit (mapMaybe plugInit plugResults)
    lPutStr ["Build result saved at ", B.stringUtf8 buildPath, "\n"]

    -- Add build.fish to config.fish
    configFish <- B.readFile configFishPath
    when (snd (B.breakSubstring sourceLine configFish) == "") $ do
        B.appendFile configFishPath $ "\n# chips" <> sourceLine
        lPutStr ["Added to ", B.stringUtf8 configFishPath, "\n"]

    -- Finish updating chips itself
    waitUpdate >>= \case
        Left e -> case e of
            AlreadyUpToDate -> B.putStr "chips is already up to date.\n"
            x -> lPutStr
                ["chips self-update fail: ", B.byteString $ updateFailMsg x]
        Right tag -> lPutStr ["Updated chips to ", B.byteString tag, "\n"]

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

-- Take a Git URL, figure out what to do (install or upgrade), and do it.
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

-- constant

greetMsg :: Builder
greetMsg = mconcat
    ["chips: fish plugin manager\nversion ", B.byteString chipsVer, "\n"]

sourceLine :: ByteString
sourceLine = "\n\
\if [ -e ~/.config/chips/build.fish ] ;\
\ source ~/.config/chips/build.fish ; end\n"
