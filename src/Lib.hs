module Lib
    ( app
    ) where

import Data.Maybe
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.RawFilePath as B
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B hiding (writeFile)
import qualified Data.ByteString.Base64.URL as B64

import System.Exit
import System.IO
import System.Environment (getExecutablePath)
import System.IO.Error

import System.Posix.Directory.ByteString (changeWorkingDirectory)

import RawFilePath.Directory

import Chips
import File
import Git
import Platform
import RawFilePath
import SelfUpdate
import Spawn
import Utility
import qualified Config as C


data Plugin = Plugin
    { plugInit :: Maybe RawFilePath
    , plugPrompt :: Maybe RawFilePath
    , plugRight :: Maybe RawFilePath
    , plugFnc :: Maybe RawFilePath
    , plugCpl :: Maybe RawFilePath
    }

-- Session configuration container. In other words,
-- things that are set up in the beginning of execution and hardly change
data Session = Session
    { fishPath :: RawFilePath
    , chipsPath :: RawFilePath
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
        then (<> "/.local/bin/chips") <$> getHomeForce
        else return p
    let yamlPath = cPath <> "/plugin.yaml"
    doesFileExist yamlPath >>= \ yamlExists -> if yamlExists then do
        conf <- C.decode (T.unpack $ T.decodeUtf8 yamlPath)
        runSync Session
            { chipsPath = cPath
            , chipsConf = conf
            , fishPath = fPath
            , binPath = execPath
            }
    else do
        createDirectoryIfMissing True cPath
        T.writeFile (T.unpack $ T.decodeUtf8 yamlPath) C.templateConfig
        lPutStr
            [ "chips has just created the default configuration at:\n    "
            , B.byteString yamlPath
            , "\nPlease edit the file and run chips again.\n"
            ]
  where
    getAppDirectory appName = fmap (\ x -> x <> "/.config/" <> appName) getHomeForce
    getHomeForce = fmap (fromMaybe (error "can't find $HOME?!")) getHomeDirectory

runSync :: Session -> IO ()
runSync Session{..} = do
    pluginsExist <- doesDirectoryExist pluginsDir
    unless pluginsExist $ createDirectoryIfMissing True pluginsDir
    changeWorkingDirectory pluginsDir

    -- Begin updating chips itself
    waitUpdate <- spawn $ do
        bPutStr "Checking update for chips...\n"
        checkUpdate "xtendo-org/chips" >>= \case
            AlreadyUpToDate -> return $ Left "chips is already up to date.\n"
            ExecutionFail err -> return $ Left $
                "chips self-update fail: " <> err
            UpdateNeeded tag -> return $ Right tag

    -- Concurrently deal with each entry of plugin.yaml
    plugResults <- fmap catMaybes $
        iParMapIO 50000 (dealPlug pluginsExist) $ C.gitURLs chipsConf

    -- Register or unregister themes
    createDirectoryIfMissing True functionsDir
    maybe (tryRemoveFile fishPromptPath) (`copyFileReport` fishPromptPath) $
        listToMaybe $ mapMaybe plugPrompt plugResults
    maybe (tryRemoveFile fishRightPath) (`copyFileReport` fishRightPath) $
        listToMaybe $ mapMaybe plugRight plugResults

    -- Create build.fish for plugins with init.fish
    B.withFile buildPath WriteMode $ \h -> B.hPutBuilder h $ mconcat $
        [ "function chips; \""
        , B.byteString binPath
        , "\" $argv; exec fish; end\n"
        ]
        <> map sourceInit (mapMaybe plugInit plugResults)
    lPutStr ["Build result saved at ", B.byteString buildPath, "\n"]

    -- Functions
    forM_ (mapMaybe plugFnc plugResults) $ \srcDir -> do
        files <- getDirectoryFilesRecursive srcDir
        forM_ files $ \path ->
            path `copyFileReport` (functionsDir <> "/" <> basename path)

    -- Add build.fish to config.fish
    tryIOError (B.readFile configFishPath) >>= \case
        Left e -> if isDoesNotExistError e
            then B.writeFile configFishPath chipsLineInConfig
            else ioError e
        Right configFish -> unless (sourceLine `B.isInfixOf` configFish) $ do
            B.appendFile configFishPath $ "\n" <> chipsLineInConfig
            lPutStr ["Added to ", B.byteString configFishPath, "\n"]

    -- Finish updating chips itself
    waitUpdate >>= \case
        Right tag -> do
            bPutStr "Updating chips itself...\n"
            updateResult <- runUpdate
                "xtendo-org/chips" tag ("chips_" <> platform) binPath
            case updateResult of
                Right _ -> lPutStr
                    ["chips is updated to ", B.byteString tag, "\n"]
                Left e -> B.putStr e
        Left e -> B.putStr e

  where
    configFishPath = fishPath <> "/config.fish"
    pluginsDir = chipsPath <> "/dist"
    buildPath :: RawFilePath
    buildPath = chipsPath <> "/build.fish"
    functionsDir = fishPath <> "/functions"
    fishPromptPath = functionsDir <> "/fish_prompt.fish"
    fishRightPath = functionsDir <> "/fish_right_prompt.fish"
    sourceInit initPath = mconcat
        [ ". "
        , B.byteString $ pluginsDir <> "/" <> initPath
        , "\n"
        ]
    copyFileReport :: RawFilePath -> RawFilePath -> IO ()
    copyFileReport x y = x `copyFile` y >>
        lPutStr ["Copied ", B.byteString x, " to ", B.byteString y, "\n"]


-- Take a Git URL, figure out what to do (install or upgrade), and do it.
dealPlug :: Bool -> Text -> IO (Maybe Plugin)
dealPlug pluginsExist url = do
    plugDirExists <- if not pluginsExist
        then return False
        else doesDirectoryExist dir
    if plugDirExists then do
        lPutStr ["Checking update for ", bDir, "...\n"]
        fetchResult <- callProcess $ proc "git" ["-C", dir, "fetch", "origin"]
        if fetchResult == ExitSuccess then do
            local <- readProcess "git" ["-C", dir, "rev-parse", "@{0}"]
            remote <- readProcess "git" ["-C", dir, "rev-parse", "@{u}"]
            if local == remote
            then lPutStr [bDir, " is already up to date.\n"] >> successWork
            else do
                resetResult <- callProcess $ proc "git"
                    ["-C", dir, "reset", "--hard", "origin/master"]
                if resetResult == ExitSuccess
                then lPutStr [bDir, " is updated.\n"] >> successWork
                else lPutStr ["Failed updating ", bDir, "\n"] >> successWork
        else do
            lPutStr ["Failed checking update for ", bDir, ".\n"]
            -- Yes, checking update has failed, but the plugin still needs to
            -- be sourced! We run successWork anyway.
            successWork
    else do
        lPutStr ["Installing ", bDir, "...\n"]
        cloneResult <- callProcess $ proc "git"
            ["clone", "--depth=1", T.encodeUtf8 url, dir]
        if cloneResult == ExitSuccess then
            lPutStr ["Installed ", bDir, ".\n"] >> successWork
        else
            lPutStr ["Failed installing ", bDir, ".\n"] >> return Nothing

  where
    dir :: RawFilePath
    dir = maybe (B64.encode $ T.encodeUtf8 url) T.encodeUtf8 (gitDir url)
    bDir :: Builder
    bDir = B.byteString dir
    initFishPath = dir <> "/init.fish"
    promptPath = dir <> "/fish_prompt.fish"
    rightPath = dir <> "/fish_right_prompt.fish"
    fncPath = dir <> "/functions"
    cplPath = dir <> "/completions"
    successWork = do
        initExists <- doesFileExist initFishPath
        promptExists <- doesFileExist promptPath
        rightExists <- doesFileExist rightPath
        fncExists <- doesDirectoryExist fncPath
        cplExists <- doesDirectoryExist cplPath
        return $ Just Plugin
            { plugInit = if initExists then Just initFishPath else Nothing
            , plugPrompt = if promptExists then Just promptPath else Nothing
            , plugRight = if rightExists then Just rightPath else Nothing
            , plugFnc = if fncExists then Just fncPath else Nothing
            , plugCpl = if cplExists then Just cplPath else Nothing
            }
    readProcess cmd args = do
        (_, printed, _) <- readProcessWithExitCode $ proc cmd args
        return printed

-- constant

greetMsg :: Builder
greetMsg = mconcat
    ["chips: fish plugin manager\nversion ", B.byteString chipsVer, "\n"]

chipsLineInConfig :: ByteString
chipsLineInConfig = "# chips" <> sourceLine

sourceLine :: ByteString
sourceLine = "\n\
\if [ -e ~/.config/chips/build.fish ] ;\
\ . ~/.config/chips/build.fish ; end\n"
