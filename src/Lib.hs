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
    when (isJust $ find (\arg -> arg == "-h" || arg == "--help") args) $
        B.putStrLn helpMsg *> exitSuccess
    B.putStrLn greetMsg
    fpath <- getAppUserDataDirectory "chips"
    -- TODO: if config.yaml does not exist, create one with the template,
    -- and add "source" in config.fish
    conf <- C.decode (fpath </> "config.yaml")
    runSync Session
        { chipsPath = fpath
        , chipsConf = conf
        }

runSync :: Session -> IO ()
runSync Session{..} = do
    pluginsExist <- doesDirectoryExist pluginsDir
    unless pluginsExist $ createDirectoryIfMissing True pluginsDir
    setCurrentDirectory pluginsDir
    -- Loop over each entry of config.yaml
    initPaths <- forM (C.gitURLs chipsConf) $ \ url -> do
        let dir = dirB url
        pluginDirExist <- if not pluginsExist
            then return False
            else doesDirectoryExist $ B.unpack dir
        if pluginDirExist then
            bPutStr $ B.byteString dir <> " is already installed.\n"
            -- TODO: do the update here
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
    withFile (chipsPath </> "build.fish") WriteMode $ \handle ->
        B.hPutBuilder handle $ mconcat $ sourcePaths initPaths
  where
    pluginsDir = chipsPath </> "plugins"
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

helpMsg :: ByteString
helpMsg = "usage: chips [-h|--help]\n\
    \\nRunning chips will read ~/.chips/config.yaml and install plugins.\
    \\nFor more information, please visit:\n\
    \\n    https://github.com/kinoru/chips\n"

greetMsg :: ByteString
greetMsg = "\
  \chips: fish plugin manager\
\\n=========================="
