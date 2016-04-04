module SelfUpdate
    ( selfUpdate
    , UpdateFail(..)
    , updateFailMsg
    ) where

import Control.Exception

import Data.Monoid
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import System.IO
import System.Environment (getExecutablePath)
import System.Posix.ByteString

import Utility

data UpdateFail
    = ExecutionFail ByteString
    | RedirectParseFail
    | AlreadyUpToDate

updateFailMsg :: UpdateFail -> ByteString
updateFailMsg e = case e of
    ExecutionFail errmsg -> errmsg
    RedirectParseFail -> msgRedirectParseFail
    AlreadyUpToDate -> "already up to date.\n"

msgRedirectParseFail :: ByteString
msgRedirectParseFail = "\
\failed parsing the GitHub releases page. \
\Perhaps you need to manually upgrade.\n"

-- Update the binary executable file itself.
selfUpdate
    :: ByteString -> ByteString -> RawFilePath
    -> IO (Either UpdateFail ByteString)
selfUpdate repo assetName execPath = inspectRedirect >>= \case
    Right latestTag
        | latestTag == "" -> return $ Left RedirectParseFail
        | latestTag == chipsVer -> return $ Left AlreadyUpToDate
        | otherwise -> runUpdate latestTag execPath >>= \ e -> return
            (e >>= const (Right latestTag))
    Left err -> return $ Left err
  where
    inspectRedirect :: IO (Either UpdateFail ByteString)
    inspectRedirect = getLatest >>= \case
        Right redirectResponse -> return $ Right $
            B.takeWhile (\ c -> c /= '\r' && c /= '\n') $
            B.drop (B.length locationHeader) $
            snd $ B.breakSubstring locationHeader redirectResponse
        Left err -> return $ Left $ ExecutionFail err
      where
        locationHeader =
            "\nLocation: https://github.com/" <> repo <> "/releases/tag/"
        getLatest = readProcessB "curl"
            [ "-D", "-", "-o", "/dev/null"
            , B.unpack $ "https://github.com/" <> repo <> "/releases/latest"
            ]
    runUpdate :: ByteString -> ByteString -> IO (Either UpdateFail ByteString)
    runUpdate tag execPath = readProcessB "curl" ["-L", assetURL] >>= \case
        Right bin -> bracket mktmpexe (hClose . snd) $ \ (tmpPath, h) -> do
            B.hPut h bin
            hClose h
            rename tmpPath execPath
            return $ Right execPath
        Left x -> return $ Left $ ExecutionFail x
      where
        mktmpexe = mkstemp "/tmp/chips-"
        assetURL :: String
        assetURL = B.unpack $ mconcat
            [ "https://github.com/" <> repo <> "/releases/download/"
            , tag
            , "/"
            , assetName
            ]
