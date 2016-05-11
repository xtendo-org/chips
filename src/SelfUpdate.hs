module SelfUpdate
    ( checkUpdate
    , runUpdate
    , CheckResult(..)
    ) where

import Control.Exception

import Data.Maybe
import Data.Monoid
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as T
import Data.SemVer

import System.IO
import System.Posix.ByteString

import RawFilePath
import Chips

data CheckResult
    = ExecutionFail ByteString
    | AlreadyUpToDate
    | UpdateNeeded ByteString
    deriving Show -- debug

msgRedirectParseFail :: ByteString
msgRedirectParseFail = "\
\failed parsing the GitHub releases page. \
\Perhaps you need to manually upgrade.\n"

-- Update the binary executable file itself.
checkUpdate :: ByteString -> IO CheckResult
checkUpdate repo = getLatest >>= \case
    Right redirectRsp -> return $ versionCmp $ searchTag redirectRsp
    Left err -> return $ ExecutionFail err
  where
    searchTag = B.takeWhile (\ c -> c /= '\r' && c /= '\n') .
        B.drop (B.length locationHeader) .
        snd . B.breakSubstring locationHeader
    versionCmp r
        | r == "" = ExecutionFail msgRedirectParseFail
        | r == chipsVer = AlreadyUpToDate
        | otherwise = mtag r
    mtag tagB = fromMaybe (ExecutionFail msgRedirectParseFail) $ do
        tag <- eitherToMaybe (T.decodeUtf8' tagB)
        targetSemVer <- eitherToMaybe (fromText tag)
        return $ if chipsSemVer < targetSemVer
            then UpdateNeeded tagB else AlreadyUpToDate
    eitherToMaybe :: Either e a -> Maybe a
    eitherToMaybe = either (const Nothing) Just
    locationHeader =
        "\nLocation: https://github.com/" <> repo <> "/releases/tag/"
    getLatest = readProcess "curl"
        [ "-D", "-", "-o", "/dev/null"
        , "https://github.com/" <> repo <> "/releases/latest"
        ]

runUpdate
    :: ByteString -> ByteString -> ByteString -> RawFilePath
    -> IO (Either ByteString ByteString)
runUpdate repo tag assetName path = getAsset >>= \case
    Right bin -> bracket topen hClose $ \ h -> do
        B.hPut h bin
        hClose h
        rename tmpPath path
        return $ Right tag
    Left err -> return $ Left err
  where
    getAsset = readProcess "curl" ["-L", assetURL]
    topen = createFile tmpPath stdFileMode >>= fdToHandle
    tmpPath = path <> ".copyFile.tmp"
    assetURL :: ByteString
    assetURL = mconcat
        [ "https://github.com/" <> repo <> "/releases/download/"
        , tag
        , "/"
        , assetName
        ]
