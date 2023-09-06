module Config
    ( decode
    , encode
    , Config(..)
    , templateConfig
    , gitURLs
    ) where

import Data.Maybe
import Data.Text (Text)

import System.Exit

import Data.Aeson.TH
import qualified Data.Yaml as Y

templateConfig :: Text
templateConfig = "\
\git:\n\
\# Add Git clone URLs for fish plugins here.\n\
\# For example, uncomment below to enable the fish-sensible plugin.\n\
\# - https://github.com/simnalamburt/fish-sensible\n\
\github:\n\
\# Add GitHub repositories for fish plugins here.\n\
\# For example, uncomment below to enable the fish-sensible plugin.\n\
\# - simnalamburt/fish-sensible\n"

data Config = Config
    { git :: Maybe [Text]
    , github :: Maybe [Text]
    }
    deriving Show -- DEBUG

deriveJSON defaultOptions ''Config

decode :: FilePath -> IO Config
decode path = Y.decodeFileEither path >>=
    either (\ err -> die (errMsg ++ Y.prettyPrintParseException err)) return
  where
    errMsg = "Failed parsing " ++ path ++ ":\n"

encode :: FilePath -> Config -> IO ()
encode = Y.encodeFile

gitURLs :: Config -> [Text]
gitURLs (Config mgits mgithubs) = gits ++ githubURLs
  where
    gits = fromMaybe [] mgits
    githubs = fromMaybe [] mgithubs
    githubURLs = ["https://github.com/" <> url | url <- githubs]
