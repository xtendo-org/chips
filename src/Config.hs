module Config
    ( decode
    , encode
    , Config(..)
    , gitURLs
    ) where

import Data.Maybe
import Data.Text (Text)
import Data.Monoid

import System.Exit

import Data.Aeson.TH
import qualified Data.Yaml as Y

data Config = Config
    { git :: Maybe [Text]
    , github :: Maybe [Text]
    }
    deriving Show -- DEBUG

deriveJSON defaultOptions ''Config

decode :: FilePath -> IO Config
decode path = Y.decodeFileEither path >>=
    either (die . Y.prettyPrintParseException) return

encode :: FilePath -> Config -> IO ()
encode = Y.encodeFile

gitURLs :: Config -> [Text]
gitURLs (Config mgits mgithubs) = gits ++ githubURLs
  where
    gits = fromMaybe [] mgits
    githubs = fromMaybe [] mgithubs
    githubURLs = ["https://github.com/" <> url | url <- githubs]
