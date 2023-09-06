module Git
    ( gitDir
    ) where

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

gitDir :: Text -> Maybe Text
gitDir repoURL = if T.length s7 < 2 || T.head s7 == '/'
    then Nothing
    else Just s7
  where
    -- s1: repoURL with its scheme dropped.
    s1Broken = snd $ T.breakOn "://" repoURL
    s1 = if T.null s1Broken then repoURL else T.drop (T.length "://") s1Broken
    s2Taken = T.dropWhileEnd (/= '@') $ T.takeWhile (not . isPathSeparator) s1
    -- s2: s1 with the auth part dropped. Dropping the auth part is greedy:
    -- will drop until the last '@' before a path separator.
    s2 = T.drop (T.length s2Taken) s1
    -- s3: s2 with spaces and path separators at the end dropped.
    s3 = T.dropWhileEnd (\c -> c == ' ' || isPathSeparator c) s2
    -- s4: s3 with the ".git" at the end dropped, and more path separators
    -- dropped (if they are there).
    s4 = if ".git" `T.isSuffixOf` s3
        then T.dropWhileEnd isPathSeparator $ T.dropEnd (T.length ".git") s3
        else s3
    -- s5: if '/' is not in s4 AND ':' is in s4 AND the last part separated by
    -- ':' is all digits, it must be considered a port number for Git's
    -- backwards compatibility. Drop the colon and the port number.
    s5Digitless = T.dropWhileEnd isDigit s4
    s5 = if and
        [ not ('/' `isIn` s4)
        , ':' `isIn` s4
        , not (T.null s5Digitless)
        , T.last s5Digitless == ':'
        ]
        then T.init s5Digitless
        else s4
    -- s6: Find the last component of s5. ':' is also considered a separator.
    s6 = T.takeWhileEnd (\c -> not (isPathSeparator c) && c /= ':') s5
    -- s7: Drop ".bundle" or ".git"
    s7
        | ".git" `T.isSuffixOf` s6 = T.dropEnd (T.length ".git") s6
        | ".bundle" `T.isSuffixOf` s6 = T.dropEnd (T.length ".bundle") s6
        | otherwise = s6

-- utility

isIn :: Char -> Text -> Bool
isIn c = T.any (c ==)

-- TODO(XT): We can probably do better than this
isPathSeparator :: Char -> Bool
isPathSeparator = ('/' ==)
