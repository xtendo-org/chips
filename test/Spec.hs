import qualified Data.Text as T

import Test.Hspec

import Git

main :: IO ()
main = hspec $ do
    describe "Git.gitDir" $ do
        testGitDir "/foo/bar:2222.git" "2222"
        testGitDir "git@github.com:kinoru/chips.git" "chips"
        testGitDir "https://github.com/kinoru/chips.git" "chips"
        testGitDir "git://multiple@auth@kinoru/chips.git/ " "chips"
  where
    testGitDir repo dir = it
        (T.unpack $ mconcat ["returns ", dir, " from ", repo]) $
        gitDir repo `shouldBe` Just dir
