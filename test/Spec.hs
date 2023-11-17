{-# LANGUAGE TypeApplications #-}
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import HaskGit
import GitObject
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Shelly -- not used, but it can call shell command;


---------------------------Test for git Show------------------------------------
-- 0. cp .git to .haskgit (bcs, `git` only use .git)
-- 1. get a list of git object paths
-- 2. call git
-- 3. compare git show <object> with haskgit show <object>
-- see `test.sh`
-- NOTE: this approach is easier to do with bash.
-- for later unit test, there will be a .dat file stores all result
-- of `git show` then do comparison

main :: IO ()
main = someFunc


{-
----------------------Abandoned tests (for review only)-------------------------
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = adjustOption @QuickCheckTests (* 10) $
  testGroup "HaskGit"
      [ testGroup "GitObject"
          [
            -- return $ localOption @QuickCheckTests 1000 $ propNewBlob
            -- testProperty "propNewBlob@Int@String" $ propNewBlob,
            -- testProperty "propNewTree@Int@[(String, String, ByteString)]" $ propNewTree,
            -- testProperty "propNewCommit@Int@ByteString@[ByteString]@GitAuthor@GitCommitter@String" $ propNewCommit
            -- newGitObjectHash,
            -- gitObjectSerialize,
            -- gitShowStr,
            -- getBlobContent,
          ]
        -- serializeGitObject
        -- core data structures
        -- deserializeGitObject
        -- gitShow
      ]

-- instance Arbitrary ByteString where
--   arbitrary = BS.pack <$> arbitrary

-- propNewBlob :: Int -> String -> Property
-- propNewBlob size content = size >= 0 ==> getBlobContent (newBlob size content) == content

-- propNewTree :: Int -> [(String, String, ByteString)] -> Property
-- propNewTree size entries = size >= 0 ==> f (newTree size entries)
--   where
--     f (Tree (s, e)) = s == size && length e == length entries

-- propNewCommit :: Int -> ByteString -> [ByteString] -> GitAuthor -> GitCommitter -> String -> Property
-- propNewCommit bytesize tree parents authorInfo committerInfo message =
--   bytesize >= 0 ==>
--   gitHashObject (newCommit bytesize tree parents authorInfo committerInfo message) True
--     == gitHashObject obj True
--     where
--       obj = Commit (bytesize, tree, parents, authorInfo, committerInfo, message)

-- NOTE:
-- An example of assertion test; The challenge is we need a `.dat` file that contains numerous test
-- gitHashObject :: GitObject -> Bool -> ByteString
-- gitHashObject obj _ = SHA1.hash (gitObjectSerialize obj)
-- gitHashObjectTest :: IO TestTree
-- gitHashObjectTest = testGroup "gitHashObject"
--   [ testCase "gitHashObject" $
--     do
--       let
--         obj = Blob (_, "This is an example of Hash")
--          -- maybe not right
--         expectedHash = "ce013625030ba8dba906f756967f9e9ca394464a"
--         actualHash = gitHashObject obj False
--       assertEqual "" expectedHash actualHash
--   ]
-}