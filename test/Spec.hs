-- {-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import GitObject
import HaskGit
import Shelly -- not used, but it can call shell command;
import System.IO.Silently (capture)
import Test.Tasty
import Test.Tasty.HUnit

-- import Test.Tasty.QuickCheck

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
main = tests >>= defaultMain

tests :: IO TestTree
tests = testGroup "Unit Tests" <$> sequence [showTests]

showTests :: IO TestTree
showTests = do
  let blobHash = "04efa50ffad0bc03edea5cbca1936c29aee18553"
  expectedBlobShow <- readFile "test/TestData/expectedBlobShow.dat"
  (actualBlobShow, ()) <- capture $ gitShow (B.pack blobHash)

  let treeHash = "0013ee97b010dc8e9646f3c5a9841b62eb754f77"
  expectedTreeShow <- readFile "test/TestData/expectedTreeShow.dat"
  (actualTreeShow, ()) <- capture $ gitShow (B.pack treeHash)

  let commitHash1 = "562c9c7b09226b6b54c28416d0ac02e0f0336bf6"
  expectedCommitShow1 <- readFile "test/TestData/expectedCommitShow1.dat"
  (actualCommitShow1, ()) <- capture $ gitShow (B.pack commitHash1)
  
  let commitHash2 = "37e229feb120a4242f784881472f5a1e32a80ca0"
  (actualCommitShow2, ()) <- capture $ gitShow (B.pack commitHash2)
  expectedCommitShow2 <- readFile "test/TestData/expectedCommitShow2.dat"

  let showTest =
        testGroup
          "gitShow"
          [ testCase "blob object" $
              actualBlobShow @?= expectedBlobShow,
            testCase "tree object" $
              actualTreeShow @?= expectedTreeShow,
            testCase "commit object 1" $
              actualCommitShow1 @?= expectedCommitShow1,
            testCase "commit object 2" $
              actualCommitShow2 @?= expectedCommitShow2
          ]
  return showTest

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