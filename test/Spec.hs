-- {-# LANGUAGE TypeApplications #-}

module Main (main) where

import Codec.Compression.Zlib (compress, decompress)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GitHash
import GitObject
import GitParser (parseGitObject)
import HaskGit
import Shelly
import System.Directory (removeFile)
import System.IO.Silently (capture)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec (parse)

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
tests = testGroup "Unit Tests" <$> sequence [showTests, updateRefTest, hashObjectTest, saveObjectTest]

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

-- Add test for updateRef
updateRefTest :: IO TestTree
updateRefTest = do
  let hash1 = "f6f754dbe0808826bed2237eb651558f75215cc6"
  let hash2 = "f6e1af0b636897ed62c8c6dad0828f1172b9b82a"
  originalMainRef <- TIO.readFile ".haskgit/refs/heads/main"
  originalHeadRef <- TIO.readFile ".haskgit/HEAD"

  -- Case1: refname, hash-value
  gitUpdateRef "refs/heads/test" hash1
  let expectedCase1 = T.pack (hash1 ++ "\n")
  actualCase1 <- TIO.readFile ".haskgit/refs/heads/test"

  -- Case2: refname, refname
  -- .haskgit/refs/heads/test will contain f6f754dbe0808826bed2237eb651558f75215cc6
  gitUpdateRef "refs/heads/main" "refs/heads/test"
  let expectedCase2 = T.pack (hash1 ++ "\n")
  actualCase2 <- TIO.readFile ".haskgit/refs/heads/main"

  -- Case3: symbolic-ref, hash-value
  -- .haskgit/HEAD will contain f6e1af0b636897ed62c8c6dad0828f1172b9b82a
  gitUpdateRef "HEAD" hash2
  let expectedCase3 = T.pack (hash2 ++ "\n")
  actualCase3 <- TIO.readFile ".haskgit/HEAD"

  let updateRefTest =
        testGroup
          "gitUpdateRef"
          [ testCase "refname hash-value" $
              actualCase1 @?= expectedCase1,
            testCase "refname refname" $
              actualCase1 @?= expectedCase1,
            testCase "symbolic-ref hash-value" $
              actualCase1 @?= expectedCase1
          ]

  -- Go back to original ref
  TIO.writeFile ".haskgit/refs/heads/main" originalMainRef
  TIO.writeFile ".haskgit/HEAD" originalHeadRef

  return updateRefTest

-- Add test for hashObject

-- Test for hashObject
-- read content from real ".git" and see if hashvalue is the same as expected.
hashObjectTest :: IO TestTree
hashObjectTest = do
  let actualBlobHash = "f6f754dbe0808826bed2237eb651558f75215cc6"
  contentBlob <- BSLC.readFile ".git/objects/f6/f754dbe0808826bed2237eb651558f75215cc6"
  let expectedBlob = case parse parseGitObject "" (BSLC.unpack (decompress contentBlob)) of
        Left err -> Nothing
        Right result -> Just (encode (getHash (hashObject result)))

  let actualTreeHash = "f6e1af0b636897ed62c8c6dad0828f1172b9b82a"
  contentTree <- BSLC.readFile ".git/objects/f6/e1af0b636897ed62c8c6dad0828f1172b9b82a"
  let expectedTree = case parse parseGitObject "" (BSLC.unpack (decompress contentTree)) of
        Left err -> Nothing
        Right result -> Just (encode (getHash (hashObject result)))

  let actualCommitHash = "562c9c7b09226b6b54c28416d0ac02e0f0336bf6"
  contentCommit <- BSLC.readFile ".git/objects/56/2c9c7b09226b6b54c28416d0ac02e0f0336bf6"
  let expectedCommit = case parse parseGitObject "" (BSLC.unpack (decompress contentCommit)) of
        Left err -> Nothing
        Right result -> Just (encode (getHash (hashObject result)))

  let hashObjectTest =
        testGroup
          "hashObejct"
          [ testCase "blob object" $
              Just (B.pack actualBlobHash) @?= expectedBlob,
            testCase "tree object" $
              Just (B.pack actualTreeHash) @?= expectedTree,
            testCase "commit object" $
              Just (B.pack actualCommitHash) @?= expectedCommit
          ]

  return hashObjectTest

-- saveGitObject hash content
saveObjectTest :: IO TestTree
saveObjectTest = do
  -- Blob
  let blobHash = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  contentBlob <- BSLC.readFile ".git/objects/04/efa50ffad0bc03edea5cbca1936c29aee18553"
  case parse parseGitObject "" (BSLC.unpack (decompress contentBlob)) of
    Left err -> putStrLn "Parse error duing test"
    Right result -> saveGitObject (B.pack blobHash) (gitObjectSerialize result)

  -- check the strings (need to decompress since compression data might depend on machine)
  tmpBlob1 <- BSLC.readFile ".haskgit/objects/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  let expectedBlob = decompress tmpBlob1
  tmpBlob2 <- BSLC.readFile ".git/objects/04/efa50ffad0bc03edea5cbca1936c29aee18553"
  let actualBlob = decompress tmpBlob2

  -- Tree
  let treeHash = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
  contentTree <- BSLC.readFile ".git/objects/00/13ee97b010dc8e9646f3c5a9841b62eb754f77"
  case parse parseGitObject "" (BSLC.unpack (decompress contentTree)) of
    Left err -> putStrLn "Parse error duing test"
    Right result -> do saveGitObject (B.pack treeHash) (gitObjectSerialize result)

  tmpTree1 <- BSLC.readFile ".haskgit/objects/bb/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
  let expectedTree = decompress tmpTree1
  tmpTree2 <- BSLC.readFile ".git/objects/00/13ee97b010dc8e9646f3c5a9841b62eb754f77"
  let actualTree = decompress tmpTree2

  -- Commit
  let commitHash = "cccccccccccccccccccccccccccccccccccccccc"
  contentCommit <- BSLC.readFile ".git/objects/56/2c9c7b09226b6b54c28416d0ac02e0f0336bf6"
  case parse parseGitObject "" (BSLC.unpack (decompress contentCommit)) of
    Left err -> putStrLn "Parse error duing test"
    Right result -> do saveGitObject (B.pack commitHash) (gitObjectSerialize result)

  tmpCommit1 <- BSLC.readFile ".haskgit/objects/cc/cccccccccccccccccccccccccccccccccccccc"
  let expectedCommit = decompress tmpCommit1
  tmpCommit2 <- BSLC.readFile ".git/objects/56/2c9c7b09226b6b54c28416d0ac02e0f0336bf6"
  let actualCommit = decompress tmpCommit2

  let saveObjectTest =
        testGroup
          "saveObejct"
          [ testCase "blob object" $
              actualBlob @?= expectedBlob,
            testCase "tree object" $
              actualTree @?= expectedTree,
            testCase "commit object" $
              actualCommit @?= expectedCommit
          ]

  -- remove created directory
  removeFile ".haskgit/objects/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  removeFile ".haskgit/objects/bb/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
  removeFile ".haskgit/objects/cc/cccccccccccccccccccccccccccccccccccccc"

  return saveObjectTest

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