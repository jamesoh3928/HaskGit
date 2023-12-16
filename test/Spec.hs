-- {-# LANGUAGE TypeApplications #-}

module Main (main) where

import Codec.Compression.Zlib (compress, decompress)
import Data.ByteString (ByteString, isPrefixOf)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encode)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GitHash
import GitObject
import GitParser (parseGitObject, parseIndexFile)
import HaskGit
import Index (addOrUpdateEntries, gitIndexSerialize, hasFile, saveIndexFile)
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

testGitDir :: FilePath
testGitDir = ".test_haskgit"

main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests =
  testGroup "Unit Tests"
    <$> sequence
      [ showTests,
        updateRefTests,
        hashObjectTests,
        saveObjectTests,
        parseSaveIndexTests,
        listBranchTests,
        addOrUpdateEntriesTests
      ]

-- Make sure if haskgit show produces expected output
showTests :: IO TestTree
showTests = do
  let blobHash = "04efa50ffad0bc03edea5cbca1936c29aee18553"
  expectedBlobShow <- readFile "test/TestData/expectedBlobShow.dat"
  (actualBlobShow, ()) <- capture $ gitShow (BSC.pack blobHash) testGitDir

  let treeHash = "0013ee97b010dc8e9646f3c5a9841b62eb754f77"
  expectedTreeShow <- readFile "test/TestData/expectedTreeShow.dat"
  (actualTreeShow, ()) <- capture $ gitShow (BSC.pack treeHash) testGitDir

  let commitHash1 = "562c9c7b09226b6b54c28416d0ac02e0f0336bf6"
  expectedCommitShow1 <- readFile "test/TestData/expectedCommitShow1.dat"
  (actualCommitShow1, ()) <- capture $ gitShow (BSC.pack commitHash1) testGitDir

  let commitHash2 = "37e229feb120a4242f784881472f5a1e32a80ca0"
  (actualCommitShow2, ()) <- capture $ gitShow (BSC.pack commitHash2) testGitDir
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

updateRefTests :: IO TestTree
updateRefTests = do
  let hash1 = "f6f754dbe0808826bed2237eb651558f75215cc6"
  let hash2 = "f6e1af0b636897ed62c8c6dad0828f1172b9b82a"
  let refMainPath = ".haskgit/refs/heads/main"
  originalMainRef <- TIO.readFile refMainPath
  originalHeadRef <- TIO.readFile ".haskgit/HEAD"

  -- Case1: refname, hash-value
  let refTestPath = ".test_haskgit/refs/heads/test"
  gitUpdateRef "refs/heads/test" hash1 testGitDir
  let expectedCase1 = T.pack (hash1 ++ "\n")
  -- Use strick IO to prevent access to same file due Haskell lazy eval
  actualCase1 <- TIO.readFile refTestPath

  -- Case2: refname, refname
  -- .haskgit/refs/heads/test will contain f6f754dbe0808826bed2237eb651558f75215cc6
  gitUpdateRef "refs/heads/main" "refs/heads/test" testGitDir
  let expectedCase2 = T.pack (hash1 ++ "\n")
  actualCase2 <- TIO.readFile refMainPath

  -- Case3: symbolic-ref, hash-value
  -- .haskgit/HEAD will contain f6e1af0b636897ed62c8c6dad0828f1172b9b82a
  gitUpdateRef "HEAD" hash2 testGitDir
  let expectedCase3 = T.pack (hash2 ++ "\n")
  actualCase3 <- TIO.readFile (testGitDir ++ "/HEAD")

  let updateRefTests =
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
  TIO.writeFile refMainPath originalMainRef
  TIO.writeFile ".haskgit/HEAD" originalHeadRef

  return updateRefTests

-- Test for hashObject
-- read content from real ".git" and see if hashvalue is the same as expected.
hashObjectTests :: IO TestTree
hashObjectTests = do
  let actualBlobHash = "f6f754dbe0808826bed2237eb651558f75215cc6"
  contentBlob <- BSLC.readFile (testGitDir ++ "/objects/f6/f754dbe0808826bed2237eb651558f75215cc6")
  let expectedBlob = case parse parseGitObject "" (BSLC.unpack (decompress contentBlob)) of
        Left err -> Nothing
        Right result -> Just (getHash (hashObject result))

  let actualTreeHash = "f6e1af0b636897ed62c8c6dad0828f1172b9b82a"
  contentTree <- BSLC.readFile (testGitDir ++ "/objects/f6/e1af0b636897ed62c8c6dad0828f1172b9b82a")
  let expectedTree = case parse parseGitObject "" (BSLC.unpack (decompress contentTree)) of
        Left err -> Nothing
        Right result -> Just (getHash (hashObject result))

  let actualCommitHash = "562c9c7b09226b6b54c28416d0ac02e0f0336bf6"
  contentCommit <- BSLC.readFile (testGitDir ++ "/objects/56/2c9c7b09226b6b54c28416d0ac02e0f0336bf6")
  let expectedCommit = case parse parseGitObject "" (BSLC.unpack (decompress contentCommit)) of
        Left err -> Nothing
        Right result -> Just (getHash (hashObject result))

  let hashObjectTests =
        testGroup
          "hashObejct"
          [ testCase "blob object" $
              Just (BSC.pack actualBlobHash) @?= encode <$> expectedBlob,
            testCase "tree object" $
              Just (BSC.pack actualTreeHash) @?= encode <$> expectedTree,
            testCase "commit object" $
              Just (BSC.pack actualCommitHash) @?= encode <$> expectedCommit
          ]

  return hashObjectTests

-- saveGitObject hash content
saveObjectTests :: IO TestTree
saveObjectTests = do
  -- Blob (bytestring of "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
  let blobHash = BSC.pack (replicate 20 '\170')
  let blobTempPath = testGitDir ++ "/objects/aa/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  contentBlob <- BSLC.readFile (testGitDir ++ "/objects/04/efa50ffad0bc03edea5cbca1936c29aee18553")
  case parse parseGitObject "" (BSLC.unpack (decompress contentBlob)) of
    Left err -> assertFailure (show err)
    Right result -> saveGitObject (bsToHash $ encode blobHash) (gitObjectSerialize result) testGitDir

  -- check the strings (need to decompress since compression data might depend on machine)
  tmpBlob1 <- BSLC.readFile blobTempPath
  let expectedBlob = decompress tmpBlob1
  tmpBlob2 <- BSLC.readFile (testGitDir ++ "/objects/04/efa50ffad0bc03edea5cbca1936c29aee18553")
  let actualBlob = decompress tmpBlob2

  -- Tree (byteString of "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
  let treeHash = BSC.pack (replicate 20 '\187')
  let treeTempPath = testGitDir ++ "/objects/bb/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
  contentTree <- BSLC.readFile (testGitDir ++ "/objects/00/13ee97b010dc8e9646f3c5a9841b62eb754f77")
  case parse parseGitObject "" (BSLC.unpack (decompress contentTree)) of
    Left err -> assertFailure (show err)
    Right result -> do saveGitObject (bsToHash $ encode treeHash) (gitObjectSerialize result) testGitDir

  tmpTree1 <- BSLC.readFile treeTempPath
  let expectedTree = decompress tmpTree1
  tmpTree2 <- BSLC.readFile (testGitDir ++ "/objects/00/13ee97b010dc8e9646f3c5a9841b62eb754f77")
  let actualTree = decompress tmpTree2

  -- Commit (bytestring of "cccccccccccccccccccccccccccccccccccccc"")
  let commitHash = BSC.pack (replicate 20 '\204')
  let commitTempPath = testGitDir ++ "/objects/cc/cccccccccccccccccccccccccccccccccccccc"
  contentCommit <- BSLC.readFile (testGitDir ++ "/objects/56/2c9c7b09226b6b54c28416d0ac02e0f0336bf6")
  case parse parseGitObject "" (BSLC.unpack (decompress contentCommit)) of
    Left err -> assertFailure (show err)
    Right result -> do saveGitObject (bsToHash $ encode commitHash) (gitObjectSerialize result) testGitDir

  tmpCommit1 <- BSLC.readFile commitTempPath
  let expectedCommit = decompress tmpCommit1
  tmpCommit2 <- BSLC.readFile (testGitDir ++ "/objects/56/2c9c7b09226b6b54c28416d0ac02e0f0336bf6")
  let actualCommit = decompress tmpCommit2

  let saveObjectTests =
        testGroup
          "saveObejct"
          [ testCase "blob object" $
              actualBlob @?= expectedBlob,
            testCase "tree object" $
              actualTree @?= expectedTree,
            testCase "commit object" $
              actualCommit @?= expectedCommit
          ]

  -- TODO: double check later Windows vs Unix
  -- remove created directory
  removeFile blobTempPath
  removeFile treeTempPath
  removeFile commitTempPath

  return saveObjectTests

-- Parsed and saved index file is exactly equal to the orginal index file
parseSaveIndexTests :: IO TestTree
parseSaveIndexTests = do
  let originalIndexFile = testGitDir ++ "/original_index"
  let actualIndexFile = testGitDir ++ "/index"
  x <- BSC.readFile originalIndexFile
  case parse parseIndexFile "" (BSC.unpack x) of
    Left err -> assertFailure (show err)
    Right result -> saveIndexFile (gitIndexSerialize result) testGitDir

  -- Read both files and compare contents
  expected <- BSC.readFile originalIndexFile
  actual <- BSC.readFile actualIndexFile
  -- Remove the checksum at the end
  let actualContent = BS.take (BS.length actual - 40) actual
  let parseSaveIndexTests =
        testGroup
          "parseSaveIndex"
          [ testCase "parseSaveIndex" $
              -- We are ignoring extensions and cache at the end so just checking if it is prefix
              isPrefixOf actualContent expected @?= True
          ]
  return parseSaveIndexTests

listBranchTests :: IO TestTree
listBranchTests = do
  -- Case1: when HEAD is pointing main (default)
  gitUpdateSymbRef "HEAD" "refs/heads/main" testGitDir
  expectedMainBranch <- readFile "test/TestData/expectedMainBranch.dat"
  (actualMainBranch, ()) <- capture (gitListBranch testGitDir)

  -- Case2: when HEAD is pointing test
  gitUpdateSymbRef "HEAD" "refs/heads/test" testGitDir
  expectedTestBranch <- readFile "test/TestData/expectedTestBranch.dat"
  (actualTestBranch, ()) <- capture (gitListBranch testGitDir)

  -- Case3: when HEAD is pointing to hash
  let blobHash = "f6f754dbe0808826bed2237eb651558f75215cc6"
  gitUpdateRef "HEAD" blobHash testGitDir
  expectedNoBranch <- readFile "test/TestData/expectedNoBranch.dat"
  (actualNoBranch, ()) <- capture (gitListBranch testGitDir)

  let gitListBarnchTest =
        testGroup
          "gitListBranchTest"
          [ testCase "gitListBranchTest" $
              actualMainBranch @?= expectedMainBranch,
            testCase "gitListBranchTest" $
              actualTestBranch @?= expectedTestBranch,
            testCase "gitListBranchTest" $
              actualNoBranch @?= expectedNoBranch
          ]

  -- go back to original ref
  gitUpdateSymbRef "HEAD" "refs/heads/main" testGitDir
  return gitListBarnchTest

addOrUpdateEntriesTests :: IO TestTree
addOrUpdateEntriesTests = do
  let paths = ["test/TestData/add_test1.txt", "test/TestData/add_test2.txt", "test/TestData/add_test3.txt"]
  let blobPath1 = ".test_haskgit/objects/07/23bccca901088280aa4713451f8c209c49172f"
  let blobPath2 = ".test_haskgit/objects/57/85a4e736f26aba8027f1ff87eaa9e7c5d2f785"
  let blobPath3 = ".test_haskgit/objects/8b/fc9d730d801f5f439e83b895bc0d5cb1b64b14"
  indexContent <- BSC.readFile (testGitDir ++ "/index")
  newIndex <- case parse parseIndexFile "" (BSC.unpack indexContent) of
    Left err -> assertFailure (show err)
    -- Remove the files from the index if they exist and add given files to the index
    Right index -> addOrUpdateEntries paths index testGitDir
  let addOrUpdateEntriesTests =
        testGroup
          "addOrUpdateEntriesTests"
          [ testCase "added files to the entries" $
              -- Check if files in paths are in the new index
              all (hasFile newIndex) paths @?= True
          ]
  -- Remove all the files created
  removeFile blobPath1
  removeFile blobPath2
  removeFile blobPath3

  return addOrUpdateEntriesTests
