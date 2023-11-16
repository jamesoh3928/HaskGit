{-# LANGUAGE TypeApplications #-}
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import HaskGit
import GitObject
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = adjustOption @QuickCheckTests (* 10) $
  testGroup "HaskGit"
      [ testGroup "GitObject"
          [
            -- return $ localOption @QuickCheckTests 1000 $ propNewBlob
            testProperty "propNewBlob@Int@String" $ propNewBlob,
            testProperty "propNewTree@Int@[(String, String, ByteString)]" $ propNewTree,
            testProperty "propNewCommit@Int@ByteString@[ByteString]@GitAuthor@GitCommitter@String" $ propNewCommit
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

instance Arbitrary ByteString where
  arbitrary = BS.pack <$> arbitrary

propNewBlob :: Int -> String -> Property
propNewBlob size content = size >= 0 ==> getBlobContent (newBlob size content) == content

propNewTree :: Int -> [(String, String, ByteString)] -> Property
propNewTree size entries = size >= 0 ==> f (newTree size entries)
  where
    f (Tree (s, e)) = s == size && length e == length entries

propNewCommit :: Int -> ByteString -> [ByteString] -> GitAuthor -> GitCommitter -> String -> Property
propNewCommit bytesize tree parents authorInfo committerInfo message =
  bytesize >= 0 ==>
  gitHashObject (newCommit bytesize tree parents authorInfo committerInfo message) True
    == gitHashObject obj True
    where
      obj = Commit (bytesize, tree, parents, authorInfo, committerInfo, message)

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

