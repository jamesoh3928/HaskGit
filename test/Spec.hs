import qualified Data.ByteString.Lazy.Char8 as BSLC
import HaskGit
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: IO TestTree
tests =
  testGroup "HaskGit Tests"
    <$> sequence
      [ testCase "testHash" testHashTest
      ]

-- FIXME: as `testHash` is an IO function, should it be tested?
-- or, pure function is better? and how to implement it?
testHashTest :: IO TestTree
testHashTest = undefined
