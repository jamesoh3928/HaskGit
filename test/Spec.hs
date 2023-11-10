import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.Hunit (Assertion, assertFailure)
-- import Test.Tasty.QuickCheck
main :: IO ()
main = tests >>= defaultMain
