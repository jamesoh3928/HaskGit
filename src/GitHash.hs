module GitHash (GitHash, bsToHash, gitHashValue, getHash) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- | GitHash is simply a wraper for "ByteString"
-- Invariant: the bytestring must be encoded in hexadecimal format to be stored in GitHash
newtype GitHash = GitHash ByteString
  deriving (Eq, Ord, Show)

bsToHash :: ByteString -> GitHash
bsToHash = GitHash

-- | Constructor that takes a ByteString and returns a GitHash
-- - @bs@: the length of bs is no longer than 40
gitHashValue :: ByteString -> Maybe GitHash
gitHashValue bs = if BS.length bs == 40 then Just (GitHash bs) else Nothing

getHash :: GitHash -> ByteString
getHash (GitHash bs) = bs
