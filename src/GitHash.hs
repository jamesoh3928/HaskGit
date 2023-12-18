module GitHash (GitHash, bsToHash, getHash) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- | GitHash is a type represnting a SHA-1 hash used in Git.
-- Invariant: the bytestring must be encoded in hexadecimal format to be stored in GitHash
newtype GitHash = GitHash ByteString
  deriving (Eq, Ord, Show)

-- | Constructor that takes a ByteString and returns a GitHash
-- The length is 40 even though sha1 return 160 bits because it is encoded in hexadecimal format
-- I.e. 160 bits converted to hexadecimal -> 4 bits become 8 bits, therefore 320 bits -> 40 bytes
-- - @bs@: the length of bs should be exactly equal to 40
bsToHash :: ByteString -> Maybe GitHash
bsToHash bs = if BS.length bs == 40 then Just (GitHash bs) else Nothing

getHash :: GitHash -> ByteString
getHash (GitHash bs) = bs