module GitHash (GitHash, gitHashValue, getHash) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

newtype GitHash = GitHash ByteString
  deriving (Eq, Ord, Show)

-- Constructor that takes a ByteString and returns a GitHash
gitHashValue :: ByteString -> Maybe GitHash
gitHashValue bs = if BS.length bs == 40 then Just (GitHash bs) else Nothing

getHash :: GitHash -> ByteString
getHash (GitHash bs) = bs