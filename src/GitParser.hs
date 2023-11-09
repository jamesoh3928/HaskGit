module GitParser () where

import Data.ByteString as B
import Data.ByteString.Char8 as BC
import GitObject
import Text.Parsec.String
import Text.ParserCombinators.Parsec
import Text.Read (readMaybe)

byteSize :: String -> Int
byteSize s = B.length (BC.pack s)

-- parseByteSize :: Parser Int
-- parseByteSize = do
--   bytesize <- manyTill digit (char '\0')
--   return bytesize

paseBlob :: String -> Parser GitObject
paseBlob filename = do
  _ <- string "blob "
  bytesizeString <- manyTill digit (char '\0')
  case readMaybe bytesizeString of
    Nothing -> fail "Not a valid byte size"
    Just bytesize -> do
      content <- many anyChar
      if bytesize /= byteSize content
        then fail "Byte size does not match"
        else return (GitObject.newBlob content filename)

parseTree :: Parser String
parseTree = string "tree "

parseCommit :: Parser String
parseCommit = string "commit "