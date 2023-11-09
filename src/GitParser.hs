module GitParser
  ( parseBlob,
    parseCommit,
    parseTree,
  )
where

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

parseBlob :: String -> Parser GitObject
parseBlob filename = do
  _ <- string "blob "
  bytesizeString <- manyTill digit (char '\0')
  case readMaybe bytesizeString of
    Nothing -> fail "Not a valid byte size in blob file"
    Just bytesize -> do
      content <- many anyChar
      -- data integrity check
      if bytesize /= byteSize content
        then fail "Byte size does not match in blob file"
        else return (GitObject.newBlob content filename)

parseTree :: Parser String
parseTree = string "tree "

parseCommit :: Parser String
parseCommit = string "commit "