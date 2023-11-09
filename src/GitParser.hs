module GitParser
  ( parseBlob,
    parseCommit,
    parseTree,
  )
where

import Data.ByteString as B
import Data.ByteString.Char8 as BC
import Data.Functor
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
        else return (GitObject.newBlob (BC.pack content) filename)

parseGitTreeEntry :: Parser (String, String, ByteString)
parseGitTreeEntry = do
  filemode <- manyTill digit (char ' ') :: Parser String
  name <- manyTill anyChar (char '\0')
  -- Read 20 bytes of SHA-1 hash
  sha <- Text.ParserCombinators.Parsec.count 20 anyChar
  return (filemode, name, BC.pack sha)

parseTree :: Parser GitObject
parseTree = do
  _ <- string "tree "
  manyTill parseGitTreeEntry eof <&> GitObject.newTree

parseCommit :: Parser String
parseCommit = do
  string "commit "

parseGitObject :: Parser GitObject
parseGitObject = undefined