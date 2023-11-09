module GitParser
  ( parseGitObject,
  )
where

import Data.ByteString as B (ByteString, length)
import Data.ByteString.Char8 as BC (ByteString, pack)
import GitObject (GitObject, newBlob, newCommit, newTree)
import Text.ParserCombinators.Parsec
import Text.Read (readMaybe)

byteSize :: String -> Int
byteSize s = B.length (BC.pack s)

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
        else return (GitObject.newBlob bytesize content filename)

parseTree :: String -> Parser GitObject
parseTree filename = do
  _ <- string "tree "
  -- TODO: check data integrity of byte size
  bytesizeString <- manyTill digit (char '\0')
  case readMaybe bytesizeString of
    Nothing -> fail "Not a valid byte size in tree file"
    Just bytesize -> do
      elems <- manyTill parseGitTreeEntry eof
      return (GitObject.newTree bytesize elems filename)
  where
    parseGitTreeEntry :: Parser (String, String, ByteString)
    parseGitTreeEntry = do
      filemode <- manyTill digit (char ' ') :: Parser String
      name <- manyTill anyChar (char '\0')
      -- Read 20 bytes of SHA-1 hash
      sha <- Text.ParserCombinators.Parsec.count 20 anyChar
      return (filemode, name, BC.pack sha)

-- GitCommit = (tree, parent, author, committer, message, timestamp)
parseCommit :: String -> Parser GitObject
parseCommit filename = do
  _ <- string "commit "
  _ <- manyTill digit (char '\0')
  _ <- string "tree "
  rootTree <- manyTill anyChar (char '\n')
  _ <- string "parent "
  parent <- manyTill anyChar (char '\n')
  _ <- string "author "
  authorLine <- manyTill anyChar (char '\n')
  let authorWords = words authorLine
  --   TODO: in the future, also consider the timezone
  let authorInfo = (head authorWords, last (init authorWords), head (init authorWords))
  _ <- string "committer "
  committerLine <- manyTill anyChar (char '\n')
  let committerWords = words committerLine
  let committerInfo = (head committerWords, last (init committerWords), head (init committerWords))
  _ <- string "\n"
  message <- manyTill anyChar (char '\n')
  return (GitObject.newCommit (BC.pack rootTree) [BC.pack parent] authorInfo committerInfo message filename)

parseGitObject :: String -> Parser GitObject
parseGitObject filename = parseBlob filename <|> parseTree filename <|> parseCommit filename