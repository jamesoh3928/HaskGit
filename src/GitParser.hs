module GitParser
  ( parseGitObject,
  )
where

import Data.ByteString as B (ByteString, length)
import Data.ByteString.Char8 as BC (pack)
import GitObject (GitObject, newBlob, newCommit, newTree)
import Text.ParserCombinators.Parsec
import Text.Read (readMaybe)

byteSize :: String -> Int
byteSize s = B.length (BC.pack s)

parseBlob :: Parser GitObject
parseBlob = do
  _ <- string "blob "
  bytesizeString <- manyTill digit (char '\0')
  case readMaybe bytesizeString of
    Nothing -> fail "Not a valid byte size in blob file"
    Just bytesize -> do
      content <- many anyChar
      -- data integrity check
      if bytesize /= byteSize content
        then fail "Byte size does not match in blob file"
        else return (GitObject.newBlob bytesize content)

parseTree :: Parser GitObject
parseTree = do
  _ <- string "tree "
  -- TODO: check data integrity of byte size
  bytesizeString <- manyTill digit (char '\0')
  case readMaybe bytesizeString of
    Nothing -> fail "Not a valid byte size in tree file"
    Just bytesize -> do
      elems <- manyTill parseGitTreeEntry eof
      return (GitObject.newTree bytesize elems)
  where
    parseGitTreeEntry :: Parser (String, String, ByteString)
    parseGitTreeEntry = do
      filemode <- manyTill digit (char ' ') :: Parser String
      name <- manyTill anyChar (char '\0')
      -- Read 20 bytes of SHA-1 hash
      sha <- Text.ParserCombinators.Parsec.count 20 anyChar
      return (filemode, name, BC.pack sha)

-- GitCommit = (tree, parent, author, committer, message, timestamp)
parseCommit :: Parser GitObject
parseCommit = do
  _ <- string "commit "
  bytesizeString <- manyTill digit (char '\0')
  case readMaybe bytesizeString of
    Nothing -> fail "Not a valid byte size in commit file"
    Just bytesize -> do
      _ <- string "tree "
      rootTree <- manyTill anyChar (char '\n')
      --   Assuming only one parent for MVP
      _ <- string "parent "
      parent <- manyTill anyChar (char '\n')
      _ <- string "author "
      authorName <- manyTill (noneOf "<") (char '<')
      authorEmail <- manyTill (noneOf ">") (char '>')
      spaces
      authorTimestamp <- manyTill digit (char ' ')
      --   TODO: consider timezone in the future
      _ <- manyTill anyChar newline
      _ <- string "committer "
      committerName <- manyTill (noneOf "<") (char '<')
      committerEmail <- manyTill (noneOf ">") (char '>')
      _ <- spaces
      committerTimestamp <- manyTill digit (char ' ')
      --   TODO: consider timezone in the future
      _ <- manyTill anyChar newline
      _ <- string "\n"
      message <- manyTill anyChar (char '\n')
      return (GitObject.newCommit bytesize (BC.pack rootTree) [BC.pack parent] (authorName, authorEmail, authorTimestamp) (committerName, committerEmail, committerTimestamp) message)

parseGitObject :: Parser GitObject
parseGitObject = parseBlob <|> parseTree <|> parseCommit