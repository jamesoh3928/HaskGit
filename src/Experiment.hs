import Codec.Compression.Zlib (compress, decompress)
import Data.ByteString.Lazy.Char8 as BSLC
import GitObject (GitObject)
import GitParser (parseBlob)
import Text.Parsec (parse)

-- import System.Console.CmdArgs.Implicit

-- haskgit show hash_value
-- show latest commit
-- haskgit show

-- "d5332273b6680b6515ca0719afa54a7e6b5f6efc"
-- .git/objects/d5/332273b6680b6515ca0719afa54a7e6b5f6efc

dummyString :: String
dummyString = "commit d5332273b6680b6515ca0719afa54a7e6b5f6efc (HEAD -> main, origin/main, origin/HEAD)\nAuthor: James Oh <jo9347@cs.rit.edu>\nDate:   Wed Nov 1 14:49:36 2023 -0400\n\nAdd load/save functions"

-- Write and save git object
saveContent :: String -> String -> IO ()
saveContent filename content = BSLC.writeFile filename (compress (BSLC.pack content))

-- TODO: rename, use https://hackage.haskell.org/package/parsec
sToGitObject :: String -> GitObject
sToGitObject s = undefined

-- ToDO
-- After unpacking to string, parse the string and covert to correct format
-- (Depends on how the GitObject file content will be saved)
gitShow :: String -> IO ()
gitShow filename = do
  x <- BSLC.readFile filename
  -- BSLC.putStrLn (decompress x)
  -- y <- parseBlob (unpack x)
  -- print the gitobject
  -- Prelude.putStrLn (show y)
  case parse (parseBlob filename) "" (unpack (decompress x)) of
    Left err -> Prelude.putStrLn $ "Parse error: " ++ show err
    Right result -> print result

-- Blob test
-- ".git/objects/f6/f754dbe0808826bed2237eb651558f75215cc6"

test filename = do
  x <- BSLC.readFile filename
  BSLC.putStrLn (decompress x)
