import Codec.Compression.Zlib (compress, decompress)
import Data.ByteString.Base16 (encode)
import Data.ByteString.Lazy.Char8 as BSLC
import GitParser (parseGitObject, parseIndexFile)
import HaskGit (gitHashObject)
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

--  Just a test function
gitShow :: String -> IO ()
gitShow filename = do
  x <- BSLC.readFile filename
  case parse parseGitObject "" (unpack (decompress x)) of
    Left err -> Prelude.putStrLn $ "Parse error: " ++ show err
    Right result -> print result

testParseIndex :: IO ()
testParseIndex = do
  x <- BSLC.readFile ".git/index"
  case parse parseIndexFile "" (unpack x) of
    Left err -> Prelude.putStrLn $ "Parse error: " ++ show err
    Right result -> print result

------------------------------------------------
-- Blob test
-- gitShow ".git/objects/f6/f754dbe0808826bed2237eb651558f75215cc6"

-- Tree test
-- gitShow ".git/objects/f6/e1af0b636897ed62c8c6dad0828f1172b9b82a"

-- Commit test
-- gitShow ".git/objects/56/2c9c7b09226b6b54c28416d0ac02e0f0336bf6"

decompressPrint filename = do
  x <- BSLC.readFile filename
  BSLC.putStrLn (decompress x)

readIndexFile :: IO ()
readIndexFile = do
  x <- BSLC.readFile ".git/index"
  Prelude.putStrLn (BSLC.unpack x)

testHash :: String -> IO ()
testHash filename = do
  content <- BSLC.readFile filename

  -- hashing without the header
  case parse parseGitObject "" (BSLC.unpack (decompress content)) of
    Left err -> Prelude.putStrLn $ "Parse error: " ++ show err
    Right result -> do
      print (encode (gitHashObject result))

-- CONTINUE: https://wyag.thb.lt/#staging-area:~:text=8.2.%20Parsing%20the-,index,-The%20index%20file