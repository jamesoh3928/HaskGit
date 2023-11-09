import Codec.Compression.Zlib (compress, decompress)
import Data.ByteString.Lazy.Char8 as BSLC

-- import Data.ByteString.Lazy as BLU

-- import System.Console.CmdArgs.Implicit

-- import GitObject

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

-- ToDO
-- After unpacking to string, parse the string and covert to correct format
-- (Depends on how the GitObject file content will be saved)
gitShow :: String -> IO ()
gitShow filename = do
  x <- BSLC.readFile filename
  Prelude.putStrLn (BSLC.unpack (decompress x))