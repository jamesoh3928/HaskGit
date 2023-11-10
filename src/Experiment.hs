import Codec.Compression.Zlib (compress, decompress)
import Data.ByteString.Lazy.Char8 as BSLC
import GitObject (GitObject)
import GitParser (parseGitObject)
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
  case parse parseGitObject "" (unpack (decompress x)) of
    Left err -> Prelude.putStrLn $ "Parse error: " ++ show err
    Right result -> print result

------------------------------------------------
-- Blob test
-- ".git/objects/f6/f754dbe0808826bed2237eb651558f75215cc6"

-- Tree test
-- ".git/objects/f6/e1af0b636897ed62c8c6dad0828f1172b9b82a"

-- Commit test
-- ".git/objects/56/2c9c7b09226b6b54c28416d0ac02e0f0336bf6"

decompressPrint filename = do
  x <- BSLC.readFile filename
  BSLC.putStrLn (decompress x)

-- TODO: delete
-- Tree: ".git/objects/02/c665efc85f165dd60563accf221cb6ecfbdbc4"
-- tree 427100644 .gitattributes∙:èP∙ΘSFO↔ΩΘZ6∩±¡sBh100644 .gitignoreΩ║G]╪♥4?┘hpe⌡!╚╕·D╠100644 CHANGELOG.md┴σ%↕kΓ┘╓x
-- 1Θ┌ù≥≥♥▄@`100644 HaskGit.cabal╫ä╠If╤÷Φ♠]        nΩIn$ε┴100644 README.mdÄ»∟A►$╧Ñó▐^Mu╡ù+ε100644 Setup.hsÜÖJ÷w░▀╘;v│τµ♦=dß40000 appdb>$(=¬╥ä░9╝╡O┤☻½ql40000 assets±↕√ΓP⌠╕°t[φr→µê╠⌠│ÿ°40000 docs↓?ƒ♠╗%ô:¡☺,=S`e¥\╬ß»40000 src¢î»i│9`Ωò·╕ª1║<╛`╧
-- 100644 stack.yaml╧º2y{%2╢eHt≡╫wò¥⌐√â140000 test$♥å╡U╗▀┼æ╔r╙⌡▒ëφ°M¶Å

-- Commit test
-- 562c9c7b09226b6b54c28416d0ac02e0f0336bf6