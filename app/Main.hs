module Main (main) where

import Data.ByteString (ByteString)
import Lib

main :: IO ()
main = someFunc

loadIndex :: IO ByteString
loadIndex = undefined

saveIndex :: ByteString -> IO ()
saveIndex = undefined

loadGitObject :: ByteString -> IO ByteString
loadGitObject = undefined

saveGitObject :: ByteString -> ByteString -> IO ()
saveGitObject = undefined

-- GitMonad: wrap

-- Question: having load and save in the main is not ideal?
-- Question about the scope (if it become infeasible, can we move branching to stretch goal)