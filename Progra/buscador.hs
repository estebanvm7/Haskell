module Main where

import Data.List
import System.FilePath.Posix
import Control.Monad ( forM, forM_, liftM )
import Debug.Trace ( trace )
import System.Directory ( doesDirectoryExist, getDirectoryContents )
import System.Environment ( getArgs )
import System.FilePath ( (</>) )
import System.IO.Unsafe ( unsafeInterleaveIO )
import Trie

join :: [String] -> Trie -> [String]
join [a] trie = (path (searchNode trie a))
join (head : tail) trie = intersect (path (searchNode trie head)) (join tail trie) 

main = do
  arg <- getArgs
  input <- readFile "trie.txt"
  let trie = read input :: [Node]
  let node = join arg trie
  print (node)
