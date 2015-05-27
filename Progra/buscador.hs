module Main where

import Data.List
import Control.Monad ( mapM_)
import System.Environment ( getArgs )
import Trie

join :: [String] -> Trie -> [String]
join [a] trie = (path (searchNode trie a))
join (head : tail) trie = intersect (path (searchNode trie head)) (join tail trie) 

main = do
  arg <- getArgs
  input <- readFile "trie.txt"
  let trie = read input :: [Node]
  let node = join arg trie
  mapM_ putStrLn node

