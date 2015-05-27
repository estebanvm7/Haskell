module Main where

import Data.List
import Control.Monad ( mapM_)
import System.Environment ( getArgs )
import Trie

buscar :: [String] -> Trie -> [String]
buscar [a] trie = (path (searchNode trie a))
buscar (head : tail) trie = intersect (path (searchNode trie head)) (buscar tail trie) 

main = do
  arg <- getArgs
  input <- readFile "/home/.indice"
  let trie = read input :: [Node]
  let node = buscar arg trie
  mapM_ putStrLn node

