module Main where

import Data.List
import Control.Monad ( mapM_)
import System.Environment ( getArgs )
import Trie

--Buscar Elemento
--Busca el argumento dentro del arbol Trie
search :: [String] -> Trie -> [String]
search [a] trie = (path (searchNode trie a))
search (head : tail) trie = intersect (path (searchNode trie head)) (search tail trie) 

main = do
  arg <- getArgs
  input <- readFile "/home/.indice"
  let trie = read input :: [Node]
  let node = search arg trie
  mapM_ putStrLn node

