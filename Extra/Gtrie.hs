module Main where

import System.FilePath

data Node = Node { element :: Char,
		   path :: [FilePath],
		   childrens :: [Node]
		 } deriving (Read, Show)

data Trie  = Trie { list :: [Node]} deriving (Read, Show)

--empty :: Ord a => Trie a
empty = Trie {list = [Node {element = '\0', path = [], childrens = [] }]}  

--main :: IO ()
--main = do
  --let Trie a = []
  --putStrLn "hola"

