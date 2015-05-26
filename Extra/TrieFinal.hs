module Trie where

data Node = Node { element :: Char,
		   path :: [String],
		   childrens :: [Node]
		 } deriving (Read, Show)

--data Trie  = Trie { list :: [Node]} deriving (Read, Show)
type Trie = [Node]

--empty :: Ord a => Trie a
empty = []

--insertar
insertNode :: Trie -> Char -> Trie
insertNode trie letter = (Node {element = letter, path = [], childrens = []}):trie

verifyNode :: Trie->Char->Bool
verifyNode [] char = False
verifyNode (head:tail) char = (char == (element head)) || verifyNode tail char

findNodeAux :: Trie->Char->Int->Int
findNodeAux [] letter acum = acum
findNodeAux (head:tail) letter acum
	| (letter == (element head)) = acum
	| otherwise = findNodeAux tail letter (acum + 1)

findNode :: Trie->Char->Int
findNode trie letter = findNodeAux trie letter 0 

getNode :: Trie->Char->Node
getNode trie char = trie !! findNode trie char

ignoreNode :: Trie -> Char -> Trie
ignoreNode trie letter = filter (\x -> element x /= letter) trie

addPath :: Node -> String -> Node
addPath node dir = (node {element = element node, path = (dir : path node), childrens = childrens node }) 


addSubPath :: Node -> Node -> Node
addSubPath father son = (father {element = element father, path = path father, childrens = (son : childrens father) }) 

insertInTrie :: Trie -> [Char] -> Trie
insertInTrie trie [letter]
	| verifyNode trie letter = trie 
	| otherwise = insertNode trie letter
insertInTrie trie (head : tail) 
	| verifyNode trie head = (getNode trie head) {element = element (getNode trie head), path = path (getNode trie head), childrens = (insertInTrie (childrens (getNode trie head)) tail)} : (ignoreNode trie head)
	| otherwise = insertInTrie (insertNode trie head) (head : tail) 
	
searchNode :: Trie -> [Char] -> Node
searchNode trie [letter]
	| verifyNode trie letter = getNode trie letter
	| otherwise = (Node {element = ' ', path = [], childrens = []})
searchNode trie (head : tail)
	| verifyNode trie head = searchNode (childrens (getNode trie head)) tail
	| otherwise = (Node {element = ' ', path = [], childrens = []})
	
addPathToNode :: Trie -> [Char] -> String -> Trie
addPathToNode trie [letter] dir 
	| verifyNode trie letter = addPath (getNode trie letter) dir : (ignoreNode trie letter)
	| otherwise = trie
addPathToNode trie (head : tail) dir
	| verifyNode trie head = (getNode trie head) { element = element (getNode trie head), path = path (getNode trie head), childrens = addPathToNode (childrens (getNode trie head)) tail dir } : (ignoreNode trie head)
	| otherwise = trie
	
--verificar que existe el primer Char? meterse al nodo y buscar en los hijos del nodo, lo meto
--insertInTrie trie [letter]
	
 
