module Trie where 
-----------------------------------------------------------------------------------------------------------------------------------------
-- Creación de un nodo del arbol trie.
-- Este contiene la siguiente información: 
--				- Un char que contiene identificador del nodo
--				- Una lista de rutas. Estas son de la forma "/directorio1/directorio2/..."
--				- Una lista de nodos en representación de los nodos hijos del nodo actual (padre)
data Node = Node { element :: Char, path :: [String], childrens :: [Node] } deriving (Read, Show)
-----------------------------------------------------------------------------------------------------------------------------------------
-- Creación del trie
-- Reprensentado como una lista de nodos. Como los nodos hijos son una lista de nodos, este se denota como un sub-trie.
type Trie = [Node]
-----------------------------------------------------------------------------------------------------------------------------------------
-- Función que inicializa un trie vacio
empty = []

-- Función que toma un trie y un char en especifico y crea un nodo con el identificador correspondiente al char
-- en el nivel actual en que se encuentra el trie
insertNode :: Trie -> Char -> Trie
insertNode trie letter = (Node {element = letter, path = [], childrens = []}):trie

-- Función que verifica si en un nivel especifico del trie, existe un nodo con cierto identificador o no
verifyNode :: Trie->Char->Bool
verifyNode [] char = False
verifyNode (head:tail) char = (char == (element head)) || verifyNode tail char

-- Función auxiliar que busca un nodo a partir de un identificador en especial en un nivel especifico del trie
findNodeAux :: Trie->Char->Int->Int
findNodeAux [] letter acum = acum
findNodeAux (head:tail) letter acum
	| (letter == (element head)) = acum
	| otherwise = findNodeAux tail letter (acum + 1)

-- Función que identifica en la lista de nodos, la posición del nodo que estoy buscando, en un nivel en especifico del trie
findNode :: Trie->Char->Int
findNode trie letter = findNodeAux trie letter 0 

-- Función que me retorna el nodo en especifico que estoy utilisando, en un nivel en especifico del trie
getNode :: Trie->Char->Node
getNode trie char = trie !! findNode trie char

-- Función que toma una lista de nodos y me retorna una nueva lista de nodos, ignorando uno de los nodos
ignoreNode :: Trie -> Char -> Trie
ignoreNode trie letter = filter (\x -> element x /= letter) trie

--  Función que modifica un nodo, agregandole una nueva ruta a su lista personal
addPath :: Node -> String -> Node
addPath node dir = (node {element = element node, path = (dir : path node), childrens = childrens node }) 

-- Función que toma un nodo hijo y se lo inserta a un nodo padre en particular
--addSubPath :: Node -> Node -> Node
--addSubPath father son = (father {element = element father, path = path father, childrens = (son : childrens father) }) 

-- Función que inserta en un trie varios nodos correspondientes al "string" que estoy tratando de mapear al trie
insertInTrie :: Trie -> [Char] -> Trie
insertInTrie trie [letter]
	| verifyNode trie letter = trie 
	| otherwise = insertNode trie letter
insertInTrie trie (head : tail) 
	| verifyNode trie head = (getNode trie head) {element = element (getNode trie head), path = path (getNode trie head), childrens = (insertInTrie (childrens (getNode trie head)) tail)} : (ignoreNode trie head)
	| otherwise = insertInTrie (insertNode trie head) (head : tail) 
	
-- Función que buscaen la totalidad del trie y devuele el nodo asociado al "string" que se me ha soliditado. En caso de no encontrar nada
-- devuelve un nodo vacio.
searchNode :: Trie -> [Char] -> Node
searchNode trie [letter]
	| verifyNode trie letter = getNode trie letter
	| otherwise = (Node {element = ' ', path = [], childrens = []})
searchNode trie (head : tail)
	| verifyNode trie head = searchNode (childrens (getNode trie head)) tail
	| otherwise = (Node {element = ' ', path = [], childrens = []})

--	Función que agrega una ruta a un nodo en un trie, utilizando el identificador que le estoy dando como parámetro
addPathToNode :: Trie -> [Char] -> String -> Trie
addPathToNode trie [letter] dir 
	| verifyNode trie letter = addPath (getNode trie letter) dir : (ignoreNode trie letter)
	| otherwise = trie
addPathToNode trie (head : tail) dir
	| verifyNode trie head = (getNode trie head) { element = element (getNode trie head), path = path (getNode trie head), childrens = addPathToNode (childrens (getNode trie head)) tail dir } : (ignoreNode trie head)
	| otherwise = trie

contains :: Trie -> [Char] -> Bool
contains trie [] = True
contains trie (head : tail)
	| verifyNode trie head = True && contains (childrens (getNode trie head)) tail
	| otherwise = False

containsPath :: Trie -> [Char] -> String -> Bool
containsPath trie [] directory = False
containsPath trie (head : tail) directory
 	| noExisteEn (path (getNode trie head)) directory = containsPath (childrens(getNode trie head)) tail directory
 	| otherwise = True


noExisteEn [] elemento = True
noExisteEn (cabeza : cola) elemento =
  if cabeza == elemento
  then False
  else noExisteEn cola elemento 