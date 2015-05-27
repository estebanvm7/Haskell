module Main where

import Data.List
import Control.Concurrent
import System.FilePath.Posix
import Control.Monad ( forM, forM_, liftM )
import Debug.Trace ( trace )
import System.Directory ( doesDirectoryExist, getDirectoryContents )
import System.Environment ( getArgs )
import System.FilePath ( (</>) )
import System.IO.Unsafe ( unsafeInterleaveIO , unsafePerformIO)
import Data.Time
import Trie

--función que recorre el directorio recursivamente, que se le pasa como parametro
--devuelve una lista de todos las direcciones de los archivos en el directorio
ficherosRecursivos :: FilePath -> IO [FilePath]
ficherosRecursivos topPath = do
  names <- unsafeInterleaveIO $ getDirectoryContents topPath
  let
    properNames =
      filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topPath </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then unsafeInterleaveIO $ ficherosRecursivos path
      else return [path]
  return (concat paths)

--función que verifica si una palabra contiene uno de los sufijos de la lista que le entra como parametro
esSufijo [] elemento = False
esSufijo (cabeza : cola) elemento =
  if isSuffixOf cabeza elemento
  then True
  else esSufijo cola elemento

--funcion que elimina todos los ficheros que no tienen una extención que se encuentra en la lista de extenciones que le entra de parametro
filtrar :: [String] -> [String] -> [[Char]]
filtrar files ext = filter (\x -> esSufijo ext x) files

--función que inserta una palabra en el trie, recoriendo el arbol para ver si ya se encuentra
insertar :: [String] -> Trie -> Trie
insertar [] trie = trie
insertar (cabeza:cola) trie
  | contains trie cabeza = insertar cola trie
  | otherwise = insertar cola (insertInTrie trie cabeza)

--funcion que agrega una ruta en una palabra especifica que se encuentra en el arbol
agregarRuta :: [String] -> String -> Trie -> Trie
agregarRuta [] ruta trie = trie
agregarRuta (cabeza : cola) ruta trie
  | containsPath trie cabeza ruta = agregarRuta cola ruta trie
  | otherwise = agregarRuta cola ruta (addPathToNode trie cabeza ruta)

--fucnión que abre un archivo y retorna una lista con todas las palabras dentro de el
abrirArchivo :: FilePath -> IO [String]
abrirArchivo ruta = do
  contenido <- readFile ruta
  let lista = words contenido
  return (lista)

--función que recibe una lista de palabras y elimina todas las que se encuentren en la lista de stopwords que le entra como parametro
archivoFiltrado :: [String] -> [String] -> [String]
archivoFiltrado [] stop = []
archivoFiltrado lista stop =  filter (\x -> noExisteEn stop x) lista

--
          --  trie  lista rutas  stopwords
crearTrie :: Trie -> [String] -> [String] -> Trie
crearTrie trie [] stop = trie
crearTrie trie (cabeza : cola) stop = crearTrie (agregarRuta archivo cabeza (insertar (archivo) trie)) cola stop where archivo = archivoFiltrado (unsafePerformIO(abrirArchivo cabeza)) stop

main = do
  start <- getCurrentTime
  let vacio = empty
  [path] <- getArgs
  contenido <- readFile "extensionesTexto.txt"
  stopWords <- readFile "Stopwords.txt"
  let palabras = words contenido
  let stop = words stopWords
  forkIO $ do
  	files <- unsafeInterleaveIO $ ficherosRecursivos path
  	let listaPath = filtrar files palabras
  	let trie = crearTrie vacio listaPath stop
  	writeFile "/home/.indice" (show trie)
  stop <- getCurrentTime
  print $ diffUTCTime stop start
