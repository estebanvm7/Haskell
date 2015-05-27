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

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topPath = do
  names <- unsafeInterleaveIO $ getDirectoryContents topPath
  let
    properNames =
      filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topPath </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then unsafeInterleaveIO $ getRecursiveContents path
      else return [path]
  return (concat paths)

esSufijo [] elemento = False
esSufijo (cabeza : cola) elemento =
  if isSuffixOf cabeza elemento
  then True
  else esSufijo cola elemento

filtrar :: [FilePath] -> [String] -> [[Char]]
filtrar files ext = filter (\x -> esSufijo ext x) files

--noExisteEn [] elemento = True
--noExisteEn (cabeza : cola) elemento =
--  if cabeza == elemento
 -- then False
 -- else noExisteEn cola elemento 
 
insertar :: [String] -> Trie -> Trie
insertar [] trie = trie
insertar (cabeza:cola) trie     
  | contains trie cabeza = insertar cola trie
  | otherwise = insertar cola (insertInTrie trie cabeza)

agregarRuta :: [String] -> String -> Trie -> Trie
agregarRuta [] ruta trie = trie
agregarRuta (cabeza : cola) ruta trie 
  | containsPath trie cabeza ruta = agregarRuta cola ruta trie 
  | otherwise = agregarRuta cola ruta (addPathToNode trie cabeza ruta)

--crearTrie :: Trie -> [String] -> Trie
--crearTrie trie [] = trie
--crearTrie trie (cabeza : cola)
--  | 
--  | otherwise

abrirArchivo ruta = do
  contenido <- readFile ruta
  let lista = words contenido
  return $ show lista
  
archivoFiltrado :: [String] -> [String] -> [String]  
archivoFiltrado [] stop = []
archivoFiltrado lista stop =  filter (\x -> noExisteEn stop x) lista
 
main = do
  let vacio = empty
  [path] <- getArgs
  contenido <- readFile "extensionesTexto.txt"
  stopWords <- readFile "Stopwords.txt"
  let palabras = words contenido 
  let stop = words stopWords
  files <- unsafeInterleaveIO $ getRecursiveContents path
  let listaPath = filtrar files palabras

  

  archivo <- map (\x -> abrirArchivo x) listaPath
  let archivoFiltrado = archivoFiltrado archivo stop
  let trie1 = map (\x -> insertar x vacio) archivoFiltrado
  --let x = insertar ["abc","acb","vcx"] a
  --let z = agregarRuta ["abc","ac","vc"] "/home/mauricio/repetido.hs" x
  writeFile "trie.txt" (show trie1)
  return (0)

