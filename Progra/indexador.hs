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

noExisteEn [] elemento = True
noExisteEn (cabeza : cola) elemento =
  if cabeza == elemento
  then False
  else noExisteEn cola elemento 
 
--iterar :: [String] -> [String] -> Trie -> Trie
--iterar [] stop trie = trie
--iterar (head : tail) stop trie = do
--  let tiene = abrirArchivo head
--  let filtrados = archivoFiltrado tiene stop
--  let arbol = map (\x -> insertInTrie trie x) filtrados
  --let arbol2 = map (\x -> addPathToNode arbol x head) filtrados
--  return (iterar tail stop arbol)
   
abrirArchivo ruta = do
  contenido <- readFile ruta
  let lista = words contenido
  return (lista)
  
archivoFiltrado :: [String] -> [String] -> [String]  
archivoFiltrado [] stop = []
archivoFiltrado lista stop =  filter (\x -> noExisteEn stop x) lista
 

main = do
  let a = empty
  --[path] <- getArgs
  --contenido <- readFile "extensionesTexto.txt"
  --stopWords <- readFile "Stopwords.txt"
  --let palabras = words contenido 
  --let stop = words stopWords
  --files <- unsafeInterleaveIO $ getRecursiveContents path
  --let listaPath = filtrar files palabras
  --let lista =  ["hola","como","esta", "yolo"]
  --let algo = filter (\x -> noExisteEn stop x) lista 
  --let trie = iterar listaPath stop trieVacio
  --trie <- abrirArchivo "/home/mauricio/Escritorio/Lenguajes/source/verbos.txt" 
  --let archi = archivoFiltrado trie stop
  --let trie = iterar listaPath stop trieVacio
  --trie <- abrirArchivo "/home/mauricio/Escritorio/Lenguajes/source/verbos.txt" 
  --let a = archivoFiltrado trie stop
  let b = insertInTrie a "a"
  let c = insertInTrie b "aca"
  let d = insertInTrie c "acaba"
  let e = insertInTrie d "acabado"
  let f = insertInTrie e "acabarias"  
  let g = insertInTrie f "acasias"
  let h = addPathToNode g "acasias" "/home/mauricio/acasias.txt"
  let j = addPathToNode h "acabado" "/home/mauricio/acasias.txt"
  let l = addPathToNode j "acabado" "/home/mauricio/repetido.hs"
  
  writeFile "trie.txt" (show l)
  --print (listaPath)

