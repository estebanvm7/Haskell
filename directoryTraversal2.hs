module Main where

import Data.List
import System.FilePath.Posix
import Control.Monad ( forM, forM_, liftM )
import Debug.Trace ( trace )
import System.Directory ( doesDirectoryExist, getDirectoryContents )
import System.Environment ( getArgs )
import System.FilePath ( (</>) )
import System.IO.Unsafe ( unsafeInterleaveIO )


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
esSufijo (cabesa : cola) elemento =
  if isSuffixOf cabesa elemento
  then True
  else esSufijo cola elemento

filtrar :: [FilePath] -> [String] -> [[Char]]
filtrar files ext = filter (\x -> esSufijo ext x) files



main = do
  [path] <- getArgs
  contenido <- readFile "extensionesTexto.txt"
  let palabras = words contenido 
  files <- unsafeInterleaveIO $ getRecursiveContents path
  let lista = filtrar files palabras
  print(lista)

