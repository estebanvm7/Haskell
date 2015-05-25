module Main where

import Data.List
import Control.Monad ( forM, forM_, liftM )
import Debug.Trace ( trace )
import System.Directory ( doesDirectoryExist, getDirectoryContents )
import System.Environment ( getArgs )
import System.FilePath ( (</>) )
import System.IO.Unsafe ( unsafeInterleaveIO )


-- From Real World Haskell, p. 214
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topPath = do
  names <- unsafeInterleaveIO $ getDirectoryContents topPath
  let
    properNames =
      filter (`notElem` [".", ".."]) $
      trace ("Carpeta " ++ topPath) names
  paths <- forM properNames $ \name -> do
    let path = topPath </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then unsafeInterleaveIO $ getRecursiveContents path
      else return [path]
  return (concat paths)

--main :: IO ()
main = do
  [path] <- getArgs
  --let extensiones = [".h",".pch",".c",".hpp","cpp",".java","js",".make",".txt",".pl",".o",".hi"]
  contenido <- readFile "extensionesTexto.txt"
  files <- unsafeInterleaveIO $ getRecursiveContents path
  forM_ files $ \file -> evaluar (lines contenido) path file 	

--{tomo el archivo, lo evaluamos en una funcion que retornara true si la extension se encuentra o false si recorre toda la lista de extensiones y no la encuentra(extension vacia). Mete en la lista los archivos que retornan true}
evaluar [] valor file = return ()
evaluar (cabeza:cola) valor file = if isSuffixOf cabeza file == True then print file else evaluar cola valor file
