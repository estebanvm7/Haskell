module Main where

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
      trace (topPath) names
  paths <- forM properNames $ \name -> do
    let path = topPath </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then unsafeInterleaveIO $ getRecursiveContents path
      else return [path]
  return (concat paths)

main :: IO ()
main = do
  [path] <- getArgs
  files <- unsafeInterleaveIO $ getRecursiveContents path
  forM_ files $ \file -> putStrLn $ file
