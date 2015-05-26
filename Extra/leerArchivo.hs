import System.Environment
import System.Directory
import System.IO
import Data.List

main = do
	args <- getArgs
	view args
	
view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

