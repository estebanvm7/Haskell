import System.Environment
import System.Directory
import System.IO
import Data.List

main = do
	args <- getArgs
	add args

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
