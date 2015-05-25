import Data.List

main = do
    contents <- readFile "test.txt"
    let palabras = words contents
    --print (head palabras)
	  let x = filter (isSuffixOf palabras [".cpp"])
    putStr (unlines (x))
    --mapM_ print palabras
