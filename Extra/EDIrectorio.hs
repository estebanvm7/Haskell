import System.Directory
import Data.List

main = do
    directorio <- getCurrentDirectory
    direcciones <- getDirectoryContents directorio
    let lista = filter (any (`elem` ".")) direcciones
    putStr (unlines lista)
    
    
    --putStr (unlines direcciones)
    
    --filter doesNotBeginWithA "." direcciones 
    --putStr (direcciones !! 4)
    --putStr (unlines direcciones)


{-double_2nd :: [String] -> [String]
double_2nd [] = []
double_2nd (x:xs) = x : (head xs) : double_2nd (tail xs)-}
