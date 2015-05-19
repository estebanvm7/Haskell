import System.Environment

main = do
	args <- getArgs
	valoresDigitados args
	
archivoPalabras [fileName] elemento = do
    contenido <- readFile "Stopwords.txt"
    let lineas = lines contenido
        listaPalabras = zipWith (\n line -> line) [0..] lineas
    resultado listaPalabras elemento 

valoresDigitados [] = return () 
valoresDigitados (head:tail) = do
	archivoPalabras ["Stopwords.txt"] head 
	valoresDigitados tail

resultado [] palabra = return ()
resultado (head:tail) palabra = do
	if  head /= palabra then resultado tail palabra else print palabra

	
	
	
	
	
