import System.Environment

main = do
	args <- getArgs
	valoresDigitados args

valoresDigitados [] = return ()
valoresDigitados (head:tail) = do
	contenido <- readFile "Stopwords.txt"
	resultado (lines contenido) head
	valoresDigitados tail

resultado [] palabra =  print palabra
resultado (head:tail) palabra = do
	if head /= palabra then resultado tail palabra else return ()
