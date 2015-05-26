import Data.List

main = do
    contents <- readFile "test.txt"
    let palabras = words contents
    if esSufijo palabras "esteban.cpp"
    then return True
    else return False

esSufijo [] elemento = False
esSufijo (cabesa : cola) elemento =
  if isSuffixOf cabesa elemento
  then True
  else esSufijo cola elemento


noExisteEn [] elemento = True
noExisteEn (cabesa : cola) elemento =
  if cabesa == elemento
  then False
  else noExisteEn cola elemento
