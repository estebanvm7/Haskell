import Trie

main = do
	let a = empty
--	let b = insertNode a 's'
--	let c = insertNode b 'a'
--	let d = insertNode c 'c'
--    --print (verifyNode d 'c')
--	let res = getNode d 'a'
	--let e = insertInTrie d "ca" (Node {element = 'a', path = ["2do nivel"], childrens = []})
--	let e = ignoreNode d 'a'
--	let e = addPath (getNode d 'c') "/home/mauricio/" : ignoreNode d 'c'
--	let nodo = (Node {element = 'o', path = [], childrens = []})
--	let f = addSubPath (getNode e 'c') nodo : ignoreNode e 'c'
	--let f = insertInTrie a "a"
--	print (f)
--	let x = getNode f 'p'
--	print (x) 
	let b = insertInTrie a "a"
	let c = insertInTrie b "aca"
	let d = insertInTrie c "acaba"
	let e = insertInTrie d "acabado"
	let f = insertInTrie e "acabarias"
	let g = insertInTrie f "acasias"
	let h = addPathToNode g "acasias" "/home/mauricio/acasias.txt"
	let j = addPathToNode h "acabado" "/home/mauricio/acasias.hs"
	let l = addPathToNode j "acabado" "/home/mauricio/repetido.hs"
	
	--let e = insertInTrie d "so"
	print (l)
