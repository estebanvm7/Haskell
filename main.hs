import Trie

main = do
	let a = empty
	let b = insertNode a 's'
	let c = insertNode b 'a'
	let d = insertNode c 'c'
	print (verifyNode d 'c')
	--let res = getNode d 'a'
	--let e = insertInTrie d "ca" (Node {element = 'a', path = ["2do nivel"], childrens = []})
	--let e = ignoreNode d 'a'
	let e = addPath (getNode d 'c') "/home/mauricio/" : ignoreNode d 'c'
	let nodo = (Node {element = 'o', path = [], childrens = []})
	let f = addSubPath (getNode e 'c') nodo : ignoreNode e 'c'
	print (f)
	
