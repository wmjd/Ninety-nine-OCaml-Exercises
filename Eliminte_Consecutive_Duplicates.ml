let eliminate_dups ls =
	let rec aux = function 
		| [] , _ -> []
		| x::xs , Some y when x=y -> aux (xs, Some x)  
		| x::xs , _ -> x::(aux (xs, Some x))
	in aux (ls, None)
