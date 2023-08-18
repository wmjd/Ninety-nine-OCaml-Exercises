let split ls n =
	let rec aux = function (* ls, c, acc *)
		| ([], _, acc) -> (List.rev acc, [])
		| (ls, 0, acc) -> (List.rev acc, ls)  
		| (x::xs, n, acc) -> aux (xs, pred n, x::acc)
	in aux (ls, n, []) 
