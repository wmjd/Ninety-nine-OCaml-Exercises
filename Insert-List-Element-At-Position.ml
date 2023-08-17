let insert_at elt k ls=
	let rec aux = function (*ls k acc*)
		| [], _, acc -> List.rev acc
		| x::xs, 0, acc -> aux (xs, pred 0, elt::acc)
		| x::xs, n, acc -> aux (xs, pred n, x::acc) 
	in aux (ls, k, [])
