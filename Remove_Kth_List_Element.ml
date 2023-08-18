let remove_at ls k=
	let rec aux = function (*ls k acc*)
		| [], _, acc -> List.rev acc
		| x::xs, 0, acc -> aux (xs, pred 0, acc)
		| x::xs, n, acc -> aux (xs, pred n, x::acc) 
	in aux (ls, k, [])
