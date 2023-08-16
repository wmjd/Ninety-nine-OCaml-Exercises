let drop ls n =
	let rec d = function
		| ([], i, acc) -> acc
		| (x::xs, 0, acc) -> d(xs, n-1, acc) 
		| (x::xs, i, acc) -> d(xs, i-1, x::acc)
	in List.rev (d(ls, n-1, []))
