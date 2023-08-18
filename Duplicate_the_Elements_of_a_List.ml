let duplicate ls =
	let rec d ls acc = match ls with
		| [] -> List.rev acc
		| x::xs -> d xs (x::x::acc)
	in d ls []
