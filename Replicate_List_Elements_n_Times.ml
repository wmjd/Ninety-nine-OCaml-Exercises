let replicate ls n =
	let rec (^) x pow = match pow with
		| (0, ls) -> ls
		| (n, ls) -> x ^ (n-1, x::ls)
	in let rec aux = function
		| ([], acc) -> acc
		| (x::xs, acc) -> aux(xs, x^(n, acc))
	in List.rev (aux(ls, []))
		
