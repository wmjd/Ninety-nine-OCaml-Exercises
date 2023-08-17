exception Bad of string

let rand_select ls n=
	let rec get1 = function
		| ([], _, acc, Some elt) -> (elt, acc)
		| (x::xs, 0, acc, None) -> get1(xs, 0, acc, Some x)
		| (x::xs, n, acc, y) -> get1(xs, (n-1), x::acc, y)   
		| _ -> raise (Bad "input to get1") 
	in 
	let getn n =
		let rec aux ls n acc = if n = 0 then acc else
			match get1(ls, (ls |> List.length |> Random.int), [], None) with
				(x, ls) -> aux ls (n-1) (x::acc)
		in aux ls n []
	in getn n
