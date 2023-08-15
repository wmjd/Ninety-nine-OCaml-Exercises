let eliminate_dups ls =
	let rec aux = function 
		| [] , _ -> []
		| x::xs , Some y when x=y -> aux (xs, Some x)  
		| x::xs , _ -> x::(aux (xs, Some x))
	in aux (ls, None)

(* And do it with tail recursion: *)

let elimdup ls =  
	let rec aux = function (* takes ls, last, acc *) 
		| ([], _ , acc) -> acc (* if list is empty, no matter the last element, return the accumulated answer *) 
		| (x::xs, Some y, acc) when x=y -> aux (xs, Some x, acc)
		| (x::xs, _ , acc) -> aux (xs, Some x, x::acc)
	in List.rev (aux (ls, None, []))
		
