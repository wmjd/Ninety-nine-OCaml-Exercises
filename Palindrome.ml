(* is_palindrome : string list -> bool *)

let is_palindrome ls = 
	let rec rev ls acc = (match ls with
		| [] -> acc
		| x::xs -> rev xs (x::acc))
	in ls = (rev ls [])
