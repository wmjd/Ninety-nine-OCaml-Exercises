(* Find the last two elements in a list *)

let rec last_two = function
	[] | [_] -> None
	| x::y::[] -> Some (x,y)
	| x::y::zs -> last_two (y::zs) 
