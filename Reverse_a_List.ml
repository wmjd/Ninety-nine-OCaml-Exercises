let rec r ls acc = match ls with
	| [] -> acc
	| x::xs -> r xs (x::acc)

let rev = function
	| ls -> r ls []

