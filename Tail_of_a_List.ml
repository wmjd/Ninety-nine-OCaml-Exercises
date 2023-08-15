(* Write a funcion last : 'a list -> a option that returns the Llast element of a list *)

(* tail rec; hence, linear space & time *)
let rec last = function
	| [] -> None 
	| x::[] -> Some x 
	| x::xs -> last xs 
