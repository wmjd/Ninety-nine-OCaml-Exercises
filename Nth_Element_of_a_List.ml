(* Find the N^th element of a list with zero-based counting. Raise exception if index is out of bounds. Rmk: OCaml has List.nth *)

(* Reimplementing List.nth behavior with exceptions *)

exception Failure of string

let rec nth ls n = match ls with 
	| [] -> raise (Failure "nth")
	| x::xs ->
		(match n with
			| 0 -> x
			| i -> nth xs (i-1))
	
(* Using options instead *)

let rec nth_opt ls n = match ls with 
	| [] -> None
	| x::xs ->
		(match n with
			| 0 -> Some x
			| i -> nth_opt xs (i-1))

(* Using an infix operator for practice *)

let rec (@@) ls n = 
	if ls = [] then None else
		if n != 0
			then (match ls with x::xs -> xs @@ (n-1))
			else Some (match ls with x::xs -> x)


(* Cleanest body syntax in another infix implementation *)

let rec (&&) ls n = match ls with
	| [] -> None
	| x::xs -> (if n = 0 then Some x else xs && (n-1))
