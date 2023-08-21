(* calculate number of coprimes r between 1 <= r < m *)

#use "coprime.ml"

let phi m = 
	let rec aux r c =
		if r = 0 then c else
		if coprime r m then aux (pred r) (succ c) else aux (pred r) c
	in aux (m-1) 0
