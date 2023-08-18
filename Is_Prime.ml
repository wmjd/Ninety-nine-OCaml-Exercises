(* Usual trick is stop counting after the counter > sqrt n *)

let is_prime n =
	let rec aux i =
		if n < i*i then true
		else if n mod i = 0 then false
		else aux @@ succ i
	in if n = 1 then false else aux 2
