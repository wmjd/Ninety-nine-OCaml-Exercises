(* Inverse of the previous problem *)

type 'a rle =
  | One of 'a
  | Many of int * 'a

let decode ls =
	let (^) x n = 
		let rec aux n acc =
			if n > 0 then aux (n-1) (x::acc) else acc
		in aux n []
	in let rec aux ls acc = match ls with 
		| [] -> acc
		| One(x)::others -> aux others (x::acc)
		| Many(n,x)::others -> aux others ((x^n) @ acc)
	in List.rev (aux ls [])

(* I may be able to avoid append by passing acc to ^ (which has to be renamed since it wont be infix) that way I can efficiently cons the elements *)

