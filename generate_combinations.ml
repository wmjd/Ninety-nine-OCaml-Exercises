(*
Generate the combinations of K distinct objects chosen from the N elements of a list.

In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.

# extract 2 ["a"; "b"; "c"; "d"];;
- : string list list =
[["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]

*)

(* at every step partition the problem: lists without elt @ lists with elt. this is not tail recursive. map calls are nested with  [] at the leaves *)

let rec extract k ls =
	if k <= 0 then   [[]] 
	else match ls with 
		| [] -> []
		| x::xs -> (
			(List.map (fun ls -> x::ls) (extract (pred k) xs))
			@ (extract k xs)
		)
