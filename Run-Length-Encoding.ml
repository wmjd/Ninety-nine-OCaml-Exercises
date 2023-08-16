(* encode : 'a list -> int * string list *)

let encode ls = 
	let rec enc ls prev n acc = match (ls, prev) with
		| (x::xs, Some p) when p<>x -> enc xs (Some x) 1 ((n,p)::acc)
		| (x::xs, _ ) -> enc xs (Some x) (n+1) acc
		| ([], Some p) -> (n,p)::acc
		| ([], None) -> acc  
	in List.rev (enc ls None 0 [])
