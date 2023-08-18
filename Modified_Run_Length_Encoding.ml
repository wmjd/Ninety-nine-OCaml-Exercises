
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode ls = 
	let rec enc ls prev n acc = match (ls, prev) with
		| (x::xs, Some p) when p<>x -> enc xs (Some x) 1 ((if n = 1 then (One p) else Many(n,p))::acc)
		| (x::xs, _ ) -> enc xs (Some x) (n+1) acc
		| ([], Some p) -> (if n = 1 then (One p) else Many(n,p)) :: acc
		| ([], None) -> acc  
	in List.rev (enc ls None 0 [])
