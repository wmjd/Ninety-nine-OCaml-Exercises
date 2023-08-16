(* This implementation should include the elements at both bounds, i.e. this is not like the python implementation which excludes the upper bound. *)

let slice ls i j =
	let rec keep ls n acc =
		if n>j then acc else keep (List.tl ls) (n+1) ((List.hd ls)::acc) 
	in let rec skip ls n =
		if n<i then skip (List.tl ls) (n+1) else keep ls n []
	in List.rev (skip ls 0)
