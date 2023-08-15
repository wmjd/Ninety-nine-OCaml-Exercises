(* Pack consecutive duplicates of list elements into sublists. (Looks like flatten of pack should be the identity function) *)

(* Could pass the last seen elt in an option type, like I did in "Eliminate_Consecutive_Duplicates.ml", or use patterns to look at consecutive elements*)


(* Frst attempt with hazy intuition into using mutually recursive helpers*)

let pack1 ls =
	let rec aux = function (* is called on new input *)
		| ([], acc) -> acc (* trivial case *)
		| (ls, acc)  -> make_packet ls acc []
	and make_packet ls acc packet = match ls with (* is called by itself until new input upon which aux is called *)
		| x::[] -> (x::packet) :: acc  
		| x::y::[] when x=y -> (x::y::packet) :: acc (* the last pattern handles case where guard is false *)
		| x::y::z when x=y -> make_packet (y::z) acc (x::packet) 
		| x::xs -> aux (xs, (x::packet) :: acc) 

	in List.rev (aux (ls, []))

(* Mutually recursive helper functions are unnecessary. I can check the trivial case once from pack and combine aux with make_packet. The previous implementation seems convulated *)

let pack2 ls =
	let rec aux ls acc packet = match ls with
		| x::[] -> (x::packet) :: acc
		| x::y::[] when x=y -> (x::y::packet) :: acc
		| x::y::z when x=y -> aux (y::z) acc (x::packet)
		| x::xs -> aux xs ((x::packet) :: acc) []
	in if ls = [] then [] else List.rev (aux ls [] []) 







