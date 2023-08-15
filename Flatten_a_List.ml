(* Flatten a nested list structure *)

type 'a node = One of 'a | Many of 'a node list

let rec flatten = function
	| [] -> []
	| (One node)::nodes -> node :: (flatten nodes)
	| (Many nodes)::nodez -> (flatten nodes) @ (flatten nodez)
