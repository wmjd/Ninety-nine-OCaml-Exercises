(* Generate random permutation of the elements in a list *)

(* Intuition:
 	getRandom returns a random list element
	aux recurs until ls = [] and passes getRandom::accumulator which is ret'd on base case. 

	Do I find List.length once and keep a count? getRandom has to be supplied with length info somehow. 

	ls |> List.length |> Random.int	
*)

let permute ls =
	let starlen = ls |> List.length 
	let getRandom ls acc 
 


