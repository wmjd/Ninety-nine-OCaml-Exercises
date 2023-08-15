(* Find number of elements in a list. Reimplement List.length.  Bonus points for a tail recursion. i*)
let rec len x i = match x with
	| [] -> i
	| x::xs -> len xs (i+1)

let length x = len x 0


