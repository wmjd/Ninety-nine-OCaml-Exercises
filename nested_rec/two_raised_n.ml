let rec two_raised n = 
	if n = 0 then 1
	else two_raised (pred n) + two_raised (pred n)

let two_raised2 n = 
	let rec two_to n acc =
		if n = 0 then (succ acc) 
		else two_to (pred n) (two_to (pred n) acc)
	in two_to n 0
	
