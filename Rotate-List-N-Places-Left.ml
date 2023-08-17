let rotate ls n =
	let rec partition ls i acc = 
		if i = n then (ls, List.rev acc) else partition (List.tl ls) (i+1) ((List.hd ls)::acc)
	in let pre, post =  partition ls 0 []
	in pre @ post 
