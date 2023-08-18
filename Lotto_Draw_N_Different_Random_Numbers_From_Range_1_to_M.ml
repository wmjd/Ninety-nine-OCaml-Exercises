(* Draw N different random numbers from the set 1..M. *)

#use "Extract_Random_Elements.ml"
#use "Create_List_Of_All_Ints_Between_Args_Inclusive.ml"

let lotto_select n m = rand_select (range 1 m) n 
	
(* Out of curiousity, I wanted to find an equivalent way of writing this expression that doesn't use any parentheses. OCaml apparently has operators for application and reverse application with certain associativity and precedence. I put this chart together and I think it is correct:

In order of precedence: 

LEFT ASSOCIATIVE
a b c = (a b) c

RIGHT ASSOCIATIVE
a @@ b @@ c = a @@ (b @@ c)

LEFT ASSOCIATIVE
a |> b |> c = (a |> b) |> c

Thus, 
	n |> rand_select @@ range 1 m = 
	n |> rand_select @@ (range 1) m =
	n |> rand_select @@ ((range 1) m) = 
	n |> (rand_select @@ ((range 1) m)) = 
	n |> (rand_select((range 1) m) =
	rand_select((range 1) m) n
Q.E.D *)

