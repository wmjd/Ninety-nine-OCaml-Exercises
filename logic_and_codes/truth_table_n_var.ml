type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr;;


exception UnboundVariable of string
(*
let table prop1 prop2 expr =
	let make_lists =  
		let eval expr b1 b2 =
			let rec aux expr = match expr with
				| Var str -> (
					if str = prop1 then b1
					else if str = prop2 then b2
 					else raise (UnboundVariable str) )
				| Not e -> not (aux e)
				| And (e1, e2) -> (aux e1) && (aux e2)
				| Or (e1, e2) -> (aux e1) || (aux e2)
			in aux expr
		in [[true; true; eval expr true true];
			[true; false; eval expr true false];
			[false; true; eval expr false true];
			[false; false; eval expr false false]] 
	in make_lists 
*)

(* the machinery of aux can remain largely intact
eval can't take b1 b2. it takes a list of strings
*)

(* 
# table ["a"; "b"] (And (Var "a", Or (Var "a", Var "b")));;
- : ((string * bool) list * bool) list =
[([("a", true); ("b", true)], true); ([("a", true); ("b", false)], true);
 ([("a", false); ("b", true)], false); ([("a", false); ("b", false)], false)]
*)


let table vars expr =

	(* create lookup table that will hold bindings for variable names;
	the names will be wrapped in an option type;
	init'ing values with None and will update with Some values later *)

	let symtable = Hashtbl.create 1024

	in let rec eval expr = match expr with
				| Var str -> (Hashtbl.find symtable str)
				| Not e -> not (eval e)
				| And (e1, e2) -> (eval e1) && (eval e2)
				| Or (e1, e2) -> (eval e1) || (eval e2)

	(* generate each element of the cart power, update symtable, call aux expr *)

(* answers are of type:
		((string * bool) list * bool) list

   cart_pow will be of type:
		(string * bool) list list

   I will iterate through this list, updating the symtable with the values of  each element of cart_pow, and get the eval'd proposition zipped in
*)

	in let cart_pow vars =
		let rec aux vars acc = match vars with
		| [] -> acc::[]
		| v::[] -> ((v, true)::acc) :: ((v, false)::acc) :: []
		| v::vs -> (aux vs ((v, true)::acc)) @ (aux vs ((v, false)::acc))

		in aux vars []

	in let make_table tagged_bool_ls_ls =  
		let rec bind = function
			| [] -> ()
			| (tag, value)::bindings -> (
				(Hashtbl.add symtable tag value);
				(bind bindings)
			)
		in let rec iter tagged_bool_ls_ls acc_table_rows = match tagged_bool_ls_ls with
			| [] -> acc_table_rows
			| bindings::more -> (
				bind bindings;
				iter more ((bindings, eval expr)::acc_table_rows)
			)

		in iter tagged_bool_ls_ls []

	in cart_pow vars |> make_table










