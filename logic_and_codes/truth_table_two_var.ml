type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr;;

(* A logical expression in two variables can then be written in prefix notation. For example, (a ∨ b) ∧ (a ∧ b) is written:

	And (Or (Var "a", Var "b"), And (Var "a", Var "b"));;

Define a function, table2 which returns the truth table of a given logical expression in two variables (specified as arguments). The return value must be a list of triples containing (value_of_a, value_of_b, value_of_expr).


*)

(* SCATTERED THOUGHTS:  

sounds like a tree recursion matching type constructors of bool_expr, and translating directly *)

exception UnboundVariable of string

let table prop1 prop2 expr =
	let make_lists =  
		let eval expr b1 b2 =
			let rec aux expr =	match expr with
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
