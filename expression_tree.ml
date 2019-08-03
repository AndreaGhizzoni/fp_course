(*=================================================================================*)
(*
	definire la funzione eval_tree:expression_tree -> int che dato un albero di
	un'espressione, restituisca l'intero corrispondente all valutazione

	(6+3)*(2^3) = 72

	prevedere una funzione op_of_operator: operator -> int -> int -> int che data in 
	input una operazione la converte nella corrispondente funzione.
*)

type 'a tree = 
	Empty
    | Tr of 'a  * 'a tree * 'a tree;;

type operator = Sum | Mul | Pow;; 
(* Value è come foglia.*)
type expression_tree = 
	Value of int 
   |Tr of operator * expression_tree * expression_tree;;

let string_of_operator = function
	Sum -> "+"
   |Mul -> "*"
   |Pow	-> "^";;


(* op_of_operator prende in input un operator e restituisce la funzione corrispondente, la quale è int -> int -> int *)
let rec op_of_operator = function
	Sum -> (+)
   |Mul -> ( * )
   |Pow -> let rec aux x = function 
   				0 -> 1
   			   |n -> x * ( aux x (n-1) )
   		   in aux;;


let rec eval_tree = function
	Value x -> x
   |Tr( op, left, right ) -> op_of_operator op ( eval_tree left ) ( eval_tree right );;





