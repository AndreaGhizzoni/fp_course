type 'a tree = 
	Empty
    | Tr of 'a  * 'a tree * 'a tree;;


(* altezza dell'albero*)
let rec height = function
	Empty -> 0
   |Tr( _, l, r) -> 1 + max( height l ) ( height r );;
(*=================================================================================*)
(*
	creare una funzione longest_path: 'a tree -> 'a list che preso in input un 
	albero binario restituisca la lista del persorso più lungo a partire dalla radice 
*)
let longest_path tree =
	let rec aux lst = function 
		 Empty -> lst
 		|Tr( x, Empty, Empty ) -> x::lst
    	|Tr( x, l, r ) -> aux ( x::lst ) ( if((height l)>=(height r)) then l else r )
	in aux [] tree;;

(*=================================================================================*)