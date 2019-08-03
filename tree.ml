type 'a tree = 
	Empty
    | Tr of 'a  * 'a tree * 'a tree;;

(*
	definizione di albero:
	
	type 'a tree =
		  Empty
		| Tr of 'a     *      'a tree           *        'a tree;;
				[root]	 [sotto albero di sx ]     [sottoalbero di dx]
	
	Per definire un tipo: parola chiave "type".
	Il tipo 'a e' definito come vuoto ( empty ) o come
	Tr ( costruttore ) che prende una tripla formata da ad esempio:
	
	Tr( 3, Tr(2, Empty, Empty ), Empty )

                    3
	                /\	
	               2  E
                  /\
                 E  E
*)

(*=================================================================================*)
(*
	voglio contare gli elementi dell'albero
*)
let rec count = function
	  Empty -> 0
	| Tr( _ , l, r ) -> 1 + ( count l ) + ( count r );;

(*
	scrivere una funzione notsoclose: int, tree -> int -> bool 
	che presi un int e un tree restituisca vero se la differenza tra il nodo
	padre e il figlio è maggiore o strettamente maggiore dell'int passato
	come parametro

	PRIMA SOLUZIONE
*)
let rec notsoclose1 i tree = 
	match tree with
	  Tr( x, Empty, Empty ) -> true
	| Tr( r, Tr(c1, l1, r1), Tr(c2, l2, r2) ) -> if ( r-c1 )< i & ( r-c2 )< i then false
												 else notsoclose1 i ( Tr( c1, l1, r1 ) ) & 
												      notsoclose1 i ( Tr( c2, l2, r2 ) );; 

(*
	SECONDA SOLUZIONE
*)
let check l t1 lim = match t1 with
	  Empty -> true
	| Tr( l1, _ , _ ) -> abs( l - l1 ) > lim;;

let rec notsoclose2 tr lim = match tr with
	  Empty -> true
	| Tr( label, left, right ) -> 
		( check label left  lim ) & ( check label right lim ) &
		( notsoclose2 left lim ) & ( notsoclose2 right lim	);;

(*=================================================================================*)
(*
	scrivere una funzione  mindist: int -> 'a tree -> bool che restituisca vero se la 
	distanza delle foglie dalla radice e' almeno n
*)
let rec mindist n tree = 
	match tree with
		  Empty -> n < 0 
		  | Tr( _ , l, r ) -> mindist (n-1) l & mindist (n-1) r;;


(*=================================================================================*)
(*
	scrivere una funzione get_level : int -> 'a tree -> 'a list che preso in input
	un intero n e un albero restituisca la lista di tutti gli elementi dello stesso
	presenti nel livello n
*)
let get_level n tree = 
	let rec aux count = function
		| Empty -> []
		| Tr( n, l, r ) -> let lst = ( aux (count+1) l)@( aux(count+1) r) in
						   if count = n then x::lst else lst 
	in aux 1 tree;;

(* altezza dell'albero*)
let rec height = function
	Empty -> 0
   |Tr( _, l, r) -> 1 + max( height l ) ( height r );;

(*scrivere una funzione che restituisca tutti i livelli di un albero*)
let get_all_levels tree =
	let rec aux count = function
		0 -> count
   	   |n -> aux ( (get_level n tree)::count) (n-1)
   	in aux 0 (height tree);;

(*=================================================================================*)
(*
	scrivere una funzione swap: 'a tree -> 'a tree che scambi i sottoalberi di destra
	e di sinistra in maniera ricorsiva
*)
let rec swap = function 
	Empty -> Empty
   |Tr( n, sx, dx ) -> Tr( n, swap dx, swap sx );


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