(*
    Le liste sono definite come:
    
    list ::= []  >> costruzione lista vuota 
    x::list      >> elemento::lista

    [] e' una list vuota, 
    [1] e' una lista che contiene un'elemento,
    [(2, true); (3, false)] e' una list che contiene una tupla di int e bool,
    
    le liste le posso vedere come:
    [1;2;3] gli elementi sono tutti nella lista
    1::[2;3] prendere il primo elemento e la rimanente lista
    1::2::[3] prendere il primo e il secondo elemento e la rimanente lista
    1::2::3::[] prendere tutti gli elementi della lista e la rimanente lista vuota


    concatenazione di liste:
    @: 'a list -> 'a list -> 'a list

    [1;2]@[3;4] = [1;2;3;4]

*)

(* conta quanti elementi ci sono un una lista *)
let rec length = function
    [] -> 0
   |x::xs -> 1+length xs;;


(* costruire una funzione che applichi a tutti gli elementi della lista una funzione data *)
let rec map f = function
    [] -> []
   |x::xs -> (f x)::(map f xs);;
       

(* costruire una funzione che faccia l'insieme delle parti: [1;2]=[[];[1];[1,2]] *)
let rec powerset = function
    [] -> [[]]
    |x::xs -> let sub = powerset xs in
              sub @ ( map ( fun l -> x::l ) sub ) ;;

(* costruire una funzione che presi in input una lista ritorni una lista contentente l'elemento della lista e la sua posizione *)
let enumerate list = 
    let rec aux i = function
        [] -> []
        |x::xs -> (x, i) :: ( aux (i+1) xs )
    in aux 0 list;;


(* costruire una funzione che data una lista di int ritorni una lista di soli gli elementi strettamente crescenti nella lista data *)
let rec increasing = function
    [] -> []
    |xs::[] -> [xs]
    |x::y::xs -> if( x < y ) then x::(increasing xs)
                 else increasing (x::xs) ;;


(* costruire una funzione che applica una funzione agli elementi di una lista e ne ritorni una lista degli elementi applicabili della lista *)
let rec filter f list =
    match list with 
        [] -> []
        |x::xs -> if ( f x ) then x::(filter f xs)
                  else filter f xs;;








