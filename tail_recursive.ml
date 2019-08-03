(* scrivere una funzione con la tail_recursive che preso in input un intero n sommi le prime n potenze di 2 a partire da 0.  *)
let somma_potenze n =
    let rec aux n i acc =
        match n with
            0 -> acc
           |_ -> aux (n-1) (i+1) (acc+( int_of_float( 2.0**(float_of_int i) ) )) 
    in aux n 0 0;;
(*print_int( somma_potenze 4 );;*)


(* scriver una funzione tail_recursive the prenda un intero e restituisca una string con la lista dei divisori del numero: divisori 10 = "1 2 5 10" *)
let divisori n =
    let rec aux n i acc =
        match i with (* pattern matching quando i arriva a n+1*)
            x when x = n+1 -> acc
           |_            -> if n mod i = 0 then (aux n (i+1) (acc^" "^(string_of_int i)) ) 
                            else (aux n (i+1) acc)
    in aux n 1 "";;
(*print_string( divisori 10 );;*)


(* scriver una funzione che rovesci una stringa "pippo"->"oppip" *)
let string_of_char c = String.make 1 c;;

let rovescia_stringa s =
    let rec aux s length acc =
        match length with
            0 -> acc
           |_ -> aux s (length-1) (acc^(string_of_char s.[length-1]))
    in aux s (String.length s) "";
(*print_string( rovescia_stringa "pippo" );;*)
