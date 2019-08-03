(* simple method *)
let rec fact x =
    if x = 0 then 1
    else x*(fact (x-1));;

let binomiale n k =
    ( fact n )/( ( fact k  )*(fact (n-k) ));; 

(* print_int( binomiale 1 1 );; *)

(*=====================================================*)

(* right method *)

exception InvalidBounds;;

let binomialeRight n k =
    if n < k then
        raise InvalidBounds
    else let rec fact n =
            if n = 0 then 1
            else n*fact(n-1)
         in
            fact(n)/fact(n)*fact(n-k);;

let rec sumbin n =
    let rec aux k acc =
        if k = 0 then acc
        else aux( k-1 ) ( acc+(binomialeRight n k ))
    in aux n ( binomialeRight n 0 );;

print_int( sumbin 5  );;
