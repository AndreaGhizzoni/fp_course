let string_of_char c = String.make 1 c;;

(* funzione string->int->int->string->string che inserisca una string n volte nella posizione i all'interno della prima stringa  *) 
(* la funzione aux riceve la prima parte della stringa e ci concatena N volte la stringa scelta *)
let rec replica word i times s =
    let rec aux word count
        if count = 0 then word
        else aux (word^s)(count-1)
    in ( aux ( String.sub word 0 i ) times )^( String.sub word times ( String.length word-times );;
