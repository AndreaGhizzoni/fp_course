let string_of_char c = String.make 1 c;;

(* scrivere una funzione da string->char->int che conti le occorrenze di un caratte in una stringa: usare String.sub e String.length *)
let rec occorrenze s c = 
    match s with
         "" -> 0
        | _ -> ( occorrenze( String.sub s 1 (String.length s-1) ) c ) + if s.[0] = c then 1 else 0;;


(* scrivere una funzione da string->char->string*int che restituisca una tupla con la stringa senza il carattere passato e le occorenze di quel carattere *)
let rec countRemove s c =
    match s with
        "" -> ( "" , 0 )
       | s -> let (str, count) = countRemove( String.sub s 1 ( String.length s-1 )) c 
              in if s.[0]=c then ( str, count+1 )
                 else ( (string_of_char s[0])^str ,count );;

