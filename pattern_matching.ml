(* primo modo per fare pattern matching  *)

(* patter matching esplicito *) 
(* supponiamo che n sia un simbolo *)
let rec fact n =
    match n with
        0 -> 1
        | x -> x*fact(x-1);;

(* pattern matching implicito 
non c'e' parametro di fact perche' 'function' ha gia' un parametro implicito definito nell'ultima riga, 
avrei in altre parole un conflitto di nomi nella chiamata ricorsiva di fact(n-1) *)
let rec fact = function
    0 -> 1
    |n -> n*fact(n-1);;


(* scrivere una funzione da string->char->int che conti le occorrenze di un caratte in una stringa: usare String.sub e String.length *)
let rec occorrenze word c = 
    match word with
        "" -> 0
       | _ -> ( occorrenze( String.sub s 1 (String.length s-1) ) c ) + if s.[0] = c then 1 else 0;;
