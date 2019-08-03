let div_pari_by3 n =
    (if n mod 2 = 0 then "Pari" else "Dispari" )^" e "^
    (if n mod 3 = 0 then "" else " non" )^" divisibilie per 3";;

print_string( div_pari_by3 50 );;
