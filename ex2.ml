let pari_div_by3 n =
	(if n mod 2 = 0 then "Pari" else "Dispari")^" e "^
 	(if n mod 3 = 0 then "" else " non" )^" divisibile per 3";;

print_string( pari_div_by3 50 );;
