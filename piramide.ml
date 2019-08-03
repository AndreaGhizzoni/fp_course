let rec print_line_numbers n =
    if n = 0 then ""
    else ( print_line_numbers (n-1) )^" "^( string_of_int n );;


let rec piramide n =
    if n = 0 then ""
    else ( piramide (n-1) )^"\n"^( print_line_numbers n );;


print_string( piramide 5 );;
