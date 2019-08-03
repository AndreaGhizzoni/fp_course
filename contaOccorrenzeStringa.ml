let string_of_char c = String.make 1 c;; 

let rec substr string s e =
	if s = e then ""
	else ( string_of_char string.[s])^( substr string (s+1) e );;

let rec many_times str c =
	if String.length str  = 0 then 0
	else ( many_times (substr str 1 (String.length str)) c) +
	     if str.[0] = c then 1
 	     else 0;;

print_int ( many_times "my stringis" 'a');;	 
