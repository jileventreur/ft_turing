
(* let print_data data = 
	let _print_alphabet c = print_char c ; print_string " " in
	printf "-------------------- DATA --------------------\n";
	printf "name : %s\n" data.name;
	printf "alphabet : ";
	CharSet.iter _print_alphabet data.alphabet;
	printf "blank : %c" data.blank;
	print_table data.table;
	printf "state register : %d" data.state_register 
	printf "----------------------------------------------\n"
 *)
(* let action_to_string action = match action with
	| Left -> "LEFT"
	| Right -> "LEFT"

let print_table table =
	let print_transition index transition = match transition with
	| Undefined -> ()
	| Definded (write, action, next)-> printf "{read : %c, write : %c, action %s, next %d}\n" (int_to_char index) write (action_to_string action) next
	let print_state state = match state with
	| Normal (name, transition) -> begin printf "Normal (%s) [\n" name ; print_transition transition; printf "]\n" end
	| Final (name) -> printf "Final (%s) [\n" name
	in *)