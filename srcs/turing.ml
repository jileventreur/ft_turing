open Json

let _print_transition _i _t = match _t with
| Defined (c, a, i) -> begin
	let s = "<" ^ (Core.Std.Char.to_string (Core.Std.Char.of_int_exn (_i))) ^ ">\t'" ^ (Core.Std.Char.to_string c) in
	match a with
	| Right -> print_endline (s ^ "' : Right | " ^ (string_of_int i))
	| Left -> print_endline (s ^ "' : Left | " ^ (string_of_int i))
	end
| Undefined -> () (* print_endline "\tUndefined" *)

let _print_table t = match t with
| Normal (s, _t) ->
	print_endline ("Normal: " ^ s) ;
	Core.Std.Array.iteri _print_transition _t
| Final (s) -> print_endline ("Final: " ^ s)

let debug data =
	print_endline ("name: " ^ data.name) ;
	print_string "alphabet: [ " ;
	let _print_alphabet c = print_char c ; print_string " " in
	CharSet.iter _print_alphabet data.alphabet ;
	print_endline "]" ;
	print_string "blank: ";
	print_char data.blank ;
	print_endline "" ;
	Core.Std.Array.iter data.table _print_table

let rec exec tape data =
	let s = (Tape.to_string ~color:false tape) in
	print_string ( "[ " ^ s ) ;
	let rec _loop i = if i < 32 then begin
		print_char '.' ;
		_loop (i + 1)
	end in
	_loop (String.length s) ;
	print_string " ] | " ;
	let state = data.table.(data.state_register) in
	match state with
	| Normal (s, t) -> begin
		let transition = t.(Core.Std.Char.to_int (Tape.get tape)) in
		match transition with
		| Defined (c,a,i) -> begin
			let tape = Tape.set tape c in
			print_int data.state_register ;
			print_string " -> " ;
			print_int i ;
			print_endline "" ;
			let raise_tape t = match t with
				| Tape.Ok t -> exec t {data with state_register = i}
				| Tape.Error s -> s
			in match a with
			| Right -> raise_tape (Tape.right tape)
			| Left -> raise_tape (Tape.left tape)

		end
		| Undefined -> "Undefined state"
	end
	| Final s -> s ^ "\n"
