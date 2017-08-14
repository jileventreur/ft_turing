open Json
open Opt
open Core.Std
open Printf

let print_header json =
	print_endline ("name: " ^ json.name) ;
	print_string "alphabet:" ;
	let print_alphablet c =
		print_string " ";
		print_char c
	in CharSet.iter print_alphablet json.alphabet ;
	print_endline "" ;
	print_string ("blank: ");
	print_char json.blank ;
	print_endline ""
	(* print_string "states:" ;
	let print_states c =
		print_string " ";
		print_string c
	in StringSet.iter print_states json.states ;
	print_endline "" ;
	print_string "finals:" ;
	StringSet.iter print_states json.finals ;
	print_endline "" ; *)
	(* print_endline ("initial: " ^ json.initial) ; *)
	(* print_endline "transitions:";
	let _print s = print_endline ("\t" ^ (fst s)) in
	List.iter _print json.transitions *)

let _print_transition _i _t = match _t with
| Defined (c, a, i) -> begin
	let s = "<" ^ (Char.to_string (Char.of_int_exn (_i))) ^ ">\t'" ^ (Char.to_string c) in
	match a with
	| Turing.Right -> print_endline (s ^ "' : Right | " ^ (string_of_int i))
	| Turing.Left -> print_endline (s ^ "' : Left | " ^ (string_of_int i))
	end
| Undefined -> () (* print_endline "\tUndefined" *)

let _print_table t = match t with
| Normal (s, _t) ->
	print_endline ("Normal: " ^ s) ;
	Array.iteri _print_transition _t
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
	Array.iter data.table _print_table

let rec exec tape data =
	let s = (Tape._to_string ~color:false tape) in
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
		let transition = t.(Char.to_int (Tape.get tape)) in
		(* Missing functional update for the tape *)
		match transition with
		| Defined (c,a,i) -> begin
			Tape.set tape c ;
			begin match a with
			| Turing.Right -> Tape.right tape
			| Turing.Left -> Tape.left tape
			end ;
			print_int data.state_register ;
			print_string " -> " ;
			print_int i ;
			print_endline "" ;
			exec tape {data with state_register = i}
		end
		| Undefined -> "Undefined state"
	end
	| Final s -> s ^ "\n"

let () =
	let argv = getopt Sys.argv in
	print_endline argv.jsonfile ;
	let data = extract argv.jsonfile in
	debug data ;
	print_string (exec (Tape.create argv.input) data)

(*
let () =
	let tape = Tape.create "c" in
	Tape.color_print tape;

	Tape.right tape;
	Tape.print_infos tape;
	Tape.color_print tape;
	Tape.left tape;
	Tape.left tape;
	Tape.print_infos tape;
	Tape.color_print tape;
	Tape.left tape;
	Tape.set tape 'g';
	Tape.right tape;
	Tape.right tape;
	Tape.right tape;
	Tape.color_print tape;
	Tape.print_infos tape;
	Tape.set tape 'g';
	Tape.left tape;
	Tape.color_print tape;
	Tape.print_infos tape;

 *)
