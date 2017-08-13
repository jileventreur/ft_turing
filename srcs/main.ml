open Json
open Opt

let print json =
	print_endline ("name: " ^ json.name) ;
	print_string "alphabet:" ;
	let print_alphablet c =
		print_string " ";
		print_char c
	in CharSet.iter print_alphablet json.alphabet ;
	print_endline "" ;
	print_string ("blank: ");
	print_char json.blank ;
	print_endline "";
	print_string "states:" ;
	let print_states c =
		print_string " ";
		print_string c
	in StringSet.iter print_states json.states ;
	print_endline "" ;
	print_string "finals:" ;
	StringSet.iter print_states json.finals ;
	print_endline "" ;
	print_endline ("initial: " ^ json.initial) ;
	print_endline "transitions:";
	let _print s = print_endline ("\t" ^ (fst s)) in
	List.iter _print json.transitions

let () =
	let argv = getopt Sys.argv in
	print (extract argv.jsonfile)

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
