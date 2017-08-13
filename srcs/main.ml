open Json
open Opt
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

let debug data =
	print_endline ("name: " ^ data.name) ;
	print_string "alphabet: [ " ;
	let _print_alphabet c = print_char c ; print_string " " in
	CharSet.iter _print_alphabet data.alphabet ;
	print_endline "]"

let () =
	let argv = getopt Sys.argv in
	print_endline argv.jsonfile ;
	print_header (extract argv.jsonfile) ;

	let data = {
		name = "template name" ;
		alphabet = CharSet.of_list [ 'd' ; 'a' ; 'b' ] ;
		blank = '.' ;
		table = [|
			Normal( "testA", [|
				Defined( 'a', Turing.Right, 1 )
			|] )
		|] ;
		state_register = 1
	} in
	print_endline "---- ---- debug ---- ----" ;
	print_endline ("input : " ^ argv.input) ;
	debug data

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
