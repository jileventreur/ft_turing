open Turing
open Json
open Core.Std


let debug data  =
    print_endline ("name: " ^ data.name) ;
    print_string "alphabet: [ " ;
    let _print_alphabet c = print_char c ; print_string " " in
    CharSet.iter _print_alphabet data.alphabet ;
    print_endline "]" ;
    print_string "blank: ";
    print_char data.blank ;
    print_endline "" ;
    let _print_transition _i _t = match _t with
    | Defined(c, a, i) -> begin   
        let s = "<" ^ (Char.to_string (Char.of_int_exn (_i))) ^ ">\t'" ^ (Char.to_string c) in
        match a with
        | Turing.Right -> print_endline (s ^ "' : Right | " ^ (string_of_int i))
        | Turing.Left -> print_endline (s ^ "' : Left | " ^ (string_of_int i))
        end
    | Undefined -> () (* print_endline "\tUndefined" *)
    in
    let _print_table t = match t with
    | Normal(s, _t) ->
        print_endline ("Normal: " ^ s) ;
        (* Array.iter _t _print_transition; *)
        Array.iteri _print_transition _t
    | Final(s) -> print_endline ("Final: " ^ s)
    in
    Array.iter data.table _print_table

let () = 
	ignore (Json.extract "json/unary_Sub.json")

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