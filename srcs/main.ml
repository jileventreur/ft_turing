open Turing
open Json

let () =
	let count = ref 0 in
	let opts = [| "jsonfile" ; "input" |] in
	let getopt opts s =
		opts.(!count) <- s;
		incr count
	in
	Arg.parse [] (getopt opts) "Usage: ft_turing [-h] jsonfile input";
	match !count with
	| 2 -> let data = Json.extract opts.(0) in
		Turing.debug data ;
		print_string (Turing.exec (Tape.create data opts.(1)) data)
	| _ -> print_endline "Usage: ft_turing [-h] jsonfile input"

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
