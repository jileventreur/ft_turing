open Turing
open Json

let () =
	let count = ref 0 in
	let opts = [| "jsonfile" ; "input" |] in
	let pretty = ref false in
	let color = ref false in
	let getopt opts s =
		opts.(!count) <- s;
		incr count
	in
	Arg.parse [
		("--pretty", Arg.Set pretty , "A prettier output" );
		("--no-color", Arg.Set color, "A colored output" )
	] (getopt opts) "Usage: ft_turing [-h] jsonfile input";
	match !count with
	| 2 -> let data = Json.extract opts.(0) in
		(* Turing.debug data ; *)
		(* match String.contains opts.(1) '1' with
		| true -> print_endline "true"
		| false -> print_endline "false" *)
		(* CharSet.iter data.alphabet *)
		(* Array.fold  *)
		String.iter (fun e -> match CharSet.exists (fun c -> c = e) data.alphabet with
			| true -> ()
			| false -> print_endline "Ill formated input" ; ignore (exit 1)
		) opts.(1) ;
		print_string (Turing.exec !pretty (not !color) (Tape.create data opts.(1)) data)
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
