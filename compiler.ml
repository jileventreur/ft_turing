open Yojson.Basic.Util

type state = {
	read: string;
	to_state: string;
	write: string;
	action: string;
}

let pretty_print json = print_endline (Yojson.Basic.pretty_to_string json)
let alphabet json = json |> member "alphabet" |> to_list |> filter_string
let blank json = json |> member "blank" |> to_string
let states json = json |> member "states" |> to_list |> filter_string
let initial json = json |> member "initial" |> to_string
let finals json = json |> member "finals" |> to_list |> filter_string
let transitions json = json |> member "transitions" |> to_assoc
let read json = json |> member "read" |> to_string
let to_state json = json |> member "to_state" |> to_string
let write json = json |> member "write" |> to_string
let action json = json |> member "action" |> to_string
let generate file input =
	let json = Yojson.Basic.from_file file in

	print_endline (file ^ "\t" ^ input) ;
	pretty_print json ;

	let _alphabet alphabet = List.fold_left (fun p s -> p ^ s) "" alphabet in
	let _mapNames alphabet = List.filter (fun s -> not (List.exists (fun e -> e = s) alphabet))
		[ "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "A"; "B"; "C"; "D"; "E"; "F" ] in
	let statesName = _mapNames (alphabet json) in
	let statesCode = List.mapi (fun i s -> (s, List.nth statesName i)) (states json) in
	List.iter (fun (a, b) -> Printf.printf "%s: %s\n" a b) statesCode ;
	let _states states = List.fold_left (fun p s -> p ^ List.assoc s statesCode) "" states in
	let _initial state = List.assoc state statesCode in
	let _finals states = List.fold_left (fun p s -> p ^ List.assoc s statesCode) "" states in
	let _transitions transitions =
		let _to_letter s = match s with
		| "RIGHT" -> "R"
		| "LEFT" -> "L"
		| _ -> print_endline ("Warning, not a valid action: " ^ s) ; ""
		in
		let _to_string s = (read s) ^ (List.assoc (to_state s) statesCode) ^ (write s) ^ (_to_letter (action s)) in
		let _encode k v = List.fold_left (fun p a -> p ^ (List.assoc k statesCode) ^ (_to_string a)) "" (to_list v) in
		List.fold_left (fun p (k,v) -> p ^ (_encode k v)) "" transitions
	in
	let _input = "|" ^ _alphabet (alphabet json) ^
		"|" ^ (blank json) ^
		"|" ^ _states (states json) ^
		"|" ^ _initial (initial json) ^
		"|" ^ _finals (finals json) ^
		"|" ^ _transitions (transitions json) ^
		"|" ^ input ^ "|"
	in
	print_endline _input ;
	let name =
		ignore (Str.search_forward (Str.regexp "/\\(.*\\)") file 0) ;
		Str.matched_group 1 file
	in

	let make_alphabet input =
		let rec _loop i str lst =
		if i < String.length str then
			if List.exists (fun s -> s = (String.make 1 str.[i])) lst then
				_loop (i + 1) str lst
			else
				_loop (i + 1) str (lst @ [ String.make 1 str.[i] ])
		else
			lst
		in
		_loop 0 input [ "~"; "L"; "R" ]
	in

	let _transitions =
		let _buildAssoc (name, f) =
			let _lst = List.map (fun s ->
				let (c, n, w, a) = f s in
				`Assoc [
					("read", `String c) ;
					("to_state", `String n) ;
					("write", `String w) ;
					("action", `String a)
				]
			) (make_alphabet _input)
			in (name, `List _lst)
		in List.map _buildAssoc ([
			("INIT",
				fun c -> (c, "MOVEEND", c, "RIGHT")) ;
			("MOVEEND", function "|" -> ("|", "MOVEEND_1", "|", "RIGHT")
				| c -> (c, "MOVEEND", c, "RIGHT")) ;
			("CHECK_ALPHA", function "|" -> ("|", "BACK_STATES_1", "|", "RIGHT")
				| c -> (c, "MOVEBEGIN_" ^ c ^ "_1", "~", "LEFT")) ;
			("CHECK_STATE", function "|" -> ("|", "BACK_FINALS_1", "|", "RIGHT")
				| "R" -> ("R", "CHECK_STATE", "R", "RIGHT")
				| "L" -> ("L", "CHECK_STATE", "L", "RIGHT")
				| c -> (c, "CHECK_STATE_" ^ c ^ "_1", "~", "LEFT")) ;
			("CHECK_STATE_ALPHA", function "|" -> ("|", "HALT", "|", "RIGHT")
				| c -> (c, "CHECK_STATE_ALPHA_" ^ c ^ "_1", "~", "LEFT")) ;
			("CHECK_FINALS", function "|" -> ("|", "BACK_INITIAL_1", "|", "RIGHT")
				| c -> (c, "CHECK_FINALS_" ^ c ^ "_1", "~", "LEFT")) ;
			("CHECK_INITIAL", function "|" -> ("|", "BACK_BLANK_1", "|", "RIGHT")
				| c -> (c, "CHECK_INITIAL_" ^ c ^ "_1", "~", "LEFT")) ;
			("CHECK_BLANK", function "|" -> ("|", "MOVEBEGIN", "|", "RIGHT")
				| c -> (c, "CHECK_BLANK_" ^ c ^ "_1", "~", "LEFT")) ;
			("MOVEBEGIN", function "~" -> ("~", "HALT", "~", "RIGHT")
				| c -> (c, "MOVEBEGIN", c, "LEFT"))

		] @ (List.map (
			function 5 -> ( "MOVEEND_5",
				function "|" -> ("|", "CHECK_ALPHA", "|", "RIGHT")
				| c -> (c, "MOVEEND_5", c, "RIGHT")
			)
			| i -> ( "MOVEEND_" ^ (string_of_int i),
				function "|" -> ("|", "MOVEEND_" ^ (string_of_int (i + 1)), "|", "RIGHT")
				| c -> (c, "MOVEEND_" ^ (string_of_int i), c, "RIGHT")
			)
		) [ 1; 2; 3; 4; 5 ]) @ (List.map (
			function (_c, 6) -> ( "MOVEBEGIN_" ^ _c ^ "_6",
				function "|" -> ("|", "MATCH_ALPHA_" ^ _c, "|", "LEFT")
				| c -> (c, "MOVEBEGIN_" ^ _c ^ "_6", c, "LEFT")
			)
			| (_c, i) -> ( "MOVEBEGIN_" ^ _c ^ "_" ^ (string_of_int i),
				function "|" -> ("|", "MOVEBEGIN_" ^ _c ^ "_" ^ (string_of_int (i + 1)), "|", "LEFT")
				| c -> (c, "MOVEBEGIN_" ^ _c ^ "_" ^ (string_of_int i), c, "LEFT")
			)
		) (List.flatten (List.map ( fun x -> List.map ( fun y -> (x, y) ) [1 ; 2; 3; 4; 5; 6] ) (make_alphabet _input) ))
		) @ (List.map (
			function _c -> ( "MATCH_ALPHA_" ^ _c,
				function "|" -> ("|", "ALPHA_ERROR", "|", "LEFT")
				| c when c = _c -> (c, "WRITE_ALPHA_" ^ _c, c, "RIGHT")
				| c -> (c, "MATCH_ALPHA_" ^ _c, c, "LEFT")
			)
		) (make_alphabet _input)) @ (List.map (
			function _c -> ( "WRITE_ALPHA_" ^ _c,
				function "~" -> ("~", "CHECK_ALPHA", _c, "RIGHT")
				| c -> (c, "WRITE_ALPHA_" ^ _c, c, "RIGHT")
			)
		) (make_alphabet _input)) @ (List.map (
			function 3 -> ( "BACK_STATES_3",
				function "|" -> ("|", "CHECK_STATE", "|", "RIGHT")
				| c -> (c, "BACK_STATES_3", c, "LEFT")
			)
			| i -> ( "BACK_STATES_" ^ (string_of_int i),
				function "|" -> ("|", "BACK_STATES_" ^ (string_of_int (i + 1)), "|", "LEFT")
				| c -> (c, "BACK_STATES_" ^ (string_of_int i), c, "LEFT")
			)
		) [ 1; 2; 3 ]) @ (List.map (
			function (_c, 3) -> ( "CHECK_STATE_" ^ _c ^ "_3",
				function "|" -> ("|", "MATCH_STATE_" ^ _c, "|", "LEFT")
				(* | c when c = _c -> (c, "HALT", c, "RIGHT") *)
				| c -> (c, "CHECK_STATE_" ^ _c ^ "_3", c, "LEFT")
			)
			| (_c, i) -> ( "CHECK_STATE_" ^ _c ^ "_" ^ (string_of_int i),
				function "|" -> ("|", "CHECK_STATE_" ^ _c ^ "_" ^ (string_of_int (i + 1)), "|", "LEFT")
				(* | c when c = _c -> (c, "HALT", c, "RIGHT") *)
				| c -> (c, "CHECK_STATE_" ^ _c ^ "_" ^ (string_of_int i), c, "LEFT")
			)
		) (List.flatten (List.map ( fun x -> List.map ( fun y -> (x, y) ) [1 ; 2; 3] ) (make_alphabet _input) ))
		) @ (List.map (
			function _c -> ( "MATCH_STATE_" ^ _c,
				function "|" -> ("|", "STATE_ERROR", "|", "LEFT")
				| c when c = _c -> (c, "WRITE_STATE_" ^ _c, c, "RIGHT")
				| c -> (c, "MATCH_STATE_" ^ _c, c, "LEFT")
			)
		) (make_alphabet _input)) @ (List.map (
			function _c -> ( "WRITE_STATE_" ^ _c,
				function "~" -> ("~", "CHECK_STATE_ALPHA", _c, "RIGHT")
				| c -> (c, "WRITE_STATE_" ^ _c, c, "RIGHT")
			)
		) (make_alphabet _input)) @ (List.map (
			function (_c, 5) -> ( "CHECK_STATE_ALPHA_" ^ _c ^ "_5",
				function "|" -> ("|", "MATCH_STATE_ALPHA_" ^ _c, "|", "LEFT")
				(* | c when c = _c -> (c, "HALT", c, "RIGHT") *)
				| c -> (c, "CHECK_STATE_ALPHA_" ^ _c ^ "_5", c, "LEFT")
			)
			| (_c, i) -> ( "CHECK_STATE_ALPHA_" ^ _c ^ "_" ^ (string_of_int i),
				function "|" -> ("|", "CHECK_STATE_ALPHA_" ^ _c ^ "_" ^ (string_of_int (i + 1)), "|", "LEFT")
				(* | c when c = _c -> (c, "HALT", c, "RIGHT") *)
				| c -> (c, "CHECK_STATE_ALPHA_" ^ _c ^ "_" ^ (string_of_int i), c, "LEFT")
			)
		) (List.flatten (List.map ( fun x -> List.map ( fun y -> (x, y) ) [1 ; 2; 3; 4; 5] ) (make_alphabet _input) ))
		) @ (List.map (
			function _c -> ( "MATCH_STATE_ALPHA_" ^ _c,
				function "|" -> ("|", "STATE_ERROR", "|", "LEFT")
				| c when c = _c -> (c, "WRITE_STATE_ALPHA_" ^ _c, c, "RIGHT")
				| c -> (c, "MATCH_STATE_ALPHA_" ^ _c, c, "LEFT")
			)
		) (make_alphabet _input)) @ (List.map (
			function _c -> ( "WRITE_STATE_ALPHA_" ^ _c,
				function "~" -> ("~", "CHECK_STATE", _c, "RIGHT")
				| c -> (c, "WRITE_STATE_ALPHA_" ^ _c, c, "RIGHT")
			)
		) (make_alphabet _input)) @ (List.map (
			function 3 -> ( "BACK_FINALS_3",
				function "|" -> ("|", "CHECK_FINALS", "|", "RIGHT")
				| c -> (c, "BACK_FINALS_3", c, "LEFT")
			)
			| i -> ( "BACK_FINALS_" ^ (string_of_int i),
				function "|" -> ("|", "BACK_FINALS_" ^ (string_of_int (i + 1)), "|", "LEFT")
				| c -> (c, "BACK_FINALS_" ^ (string_of_int i), c, "LEFT")
			)
		) [ 1; 2; 3 ]) @ (List.map (
			function (_c, 2) -> ( "CHECK_FINALS_" ^ _c ^ "_2",
				function "|" -> ("|", "MATCH_FINAL_" ^ _c, "|", "LEFT")
				| c -> (c, "CHECK_FINALS_" ^ _c ^ "_2", c, "LEFT")
			)
			| (_c, i) -> ( "CHECK_FINALS_" ^ _c ^ "_" ^ (string_of_int i),
				function "|" -> ("|", "CHECK_FINALS_" ^ _c ^ "_" ^ (string_of_int (i + 1)), "|", "LEFT")
				| c -> (c, "CHECK_FINALS_" ^ _c ^ "_" ^ (string_of_int i), c, "LEFT")
			)
		) (List.flatten (List.map ( fun x -> List.map ( fun y -> (x, y) ) [1; 2] ) (make_alphabet _input) ))
		) @ (List.map (
			function _c -> ( "MATCH_FINAL_" ^ _c,
				function "|" -> ("|", "STATE_ERROR", "|", "LEFT")
				| c when c = _c -> (c, "WRITE_FINAL_" ^ _c, c, "RIGHT")
				| c -> (c, "MATCH_FINAL_" ^ _c, c, "LEFT")
			)
		) (make_alphabet _input)) @ (List.map (
			function _c -> ( "WRITE_FINAL_" ^ _c,
				function "~" -> ("~", "CHECK_FINALS", _c, "RIGHT")
				| c -> (c, "WRITE_FINAL_" ^ _c, c, "RIGHT")
			)
		) (make_alphabet _input)) @ (List.map (
			function 2 -> ( "BACK_INITIAL_2",
				function "|" -> ("|", "CHECK_INITIAL", "|", "LEFT")
				| c -> (c, "BACK_INITIAL_2", c, "LEFT")
			)
			| i -> ( "BACK_INITIAL_" ^ (string_of_int i),
				function "|" -> ("|", "BACK_INITIAL_" ^ (string_of_int (i + 1)), "|", "LEFT")
				| c -> (c, "BACK_INITIAL_" ^ (string_of_int i), c, "LEFT")
			)
		) [ 1; 2 ]) @ (List.map (
			function (_c, 1) -> ( "CHECK_INITIAL_" ^ _c ^ "_1",
				function "|" -> ("|", "MATCH_INITIAL_" ^ _c, "|", "LEFT")
				| c -> (c, "CHECK_INITIAL_" ^ _c ^ "_1", c, "LEFT")
			)
			| (_c, i) -> ( "CHECK_INITIAL_" ^ _c ^ "_" ^ (string_of_int i),
				function "|" -> ("|", "CHECK_INITIAL_" ^ _c ^ "_" ^ (string_of_int (i + 1)), "|", "LEFT")
				| c -> (c, "CHECK_INITIAL_" ^ _c ^ "_" ^ (string_of_int i), c, "LEFT")
			)
		) (List.flatten (List.map ( fun x -> List.map ( fun y -> (x, y) ) [1] ) (make_alphabet _input) ))
		) @ (List.map (
			function _c -> ( "MATCH_INITIAL_" ^ _c,
				function "|" -> ("|", "STATE_ERROR", "|", "LEFT")
				| c when c = _c -> (c, "WRITE_INITIAL_" ^ _c, c, "RIGHT")
				| c -> (c, "MATCH_INITIAL_" ^ _c, c, "LEFT")
			)
		) (make_alphabet _input)) @ (List.map (
			function _c -> ( "WRITE_INITIAL_" ^ _c,
				function "~" -> ("~", "CHECK_INITIAL", _c, "RIGHT")
				| c -> (c, "WRITE_INITIAL_" ^ _c, c, "RIGHT")
			)
		) (make_alphabet _input)) @ (List.map (
			function 3 -> ( "BACK_BLANK_3",
				function "|" -> ("|", "CHECK_BLANK", "|", "LEFT")
				| c -> (c, "BACK_BLANK_3", c, "LEFT")
			)
			| i -> ( "BACK_BLANK_" ^ (string_of_int i),
				function "|" -> ("|", "BACK_BLANK_" ^ (string_of_int (i + 1)), "|", "LEFT")
				| c -> (c, "BACK_BLANK_" ^ (string_of_int i), c, "LEFT")
			)
		) [ 1; 2; 3 ]) @ (List.map (
			function (_c, 1) -> ( "CHECK_BLANK_" ^ _c ^ "_1",
				function "|" -> ("|", "MATCH_BLANK_" ^ _c, "|", "LEFT")
				| c -> (c, "CHECK_BLANK_" ^ _c ^ "_1", c, "LEFT")
			)
			| (_c, i) -> ( "CHECK_BLANK_" ^ _c ^ "_" ^ (string_of_int i),
				function "|" -> ("|", "CHECK_BLANK_" ^ _c ^ "_" ^ (string_of_int (i + 1)), "|", "LEFT")
				| c -> (c, "CHECK_BLANK_" ^ _c ^ "_" ^ (string_of_int i), c, "LEFT")
			)
		) (List.flatten (List.map ( fun x -> List.map ( fun y -> (x, y) ) [1] ) (make_alphabet _input) ))
		) @ (List.map (
			function _c -> ( "MATCH_BLANK_" ^ _c,
				function "|" -> ("|", "STATE_ERROR", "|", "LEFT")
				| c when c = _c -> (c, "WRITE_BLANK_" ^ _c, c, "RIGHT")
				| c -> (c, "MATCH_BLANK_" ^ _c, c, "LEFT")
			)
		) (make_alphabet _input)) @ (List.map (
			function _c -> ( "WRITE_BLANK_" ^ _c,
				function "~" -> ("~", "CHECK_BLANK", _c, "RIGHT")
				| c -> (c, "WRITE_BLANK_" ^ _c, c, "RIGHT")
			)
		) (make_alphabet _input))

		)
	in

	let __finals = [ `String "HALT" ; `String "ALPHA_ERROR"; `String "STATE_ERROR" ] in
	let out = `Assoc [
		("name", (`String ("sim_" ^ name))) ;
		("alphabet", (`List (List.map (fun s -> `String s) (make_alphabet _input)))) ;
		("blank", `String "~") ;
		("states", `List ( (List.map (fun t -> `String (fst t)) _transitions) @ __finals )) ;
		("initial", `String "INIT") ;
		("finals", `List __finals) ;
		("transitions", `Assoc _transitions) ;
	]
	in
	pretty_print out ;
	Yojson.Basic.to_file (member "name" out |> to_string) out

let () =
	match Array.length Sys.argv with
	| 3 -> generate Sys.argv.(1) Sys.argv.(2)
	| _ -> print_endline ("usage: " ^ Sys.argv.(0) ^ " jsonfile input")
