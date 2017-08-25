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
		let _buildAssoc f = let (_, c, n, a) = f "s" in
			`Assoc [
				("read", `String c) ;
				("to_state", `String n) ;
				("write", `String c) ;
				("action", `String a)
			]
		in List.map _buildAssoc [
			fun c -> ("INIT", c, "MOVEEND", "RIGHT")
		]
	in

	let out = `Assoc [
		("name", (`String ("sim_" ^ name))) ;
		("alphabet", (`List (List.map (fun s -> `String s) (make_alphabet _input)))) ;
		("blank", `String "~") ;
		("initial", `String "INIT") ;
		("transitions", `List _transitions)
	]
	in pretty_print out

let () =
	match Array.length Sys.argv with
	| 3 -> generate Sys.argv.(1) Sys.argv.(2)
	| _ -> print_endline ("usage: " ^ Sys.argv.(0) ^ " jsonfile input")
