open Yojson.Basic.Util

module CharSet = Set.Make (
struct
	type t = char
	let compare = Pervasives.compare
end)

module StringSet = Set.Make (
struct
	type t = string
	let compare = String.compare
end)

module TransitionsMap = Map.Make(
struct
	type t = string
	let compare = Pervasives.compare
end)

type transitions_type = {
	read		: char;
	to_state	: string;
	write		: char;
	action		: string;
}

type parsing = {
	name : string;
	alphabet : CharSet.t;
	blank : char;
	states : StringSet.t;
	initial : string;
	finals : StringSet.t;
	(* transitions : transitions_type TransitionsMap.t *)
}

let fail = 0
let success = 1

let get_json filename = 
	try Yojson.Basic.from_file filename with
	| Yojson.Json_error str -> Printf.printf "ft_turing : %s : %s\n" filename str ; exit fail
	| Sys_error str -> Printf.printf "ft_turing : %s\n" str ; exit fail
	| _ -> Printf.printf "ft_turing : %s : Error opening json file\n" filename ; exit fail

let get_assoc json = 
	try to_assoc json with
	| Yojson.Json_error str -> Printf.printf "ft_turing : %s\n" str ; exit fail
	| _ -> Printf.printf "ft_turing : Unexpected format error\n" ; exit fail

let get_unique_member name assoc =
	let member_list = List.filter (fun (str, _) -> (String.equal str name)) assoc in
	match member_list with
	| [(_, obj)] -> obj
	| [] -> Printf.printf "ft_turing : Error missing member %s\n" name ; exit fail 
	| _ -> Printf.printf "ft_turing : Error member %s must be unique\n" name ; exit fail

let string_from_assoc assoc name =
	let jobj = get_unique_member name assoc in
	let str = try (to_string jobj) with 
	| _ -> Printf.printf "ft_turing : Error %s must be a string\n" name; exit fail
	in 
	str

let list_from_assoc assoc name =
	let jobj = get_unique_member name assoc in
	let lst = try (to_list jobj) with 
	| _ -> Printf.printf "ft_turing : Error %s must be a list\n" name; exit fail
	in 
	lst

let create_alphabet assoc =
	let lst = list_from_assoc assoc "alphabet" in
	let slst = filter_string lst in 
	if (List.length lst = 0) then begin
		Printf.printf "ft_turing : Error alphabet can't be empty\n"; exit fail
	end else if List.length lst <> List.length slst then begin
		Printf.printf "ft_turing : Error alphabet members must be only composed by strings\n"; exit fail
	end else if ((List.for_all (fun str -> String.length str = 1) slst) = false) then begin
		Printf.printf "ft_turing : Error alphabet members must be char\n"; exit fail
	end;
	let char_lst = List.map (fun str -> String.get str 0) slst in 
	CharSet.of_list char_lst

let create_blank assoc alphabet = 
	let jobj = get_unique_member "blank" assoc in
	let str = try (to_string jobj) with 
	| _ -> Printf.printf "ft_turing : Error blank must be a char\n"; exit fail
	in
	let blank = String.get str 0 in
	if (String.length str <> 1) then begin
		Printf.printf "ft_turing : Error blank must be a char\n"; exit fail
	end else if not (CharSet.mem blank alphabet) then begin
		Printf.printf "ft_turing : Error blank must be part of alphabet\n"; exit fail
	end;
	blank

let create_states assoc =
	let lst = list_from_assoc assoc "states" in
	let slst = filter_string lst in 
	if (List.length lst = 0) then begin
		Printf.printf "ft_turing : Error states can't be empty\n"; exit fail
	end else if List.length lst <> List.length slst then begin
		Printf.printf "ft_turing : Error states members must be only composed by strings\n"; exit fail
	end;
	StringSet.of_list slst

let create_initial assoc states = 
	let initial = string_from_assoc assoc "initial" in
	if not (StringSet.mem initial states) then begin
		Printf.printf "ft_turing : Error initial must be part of states\n"; exit fail
	end;
	initial

let create_finals assoc states = 
	let lst = list_from_assoc assoc "finals" in
	let slst = filter_string lst in
	let finals = StringSet.of_list slst in 
	if List.length lst = 0 then begin
		Printf.printf "ft_turing : Error finals can't be empty\n"; exit fail
	end else if List.length lst <> List.length slst then begin
		Printf.printf "ft_turing : Error finals members must be only composed by strings\n"; exit fail
	end else if not (StringSet.subset finals states) then begin
		Printf.printf "ft_turing : Error finals members must be a subset of states\n"; exit fail
	end;
	finals

let verif_transition_set assoc states finals = 
	let transitions_set = 
		let (slst, _) = List.split assoc in
		StringSet.of_list slst
	in
	let compared_set = StringSet.diff states finals in
	if not (StringSet.equal compared_set transitions_set) then begin 
		Printf.printf "ft_turing : Error finals states can not have transitions\n"; exit fail
	end

let create_transitions assoc states finals =
	let jobj = get_unique_member "transitions" assoc in
	let transitions_assoc = try (to_assoc jobj) with 
	| _ -> Printf.printf "ft_turing : Error transitions must be a assoc\n"; exit fail
	in
	verif_transition_set transitions_assoc states finals

let rec list_have_duplicate lst = match lst with
| head::tail when List.exists (fun e -> head = e) tail -> false
| head::tail -> list_have_duplicate tail
| [] -> true

let extract filename = 
	let json = get_json filename in
	let assoc = get_assoc json in
	let name = string_from_assoc assoc "name" in
	let alphabet = create_alphabet assoc in
	let states = create_states assoc in
	let data = {name = name;
	alphabet = alphabet;
	blank = create_blank assoc alphabet;
	states = states;
	initial = create_initial assoc states;
	finals = create_finals assoc states;
	(* transitions = create_transitions; *)
	} in 
	ignore (create_transitions assoc data.states data.finals)

(* 
condition de parsing :
	- Fichier existant et bien formate (pas d'exception a l'ouverture) OK
	- Toutes les keys attendue sont presentes sont uniques et sont du type attendu OK
 	- chaque etats de states correspond a une transitions sauf les transitionss finales
 	- blank appartient a alphabet OK
 	- initial et finals appartiennent a states OK 
 	- transitionss :	- map : 
 						- into array 
	- Pas de membre supplementaire
	- pas de doublons dans les lists
 
Transitions :
	- Les cles represente tous les etats excepte ceux finaux
	- les elements de transitions correspondents a la struct : 
		- Pas de doublon de read
		- Action = LEFT || RIGHT
		- to state pointe vers un etat
		- write et read correspondent a des elements de l'alphabet 

 preparer la structure de stocakge des donnes parsees : 
 	- name : string
 	- alphabet : set ?
 	- blank : char
 	- states : map key string ; val transitions 
 	- transitions : : map key : char ; val (string, char, tape -> unit)
 *)