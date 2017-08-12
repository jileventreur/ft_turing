open Yojson.Basic.Util
open Printf

module CL = Core.Core_list

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

(* type transitions = {
	read		: char;
	to_state	: string;
	write		: char;
	action		: string;
}

type transition = Normal of char * action * state_index
				| Final
				| Undefined

(string * transition array) array;
type state_index = int
 *)

type parsing = {
	name : string;
	alphabet : CharSet.t;
	blank : char;
	states : StringSet.t;
	initial : string;
	finals : StringSet.t;
	transitions : (string * Yojson.Basic.json) list;
}

(* 
Turing needed : 
	- name : output
	- blank : verif tape input
	- initial : state index
	- states : array (transition * name) 
*)

let fail = 0
let success = 1

let lstfst lst = let (fst, _) = List.split lst in fst 
let lstsnd lst = let (_, snd) = List.split lst in snd 
let turing_members = CL.sort String.compare ["name"; "alphabet"; "blank"; "states"; "initial"; "finals"; "transitions"]

let get_json filename = 
	try Yojson.Basic.from_file filename with
	| Yojson.Json_error str -> printf "ft_turing : %s : %s\n" filename str ; exit fail
	| Sys_error str -> printf "ft_turing : %s\n" str ; exit fail
	| _ -> printf "ft_turing : %s : Error opening json file\n" filename ; exit fail

let get_assoc json = 
	try to_assoc json with
	| Yojson.Json_error str -> printf "ft_turing : %s\n" str ; exit fail
	| _ -> printf "ft_turing : Unexpected format error\n" ; exit fail

let get_member json name = 
	let jobj = member name json in
	match jobj with
	| `Null -> printf "ft_turing : Error missing member %s\n" name ; exit fail
	| _ -> jobj

let obj_to_lst ?(name = "member") jobj =
	try to_list jobj with
	| _ -> printf "ft_turing : %s must be a list\n" name ; exit fail

let obj_to_string ?(name = "member") jobj =
	try to_string jobj with
	| _ -> printf "ft_turing : %s must be a string\n" name ; exit fail


let create_alphabet json =
	let lst = obj_to_lst ~name:"alphabet" (get_member json "alphabet") in
	let slst = filter_string lst in
	if (List.length lst = 0) then begin
		printf "ft_turing : Error alphabet can't be empty\n"; exit fail
	end else if List.length lst <> List.length slst then begin
		printf "ft_turing : Error alphabet members must be only composed by strings\n"; exit fail
	end else if ((List.for_all (fun str -> String.length str = 1) slst) = false) then begin
		printf "ft_turing : Error alphabet members must be one char long\n"; exit fail
	end else if CL.contains_dup slst then begin
		printf "ft_turing : Error alphabet can't have duplicates members\n"; exit fail;
	end;
	let char_lst = List.map (fun str -> String.get str 0) slst in 
	CharSet.of_list char_lst

let create_blank json alphabet =
	let blank_str = obj_to_string ~name:"blank" (get_member json "blank") in
	let blank = String.get blank_str 0 in
	if (String.length blank_str <> 1) then begin
		printf "ft_turing : Error blank must be one char long\n"; exit fail
	end else if not (CharSet.mem blank alphabet) then begin
		printf "ft_turing : Error blank must be part of alphabet\n"; exit fail
	end;
	blank

let create_states json =
	let lst = obj_to_lst ~name:"states" (get_member json "states") in
	let slst = filter_string lst in 
	if (List.length lst = 0) then begin
		printf "ft_turing : Error states can't be empty\n"; exit fail
	end else if List.length lst <> List.length slst then begin
		printf "ft_turing : Error states members must be only composed by strings\n"; exit fail
	end	else if CL.contains_dup slst then begin
		printf "ft_turing : Error states can't have duplicates members\n"; exit fail;
	end;
	StringSet.of_list slst

let create_initial json states = 
	let initial = obj_to_string ~name:"initial" (get_member json "initial") in
	if not (StringSet.mem initial states) then begin
		printf "ft_turing : Error initial must be part of states\n"; exit fail
	end;
	initial

let create_finals json states = 
	let lst = obj_to_lst ~name:"finals" (get_member json "finals") in
	let slst = filter_string lst in
	let finals = StringSet.of_list slst in 
	if List.length lst = 0 then begin
		printf "ft_turing : Error finals can't be empty\n"; exit fail
	end else if List.length lst <> List.length slst then begin
		printf "ft_turing : Error finals members must be only composed by strings\n"; exit fail
	end else if not (StringSet.subset finals states) then begin
		printf "ft_turing : Error finals members must be a subset of states\n"; exit fail
	end	else if CL.contains_dup slst then begin
		printf "ft_turing : Error finals can't have duplicates members\n"; exit fail;
	end;
	finals

let verif_member ?(name = "json") assoc reflst =
	let name_lst = CL.sort String.compare (lstfst assoc) in 
	let rec members_are_contain reflst = match reflst with
	| head::tail when List.exists (fun str -> String.compare head str = 0) name_lst -> members_are_contain tail 
	| head::tail -> printf "ft_turing : Error %s member is missing from %s\n" head name; exit fail
	| [] -> ()
	in
	members_are_contain reflst;
	if CL.contains_dup name_lst then begin
		printf "ft_turing : Error %s can't have duplicates members\n" name; exit fail
	end else if not (CL.equal name_lst reflst (fun s1 s2 -> (String.compare s1 s2 = 0))) then begin
		printf "ft_turing : Error %s contain unknown member(s)\n" name; exit fail
	end

let verif_transition assoc states finals = 
	let slst = lstfst assoc in
	let transitions_set = StringSet.of_list slst in
	let compared_set = StringSet.diff states finals in
	if not (StringSet.equal compared_set transitions_set) then begin
			printf "ft_turing : Error transitions members must be states except finals\n"; exit fail
	end	else if CL.contains_dup slst then begin
		printf "ft_turing : Error transitions can't have duplicates members\n"; exit fail;
	end

let create_transitions json states finals =
	let jobj = get_member json "transitions" in
	let transitions_assoc = get_assoc jobj in
	verif_transition transitions_assoc states finals;
	transitions_assoc

let extract filename = 
	let json = get_json filename in
	let assoc = get_assoc json in
	verif_member assoc turing_members;
	let name = obj_to_string ~name:"initial" (get_member json "initial") in
	let alphabet = create_alphabet json in
	let states = create_states json in
	let finals = create_finals json states in
	{
		name = name;
		alphabet = alphabet;
		blank = create_blank json alphabet;
		states = states;
		initial = create_initial json states;
		finals = finals;
		transitions = create_transitions json states finals;
	} 

(* 
condition de parsing :
	- Fichier existant et bien formate (pas d'exception a l'ouverture) OK
	- Toutes les keys attendue sont presentes sont uniques et sont du type attendu OK
 	- chaque etats de states correspond a une transitions sauf les transitionss finales OK
 	- blank appartient a alphabet OK
 	- initial et finals appartiennent a states OK 
 	- transitionss :	- map 
 						- into array 
	- Pas de membre supplementaire OK
	- pas de doublons dans les lists OK
 
Transitions :
	- Les cles represente tous les etats excepte ceux finaux OK
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