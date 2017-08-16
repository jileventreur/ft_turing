include Table

open Json_utils
open Yojson.Basic.Util
open Printf
open Utils

(* Temporary parsing structure *)

module CharSet = Json_utils.CharSet

(* type action = Left | Right
type state_index = int
type transition = Defined of char * action * state_index | Undefined
type state = Normal of (string * transition array) | Final of string

type data = {
	name : string;
	alphabet : CharSet.t;
	blank : char;
	table : state array;
	state_register : state_index;
} *)

let turing_members = ["name"; "alphabet"; "blank"; "states"; "initial"; "finals"; "transitions"]

let create_alphabet json =
	let lst = obj_to_lst ~name:"alphabet" (get_member json "alphabet") in
	let slst = filter_string lst in
	if (List.length lst = 0) then begin
		printf "ft_turing : Error alphabet can't be empty\n"; exit fail
	end else if List.length lst <> List.length slst then begin
		printf "ft_turing : Error alphabet members must be only composed by strings\n"; exit fail
	end else if ((List.for_all is_char slst) = false) then begin
		printf "ft_turing : Error alphabet members must be one char long\n"; exit fail
	end else if CL.contains_dup slst then begin
		printf "ft_turing : Error alphabet can't have duplicates members\n"; exit fail;
	end;
	let char_lst = List.map (fun str -> String.get str 0) slst in
	CharSet.of_list char_lst

let create_blank json alphabet =
	let blank_str = obj_to_string ~name:"blank" (get_member json "blank") in
	let blank = String.get blank_str 0 in
	if not (is_char blank_str) then begin
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
	slst
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
	let transitions_assoc = obj_to_assoc ~name:"transitions" jobj in
	verif_transition transitions_assoc states finals;
	transitions_assoc

let extract filename =
	let json = get_json filename in
	let assoc = obj_to_assoc json in
	verif_member assoc turing_members;
	let name = obj_to_string ~name:"name" (get_member json "name") in
	let alphabet = create_alphabet json in
	let stateslst = create_states json in
	let states = StringSet.of_list stateslst in
	let finals = create_finals json states in
	let data = {
		name = name;
		alphabet = alphabet;
		blank = create_blank json alphabet;
		stateslst = stateslst;
		states = states;
		initial = create_initial json states;
		finals = finals;
		transitions = create_transitions json states finals;
	} in
	Table.create_table data

(*
condition de parsing :
	- Fichier existant et bien formate (pas d'exception a l'ouverture) OK
	- Toutes les keys attendue sont presentes sont uniques et sont du type attendu OK
 	- chaque etats de states correspond a une transitions sauf les transitions finales OK
 	- blank appartient a alphabet OK
 	- initial et finals appartiennent a states OK
 	- transitionss :	- map
 						- into array
	- Pas de membre supplementaire OK
	- pas de doublons dans les lists OK


 preparer la structure de stocakge des donnes parsees :
 	- name : string
 	- alphabet : set ?
 	- blank : char
 	- states : map key string ; val transitions
 	- transitions : : map key : char ; val (string, char, tape -> unit)
 *)
