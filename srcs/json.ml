include Create_table

open Json_utils
open Yojson.Basic.Util
open Printf
open Utils

module CharSet = Json_utils.CharSet

let turing_members = ["name"; "alphabet"; "blank"; "states"; "initial"; "finals"; "transitions"]

let _create_alphabet json =
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

let _create_blank json alphabet =
	let blank_str = obj_to_string ~name:"blank" (get_member json "blank") in
	let blank = String.get blank_str 0 in
	if not (is_char blank_str) then begin
		printf "ft_turing : Error blank must be one char long\n"; exit fail
	end else if not (CharSet.mem blank alphabet) then begin
		printf "ft_turing : Error blank must be part of alphabet\n"; exit fail
	end;
	blank

let _create_states json =
	let lst = obj_to_lst ~name:"states" (get_member json "states") in
	let slst = filter_string lst in 
	if (List.length lst = 0) then begin
		printf "ft_turing : Error states can't be empty\n"; exit fail
	end else if List.length lst <> List.length slst then begin
		printf "ft_turing : Error states members must be only composed by strings\n"; exit fail
	end;
	begin match CL.find_a_dup slst with 
		| None -> ()
		| Some x -> printf "ft_turing : Error states can't have duplicates members (%s)\n" x; exit fail
	end;
	slst

let _create_initial json states = 
	let initial = obj_to_string ~name:"initial" (get_member json "initial") in
	if not (StringSet.mem initial states) then begin
		printf "ft_turing : Error initial must be part of states\n"; exit fail
	end;
	initial

let _create_finals json states = 
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

let _verif_transition assoc states finals = 
	let slst = lstfst assoc in
	let transitions_set = StringSet.of_list slst in
	let compared_set = StringSet.diff states finals in
	if not (StringSet.equal compared_set transitions_set) then begin
			printf "ft_turing : Error transitions members must be states except finals\n"; exit fail
	end	else if CL.contains_dup slst then begin
		printf "ft_turing : Error transitions can't have duplicates members\n"; exit fail;
	end

let _create_transitions json states finals =
	let jobj = get_member json "transitions" in
	let transitions_assoc = obj_to_assoc ~name:"transitions" jobj in
	_verif_transition transitions_assoc states finals;
	transitions_assoc

let extract filename = 
	let json = get_json filename in
	let assoc = obj_to_assoc json in
	verif_member assoc turing_members;
	let name = obj_to_string ~name:"name" (get_member json "name") in
	let alphabet = _create_alphabet json in
	let stateslst = _create_states json in
	let states = StringSet.of_list stateslst in	
	let finals = _create_finals json states in
	let data = {
		name = name;
		alphabet = alphabet;
		blank = _create_blank json alphabet;
		stateslst = stateslst;
		states = states;
		initial = _create_initial json states;
		finals = finals;
		transitions = _create_transitions json states finals;
	} in
	Create_table.create_table data