open Printf
open Yojson.Basic.Util
module List = Core.Core_list

type alphabet = string list
type action = Right | Left

(* type transition = {
	read = string ;
	to_state = string;
	write = string;
	action = action;
}
 *)

let (states : Yojson.Basic.json list ref) = ref []
type write = Write of string | Copy
type to_state = To_state of string | Same | Next | Loop

type transition = Standart of (string * to_state * write * action) | Multiple of (string list * to_state * write * action)
type state = (string * (transition list))
type absfun = state list (* first state is an entry point for the next one *)
type labsfun = absfun list
(* type if_type = State of to_state | Function of absfun *)

let blank = "~"
let pipe = "|"
let cursor = "_"
let right_char = "R"
let left_char = "L"

let state_range = (Yojson.Basic.from_file Sys.argv.(1))
	|> member "states"
	|> to_list
	|> filter_string
	|> fun lst -> List.mapi lst (fun i e -> String.make 1 (Char.chr (i + 65)) )
(* let add_alphabet = ["1"; "."; "+" ; "="] *)
let sub_alphabet = (Yojson.Basic.from_file Sys.argv.(1)) |> member "alphabet" |> to_list |> filter_string

(* let state_range = ["A"; "B"; "C"] *)
(* let sub_alphabet = ["0"; "y"; "n"; "."] *)

let moove_chars = [left_char ; right_char]
let control_chars = [cursor; pipe]
let alphabet_noblank = state_range @ sub_alphabet @ control_chars @ moove_chars
let alphabet = alphabet_noblank @ [blank]
let jalphabet = List.map alphabet (fun x -> `String x)

let get_entry f = match f with
		| (name, _)::_ -> name
		| _ -> failwith "get_entry: Error: f is empty\n"

let apply_name name =
	states := (`String name) :: !states; name

let apply_to_state ts same final = match ts with
	| To_state name -> name
	| Same -> same
	| Next -> final
	| Loop -> failwith "Error : Loop type still exists\n"

let apply_write wr rd = match wr with
	| Write c -> c
	| Copy -> rd

let apply_action act = match act with
	| Right -> "RIGHT"
	| Left -> "LEFT"

let rev_action act = match act with
	| Right -> Left
	| Left -> Right

let apply_absfun (f : absfun) final =
	let apply_state (name, lst) =
		let state_name =  name in
		let apply_standart ts wr act rd =
		`Assoc [
			("read", `String rd);
			("to_state", `String (apply_to_state ts state_name final));
			("write", `String (apply_write wr rd));
			("action", `String (apply_action act));]
		in
		let apply_trans trans = match trans with
			| Standart (rd, ts, wr, act) -> [apply_standart ts wr act rd]
			| Multiple (rdlst, ts, wr, act) -> List.map rdlst (apply_standart ts wr act)
		in
		(apply_name state_name, `List (List.concat @@ List.map lst apply_trans))
	in
	`Assoc (List.map f apply_state)

let get_init jobj = let (init, _) = List.hd_exn (to_assoc jobj) in init

let join_cpt = ref 0

let deriv_state (str, trans) =
	let deriv_name name = (string_of_int !join_cpt) ^ "_" ^ name in
	let deriv_transition x =
		let deriv_to_state ts = match ts with
			| To_state (name) when String.compare name "HALT" = 0 || String.compare name "Sub_undefined" = 0 -> To_state name
			| To_state (name) -> To_state (deriv_name name)
			| (_ as other) -> other
		in
		match x with
		| Standart (rd, ts, wr, ac) -> Standart (rd, deriv_to_state ts, wr, ac)
		| Multiple (rd, ts, wr, ac) -> Multiple (rd, deriv_to_state ts, wr, ac)
	in
	deriv_name str, List.map trans deriv_transition

let deriv_fun f = List.map f deriv_state

let add_absfun (f1 : labsfun) (f2 : absfun) =
	incr join_cpt; List.append f1 [(List.map f2 deriv_state)]

let join_absfun (f : labsfun) =
	let join_states next x = match x with
		| Standart (rd, ts, wr, ac) when ts = Next -> Standart (rd, To_state next, wr, ac)
		| Multiple (rd, ts, wr, ac) when ts = Next -> Multiple (rd, To_state next, wr, ac)
		| (_ as other) -> other
	in
	let join_funs next (str, trans) = (str, List.map trans (join_states next)) in
	let rec loop f = match f with
		| head::(nfun::_ as tail) -> (List.map head (join_funs (get_entry nfun))) :: (loop (tail))
		| (_ as other) -> other
	in List.concat @@ loop f

let ( @< ) f1 f2 = add_absfun f1 f2

let nmoove ?(loop = false) ?(name = "moove_") len action =
	let create_cur n =
		let name n = (string_of_int (len - n)) ^ name ^ (apply_action action) in
		let ts =
			if n <> (len - 1) then
				To_state (name (n + 1))
			else if loop = true then
				Loop
			else
				Next
		in
		(name n,
		[Multiple (alphabet, ts, Copy, action)])
	in
	let (ret : absfun) = List.init len create_cur in
	ret

let action_letter action = match action with
	| Left -> "L"
	| Right -> "R"


let if_func ?(name = "if_state") condlst =
	let create_states (c, ts, action) = Standart (c, ts, Copy, action) in
	[(name, List.map condlst create_states)]

let create_loop lst =
	let entry = get_entry lst in
	let to_state_looping ts = match ts with
			| Loop -> To_state (entry)
			| (_ as other) -> other
	in
	let transition_looping transition = match transition with
		| Standart (rd, ts, wr, act) -> Standart (rd, to_state_looping ts, wr, act)
		| Multiple (rdlst, ts, wr, act) -> Multiple (rdlst, to_state_looping ts, wr, act)
	in
	let state_map (name, transition) = (name, List.map transition transition_looping) in
	List.map lst state_map

let store (clist : string list) (f : absfun) (use_c : string -> absfun) action =
	let new_name_format c oldname = "st_" ^ c ^ "_" ^ oldname in
	let new_name_extract_format c oldname = "extract_" ^ c ^ "_" ^ oldname in
	let deriv_transition c deriv_format trans =
		let deriv_to_state ts = match ts with
			| To_state (name) when String.compare name "HALT" = 0 || String.compare name "Sub_undefined" = 0 -> To_state name
			| To_state (name) -> To_state (deriv_format c name)
			| (_ as other) -> other
		in
		match trans with
		| Standart (rd, ts, wr, act) -> Standart (rd, deriv_to_state ts, wr, act)
		| Multiple (rdlst, ts, wr, act) -> Multiple (rdlst, deriv_to_state ts, wr, act)
	in
	let fderiv f c deriv_format = List.map f (fun (name, trans) -> (deriv_format c name, List.map trans (deriv_transition c deriv_format))) in
	let (flst, switch) =
		let generate_branche c =
			let deriv = fderiv f c new_name_format in
			let use = fderiv (use_c c) c new_name_extract_format in
			((deriv, use), Standart (c, To_state (get_entry deriv), Copy, action))
		in
		List.unzip @@ List.map clist generate_branche
	in
	(* ("st_switch", switch) :: (List.concat flst) *)
	(("st_switch", switch), flst)

let store_fun (switch, flst) =
	(switch) :: (List.concat @@ List.map flst (fun (deriv, use) -> join_absfun [deriv; use]))

let loop_after_switch (switch, flst) =
	let loop_branches = List.map flst (fun (deriv, use) -> create_loop (join_absfun [deriv; use])) in
	(switch) :: (List.concat loop_branches)

let loop_before_use (switch, flst) =
	let loop_branches = List.map flst (fun (deriv, use) -> join_absfun [deriv; create_loop use]) in
	(switch) :: (List.concat loop_branches)

let write_c action c = [("write_" ^ c, [Multiple (alphabet, Next, Write c, action)])]

let range_without_c range c = List.filter range (fun x -> ((String.compare x c) <> 0))

let blank_until_char ?(include_char = false) c action =
	let final_write = if include_char then Write blank else Copy in
	[("blank_until_" ^ c,
	[Standart (c, Next, final_write, action);
	Multiple (range_without_c alphabet c, Same, Write blank, action)])]

let find_nchar ?(loop = false) len c find_dir next_dir =
	let create_cur n =
		let name n =  (action_letter find_dir) ^ "find" ^ (string_of_int (len - n) ^ c) in
		let condition_to_state =
			if n <> (len - 1) then
				To_state (name (n + 1))
			else if loop = false then
				Next
			else
				Loop
		in
		(name n,
		[Standart (c, condition_to_state, Copy, next_dir);
		Multiple (range_without_c alphabet c, Same, Copy, find_dir)])
	in
	List.init len create_cur

let restruct_machine =
	let create_reg =
		let (actions : labsfun) = [
			(blank_until_char ~include_char:true pipe Right);
			(find_nchar 1 pipe Right Right);
			(blank_until_char pipe Right);
			find_nchar 2 pipe Left Left;
			store_fun (store sub_alphabet (join_absfun [find_nchar 2 pipe Right Right ; nmoove 2 Left]) (write_c Left) Left);
			(blank_until_char ~include_char:true pipe Left);
			(blank_until_char blank Right);
			find_nchar 1 pipe Right Left;
			nmoove 1 Left;
			write_c Right pipe;
			nmoove 2 Right;
			store_fun (store state_range (join_absfun [nmoove 4 Left]) (write_c Right) Left)
		] in
		List.fold actions ~init:[] ~f:(@<)
	in
	join_absfun create_reg

(* type state = (string * (transition list))
type absfun = state list (* first state is an entry point for the next one *)
 *)

let test_finals c =
	let else_alpha = range_without_c state_range c in
	let is_final_cond =
		let elselst = List.map else_alpha (fun x -> (x, Loop, Right)) in
		(c, To_state "HALT", Right)::(pipe, Next, Right)::elselst
	in
	create_loop (if_func ~name:("is_final_" ^ c) is_final_cond)

let test_state_transition c =
	let else_alpha = range_without_c state_range c in
	let is_state_transition =
		let elselst = List.map else_alpha (fun x -> (x, To_state (get_entry (nmoove ~name:(c ^ "_state_check_moove") 4 Right)), Right)) in
		(c, Next, Right)::(pipe, To_state "Sub_undefined", Right)::elselst
	in
	create_loop ((if_func ~name:(c ^ "_is_state_transition") is_state_transition)
	@ (nmoove ~loop:true ~name:(c ^ "_state_check_moove") 4 Right))

let test_read_transition c =
	let else_alpha = range_without_c sub_alphabet c in
	let is_read_transition =
		let elselst = List.map else_alpha (fun x -> (x, To_state (get_entry (nmoove ~name:(c ^ "_transition_check_moove_") 3 Right)), Right)) in
		(c, Next, Right)::elselst
	in
	(if_func ~name:(c ^ "_is_read_transition") is_read_transition) @ nmoove ~loop:true ~name:(c ^ "_transition_check_moove_") 3 Right

let find_transition =
	let new_name_format c1 c2 oldname = "st_" ^ c1 ^ "_" ^ c2 ^ "_" ^ oldname in
	let deriv_transition c1 c2 trans =
		let deriv_to_state ts = match ts with
			| To_state (name) when String.compare name "HALT" = 0 || String.compare name "Sub_undefined" = 0 -> To_state name
			| To_state (name) -> To_state (new_name_format c1 c2 name)
			| (_ as other) -> other
		in
		match trans with
		| Standart (rd, ts, wr, act) -> Standart (rd, deriv_to_state ts, wr, act)
		| Multiple (rdlst, ts, wr, act) -> Multiple (rdlst, deriv_to_state ts, wr, act)
	in
	let fderiv c1 c2 f = List.map f (fun (name, trans) -> (new_name_format c1 c2 name, List.map trans (deriv_transition c1 c2))) in
	let store_state rd st =
		let go_to_transition = find_nchar 4 pipe Right Right in
		let find_trans = create_loop @@ join_absfun [test_state_transition st; test_read_transition rd] in
		let final_fun = fderiv rd st (join_absfun [go_to_transition; find_trans]) in
		(Standart (st, To_state (get_entry final_fun), Copy, Right), final_fun)
	in
	let store_read rd =
		let store_state_fun =
			let (switch, flst) = List.unzip @@ List.map state_range (store_state rd) in
			("st" ^ rd ^ "st_switch", switch) :: List.concat flst
		in
		(Standart (rd, To_state (get_entry store_state_fun), Copy, Left), store_state_fun)
	in
	let (switch, flst) = List.unzip @@ List.map sub_alphabet store_read in
	("st_switch", switch) :: List.concat flst
(* (fun x -> if String.comparex ) @ [(c, Next), pipe, To_state ("Sub_undefined")] *)

let moove_fun c =
	let replic_tape end_char dir =
		(* let go_end = ("go_end", [Multiple (alphabet, Same, Copy, Right); Standart (blank, To_state "copy#", Write ".", Left)]) in *)
		let generate_copy str =
			let generate_tran =
				Standart (end_char, Next, Write str, dir) ::
				(List.map (range_without_c alphabet end_char) (fun x -> Standart (x, To_state ("copy" ^ x), Write str, dir)))
			in
			"copy" ^ str, generate_tran
		in
		let entry =
				("replic_entry", List.map (range_without_c alphabet end_char) (fun x -> Standart (x, To_state ("copy" ^ x), Copy, dir)))
		in
		entry :: (List.map (range_without_c alphabet end_char) generate_copy)
	in
	let right_fun =
		let if_right_end = join_absfun
			[find_nchar 4 pipe Left Left;
			store_fun (store sub_alphabet (find_nchar 1 cursor Right Right) (write_c Left) Left)
			]
		in
		let is_right_end =
			let elselst = List.map sub_alphabet (fun x -> (x, Next, Left)) in
			let condlst = (blank, To_state (get_entry if_right_end), Left)::elselst in
			(if_func ~name:(c ^ "is_right_end") condlst) @ if_right_end
		in
		let flst = [
			nmoove 1 Right;
			replic_tape cursor Left;
			nmoove 2 Right;
			write_c Right cursor;
			is_right_end;
		] in
		join_absfun @@ List.fold flst ~init:[] ~f:(@<)
	in
	let left_fun =
		let if_left_end = join_absfun
			[
			nmoove 1 Right;
			replic_tape blank Left;
			find_nchar 1 pipe Right Right;
			store_fun (store sub_alphabet (find_nchar 5 pipe Right Right) (write_c Right) Left)
			]
		in
		let is_left_end =
			let elselst = List.map sub_alphabet (fun x -> (x, Next, Right)) in
			let condlst = (pipe, To_state (get_entry if_left_end), Left)::elselst in
			(if_func ~name:(c ^ "is_left_end") condlst) @ if_left_end
		in
		let flst = [
			nmoove 1 Left;
			is_left_end;
			nmoove 1 Left;
			replic_tape cursor Right;
			nmoove 2 Left;
			write_c Right cursor;
		] in
		join_absfun @@ List.fold flst ~init:[] ~f:(@<)
	in
 	if (c = "R") then
		right_fun
	else
		left_fun

let exec_transition =
	let write_fun c =
		join_absfun [find_nchar 1 cursor Right Right ;(write_c Left c)]
	in
	let store_next = store_fun (store state_range (join_absfun [find_nchar 4 pipe Left Left; nmoove 1 Left]) (write_c Right) Left) in
	let store_write = store_fun (store sub_alphabet store_next write_fun Left) in
	let store_moove = store_fun (store moove_chars store_write moove_fun Left) in
	store_moove

let exec_machine =
	let read_cur = store_fun (store sub_alphabet (find_nchar 5 pipe Left Left) (write_c Left) Left) in
	let is_a_final_state = store_fun (store state_range (nmoove 6 Right) test_finals Right) in
	let (read_cur : labsfun) = [
			nmoove 1 Right;
			find_nchar 1 cursor Right Right;
			read_cur;
			is_a_final_state;
			find_nchar 4 pipe Left Left;
			find_transition;
			nmoove 2 Right;
			exec_transition;
			find_nchar ~loop:true 1 blank Left Right;
		] in
		create_loop @@ join_absfun @@ List.fold read_cur ~init:[] ~f:(@<)


(* type json = [
  | `Assoc of (string * json) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of json list
  | `Null
  | `String of string
] *)

 let () =
 	(* let program =  exec_machine in *)
	match Array.length Sys.argv with
	| 3 -> begin
		let program = join_absfun [restruct_machine; exec_machine] in
		let jtransition = (apply_absfun program "END") in
		let finals = [`String "END"; `String "HALT"; `String "Sub_undefined"] in
		let json = `Assoc [
		("name", `String ("turing_complete"));
		("alphabet", `List jalphabet);
		("blank", `String blank);
		("states", `List (finals @ !states));
		("initial", `String (get_init jtransition));
		("finals", `List finals);
		("transitions", jtransition);
		] in
		Yojson.Basic.to_file "complete_turing.json" json
	end
	| _ -> print_endline ("usage: " ^ Sys.argv.(0) ^ " jsonfile input")
