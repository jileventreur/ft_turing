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
type to_state = To_state of string | Same | Next

type transition = Standart of (string * to_state * write * action) | Multiple of (string list * to_state * write * action)
type state = (string * (transition list))
type absfun = state list (* first state is an entry point for the next one *)
type labsfun = absfun list

let blank = "~"
let cursor = "_"
let right_char = "R"
let left_char = "L"
(* let state_range = ["A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K"] *)

let moove_chars = [left_char ; right_char]
let state_range = ["A"; "B"; "C"]
let control_chars = ["|"; "_"]
let sub_alphabet = ["1"; "."; "+"; "="]
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
let add_absfun (f1 : labsfun) (f2 : absfun) = 
	let deriv_name entry_name name = (string_of_int !join_cpt) ^ "_" ^ name in
	let deriv_states entry_name x = 
		let deriv_to_state ts = match ts with
			| To_state (name) -> To_state (deriv_name entry_name name)
			| (_ as other) -> other
		in
		match x with
		| Standart (rd, ts, wr, ac) -> Standart (rd, deriv_to_state ts, wr, ac) 
		| Multiple (rd, ts, wr, ac) -> Multiple (rd, deriv_to_state ts, wr, ac)
	in
	let deriv_fun name (str, trans) = (deriv_name name str, List.map trans (deriv_states name)) in 
	incr join_cpt; List.append f1 [(List.map f2 (deriv_fun (get_entry f2)))]

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

let (replic_tape : absfun) = 
	let go_end = ("go_end", [Multiple (state_range, Same, Copy, Right); Standart ("#", To_state "copy#", Write ".", Left)]) in
	let generate_copy str = 
		let generate_tran = Standart (".", Next, Write str, Left) :: (List.map state_range (fun x -> Standart (x, To_state ("copy" ^ x), Write str, Left))) in
		("copy" ^ str, generate_tran)
	in
	let copy_list = List.map state_range generate_copy in
	go_end :: copy_list

let nmoove len action = 
	let create_cur n = 
		let name n = (string_of_int (len - n)) ^ "moove_" ^ (apply_action action) in
		(name n,
		[Multiple (alphabet, (if n <> (len - 1) then To_state (name (n + 1)) else Next), Copy, action)])
	in
	List.init len create_cur

(* type state = (string * (transition list))
type absfun = state list (* first state is an entry point for the next one *)
 *)

let action_letter action = match action with
	| Left -> "L" 
	| Right -> "R" 

let range_without_c range c = List.filter range (fun x -> ((String.compare x c) <> 0)) 

let blank_until_char ?(include_char = false) c action = 
	let final_write = if include_char then Write blank else Copy in
	[("blank_until_" ^ c, 
	[Standart (c, Next, final_write, action);
	Multiple (range_without_c alphabet c, Same, Write blank, action)])]

let find_nchar len c find_dir next_dir = 
	let create_cur n = 
		let name n =  (action_letter find_dir) ^ "find" ^ (string_of_int (len - n) ^ c) in
		let condition_to_state = if n <> (len - 1) then To_state (name (n + 1)) else Next in
		(name n,
		[Standart (c, condition_to_state, Copy, next_dir);
		Multiple (range_without_c alphabet c, Same, Copy, find_dir)])
	in
	List.init len create_cur

let store (clist : string list) (f : absfun) (use_c : string -> absfun) action =
	let new_name_format c oldname = "st_" ^ c ^ "_" ^ oldname in
	let deriv_transition c trans = 
		let deriv_to_state ts = match ts with
			| To_state (name) -> To_state (new_name_format c name)
			| (_ as other) -> other
		in
		match trans with
		| Standart (rd, ts, wr, act) -> Standart (rd, deriv_to_state ts, wr, act)
		| Multiple (rdlst, ts, wr, act) -> Multiple (rdlst, deriv_to_state ts, wr, act)
	in
	let fderiv c = List.map f (fun (name, trans) -> (new_name_format c name, List.map trans (deriv_transition c))) in
	let (flst, switch) =
		let generate_branche c = 
			let branche = join_absfun ([fderiv c ; use_c c]) in
			(branche, Standart (c, To_state (get_entry branche), Copy, action)) 
		in
		List.unzip @@ List.map clist generate_branche
	in
	("st_switch", switch) :: (List.concat flst)

let write_c action c = [("write_" ^ c, [Standart (blank, Next, Write c, action)])]


let restruct_machine =
	let create_reg = 
		let (actions : labsfun) = [
			(blank_until_char ~include_char:true "|" Right);
			(find_nchar 1 "|" Right Right);
			(blank_until_char "|" Right);
			find_nchar 2 "|" Left Left;
			(store sub_alphabet (join_absfun [find_nchar 2 "|" Right Right ; nmoove 2 Left]) (write_c Left) Left);
			(blank_until_char ~include_char:true "|" Left);
			(blank_until_char blank Right);
			find_nchar 1 "|" Right Left;
			nmoove 1 Left;
			write_c Right "|";
			nmoove 2 Right;
			(store state_range (join_absfun [nmoove 4 Left]) (write_c Right) Left)
		] in
		List.fold actions ~init:[] ~f:(@<)
	in
	join_absfun create_reg
(* 
let test_state c = 
	[("test_state_" ^ c, 
		[Standart (c, Next, Copy, Left);
		Standart ("|", To_state "Sub_undefined", Copy, Left);
		Multiple (range_without_c alphabet c, To_state )]
	)]
 *)
let exec_machine =
	let read_to_trans = 
		(store alphabet (join_absfun [find_nchar 5 "|" Left Left]) (nmoove 1 Right) Left)
	in
	let (read_cur : labsfun) = [
		find_nchar 1 cursor Right Right;
		(store alphabet (join_absfun [find_nchar 5 "|" Left Left]) (write_c Left) Left)
	] in
	join_absfun @@ List.fold read_cur ~init:[] ~f:(@<)


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
 	let program = join_absfun [restruct_machine; exec_machine] in
 	let jtransition = (apply_absfun program "HALT") in
 	let finals = [`String "HALT"; `String "Sub_undefined"] in
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