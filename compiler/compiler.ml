open Yojson.Basic.Util
module CL = Core.Core_list

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

let blank = "~"
(* let state_range = ["A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K"] *)
let state_range = ["A"; "B"]
let control_chars = ["|"; "_"]
let add_alphabet = ["1"; "."; "+"; "="]
let alphabet_noblank = state_range @ add_alphabet @ control_chars
let alphabet = alphabet_noblank @ [blank]

let jalphabet = List.map (fun x -> `String x) alphabet


let apply_name name =
	states := (`String name) :: !states; name 

let apply_to_state ts same next = match ts with
	| To_state name -> name ^ "_to_" ^ next
	| Same -> same
	| Next -> next

let apply_write wr rd = match wr with 
	| Write c -> c
	| Copy -> rd

let apply_action act = match act with
	| Right -> "RIGHT"
	| Left -> "LEFT"

let rev_action act = match act with
	| Right -> Left
	| Left -> Right

let apply_absfun (f : absfun) (next : string ) = 
	let apply_state (name, lst) =
		let state_name =  name ^ "_to_" ^ next in
		let apply_standart ts wr act rd =
		`Assoc [
			("read", `String rd);
			("to_state", `String (apply_to_state ts state_name next));
			("write", `String (apply_write wr rd));
			("action", `String (apply_action act));]
		in
		let apply_trans trans = match trans with 
			| Standart (rd, ts, wr, act) -> [apply_standart ts wr act rd]
			| Multiple (rdlst, ts, wr, act) -> List.map (apply_standart ts wr act) rdlst
		in 
		(apply_name state_name, `List (List.concat @@ List.map apply_trans lst))
	in
	`Assoc (List.map apply_state f)

let get_init jobj = let (init, _) = List.hd (to_assoc jobj) in init

let join_absfun (f1 : absfun) (f2 : absfun) = 
	let convert_next entry_name x = match x with
		| Standart (rd, ts, wr, ac) when ts = Next -> Standart (rd, To_state entry_name, wr, ac) 
		| Multiple (rd, ts, wr, ac) when ts = Next -> Multiple (rd, To_state entry_name, wr, ac)
		| (_ as other) -> other
	in
	let convert_func name (str, trans) = (str, List.map (convert_next name) trans) in 
	match f2 with
	| (name, _)::_ -> CL.append (List.map (convert_func name) f1) f2
	| _ -> failwith "join_absfun: Error: f2 is empty\n"

let ( >@ ) f1 f2 = join_absfun f1 f2

let (replic_tape : absfun) = 
	let go_end = ("go_end", [Multiple (state_range, Same, Copy, Right); Standart ("#", To_state "copy#", Write ".", Left)]) in
	let generate_copy str = 
		let generate_tran = Standart (".", Next, Write str, Left) :: (List.map (fun x -> Standart (x, To_state ("copy" ^ x), Write str, Left)) state_range) in
		("copy" ^ str, generate_tran)
	in
	let copy_list = List.map generate_copy state_range in
	go_end :: copy_list

let nmoove len action = 
	let create_cur n = 
		let name n = (string_of_int (len - n)) ^ "moove_" ^ (apply_action action) in
		(name n,
		[Multiple (alphabet, (if n <> (len - 1) then To_state (name (n + 1)) else Next), Copy, action)])
	in
	CL.init len create_cur

(* type state = (string * (transition list))
type absfun = state list (* first state is an entry point for the next one *)
 *)

let range_without_c range c = CL.filter range (fun x -> ((String.compare x c) <> 0)) 

let blank_until_char ?(include_char = false) c action = 
	let final_write = if include_char then Write blank else Copy in
	[("blank_until_" ^ c, 
	[Standart (c, Next, final_write, action);
	Multiple (range_without_c alphabet c, Same, Write blank, action)])]

let moove_to_nchar len c action = 
	let create_cur n = 
		let name n =  (apply_action action) ^ "_find_" ^ (string_of_int (len - n)) ^ c in
		let condition_to_state = if n <> (len - 1) then To_state (name (n + 1)) else Next in
		(name n,
		[Standart (c, condition_to_state, Copy, action);
		Multiple (range_without_c alphabet c, Same, Copy, action)])
	in
	CL.init len create_cur



let restruct_machine =
	let blank_til_pipe = blank_until_char ~include_char:true "|" Right in
	let create_reg = 
		moove_to_nchar 2 "|" Right >@ 
		Multiple (state_range, )
	in
	blank_til_pipe >@ create_reg

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
 	let jtransition = (apply_absfun restruct_machine "HALT") in
 	let json = `Assoc [
 	("name", `String ("turing_complete"));
 	("alphabet", `List jalphabet);
 	("blank", `String blank);
 	("states", `List ((`String "HALT") :: !states));
 	("initial", `String (get_init jtransition));
 	("finals", `List [`String "HALT"]);
 	("transitions", jtransition);
 	] in
 	Yojson.Basic.to_file "complete_turing.json" json