open Yojson.Basic.Util

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

(* let state_range = ["A"; "B"; "#"] *)
let state_range = ["A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K"; "#"]
let add_range = ["1"; "."; "+"; "="]

let alphabet = List.map (fun x -> `String x) (state_range @ add_range)

type absfun = state list

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

let (replic_tape : absfun) = 
	let go_end = ("go_end", [Multiple (state_range, Same, Copy, Right); Standart ("#", To_state "copy#", Write ".", Left)]) in
	let generate_copy str = 
		let generate_tran = Standart (".", Next, Write str, Left) :: (List.map (fun x -> Standart (x, To_state ("copy" ^ x), Write str, Left)) state_range) in
		("copy" ^ str, generate_tran)
	in
	let copy_list = List.map generate_copy state_range in
	go_end :: copy_list

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
 	let json = `Assoc [
 	("name", `String ("turing_complete"));
 	("alphabet", `List alphabet);
 	("blank", `String ".");
 	("states", `List ((`String "HALT") :: !states));
 	("initial", `String "go_end_to_HALT");
 	("finals", `List [`String "HALT"]);
 	("transitions", (apply_absfun replic_tape "HALT"));
 	] in
 	Yojson.Basic.to_file "replic_state.json" json