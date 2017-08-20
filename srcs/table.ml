open Json_utils
open Yojson.Basic.Util
open Printf
open Utils

(* type action = Left | Right
type state_index = int
type transition = Defined of char * action * state_index | Undefined
type state = Normal of (string * transition array) | Final of string *)

(* Structure returned by json. More or less turing machine without tape *)

type action = Left | Right
type state_index = int
type transition = Defined of char * action * state_index | Undefined
type state = Normal of (string * transition array) | Final of string

type data = {
	name : string;
	alphabet : CharSet.t;
	blank : char;
	table : state array;
	state_register : state_index;
}

let char_range = 256
let transition_members = ["read"; "to_state"; "write"; "action"]

let _get_action action = if equal action "LEFT" then Left else Right

let _get_index lst elem =
	let rec loop lst i = match lst with
	| head::tail when equal head elem -> i
	| head::tail -> loop tail (i + 1)
	| [] -> raise Not_found
	in loop lst 0

let _create_transition tab (parsing : parsing) json =
	let assoc = obj_to_assoc json in
	let read = obj_to_string @@ member "read" json in
	let to_state = obj_to_string @@ member "to_state" json in
	let write = obj_to_string @@ member "write" json in
	let action = obj_to_string @@ member "action" json in
	let index = int_of_char @@ String.get read 0 in
	let verif_transition_members =
		if not (is_char read) then begin
			printf "ft_turing : Error read value must be a char\n" ; exit fail
		end else if not (Json_utils.CharSet.mem (String.get read 0) parsing.alphabet) then begin
			printf "ft_turing : Error read must be part of alphabet\n"; exit fail
		end else if (Array.get tab index <> Undefined) then begin
			printf "ft_turing : Error one char readed must be associate with only one transition [%s]\n" to_state; exit fail
		end else if not (is_char write) then begin
			printf "ft_turing : Error write value must be a char\n" ; exit fail
		end else if not (Json_utils.CharSet.mem (String.get write 0) parsing.alphabet) then begin
			printf "ft_turing : Error write must be part of alphabet\n"; exit fail
		end else if not (StringSet.mem to_state parsing.states) then begin
			printf "ft_turing : Error to_state [%s] must be part of states\n" to_state ; exit fail
		end else if (not (equal action "LEFT")) && (not (equal action "RIGHT")) then begin
			printf "ft_turing : Error action must be part LEFT or RIGHT\n"; exit fail
		end
	in
	verif_member assoc transition_members;
	verif_transition_members;
	Array.set tab index (Defined (String.get write 0, _get_action action, _get_index parsing.stateslst to_state))

let _create_normal_state (name, json) parsing =
	let transitiontab = Array.make char_range Undefined in
	let jlst = obj_to_lst ~name:name json in
	List.iter (_create_transition transitiontab parsing) jlst;
	Normal (name, transitiontab)

let create_table (parsing : parsing) =
	let statetab = Array.make (List.length parsing.stateslst) (Final("__EMPTY__")) in
	let create_state index name =
		if StringSet.mem name parsing.finals then
			Array.set statetab index (Final(name))
		else
			Array.set statetab index (_create_normal_state (List.find (fun (str, _ ) -> equal name str) parsing.transitions) parsing)
	in
	List.iteri create_state parsing.stateslst;
	{
		name = parsing.name;
		alphabet = parsing.alphabet;
		blank = parsing.blank;
		table = statetab;
		state_register = _get_index parsing.stateslst parsing.initial
	}
