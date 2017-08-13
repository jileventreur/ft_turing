open Yojson.Basic.Util
open Printf
open Utils

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

type parsing = {
	name : string;
	alphabet : CharSet.t;
	blank : char;
	stateslst : string list;
	states : StringSet.t;
	initial : string;
	finals : StringSet.t;
	transitions : (string * Yojson.Basic.json) list;
}

(* module TransitionsMap = Map.Make(
struct
	type t = string
	let compare = Pervasives.compare
end)
 *)

let obj_to_assoc ?(name = "member") jobj = 
	try to_assoc jobj with
	| _ -> printf "ft_turing : %s must be a list\n" name ; exit fail

let obj_to_lst ?(name = "member") jobj =
	try to_list jobj with
	| _ -> printf "ft_turing : %s must be a list\n" name ; exit fail

let obj_to_string ?(name = "member") jobj =
	try to_string jobj with
	| _ -> printf "ft_turing : %s must be a string\n" name ; exit fail

let get_json filename = 
	try Yojson.Basic.from_file filename with
	| Yojson.Json_error str -> printf "ft_turing : %s : %s\n" filename str ; exit fail
	| Sys_error str -> printf "ft_turing : %s\n" str ; exit fail
	| _ -> printf "ft_turing : %s : Error opening json file\n" filename ; exit fail

let get_member json name = 
	let jobj = member name json in
	match jobj with
	| `Null -> printf "ft_turing : Error missing member %s\n" name ; exit fail
	| _ -> jobj

let verif_member ?(name = "json") assoc reflst =
	let name_lst = lstfst assoc in 
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
