(* module CharSet : Set.S with type elt = char *)

type action = Left | Right
type state_index = int
type transition = Defined of char * action * state_index | Undefined
type state = Normal of (string * transition array) | Final of string
(* 
type data = {
	name : string;
	alphabet : CharSet.t;
	blank : char;
	table : state array;
	state_register : state_index;
} *)

val extract : string -> data
