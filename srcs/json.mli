module CharSet : Set.S with type elt = char

type state_index = int

(* transition : Defined (write, action, to_state) *)
type transition = Defined of char * Turing.action * state_index | Undefined

(* state : Normal (name, transition array indexed by readed char) OR Final (name) *)
type state = Normal of (string * transition array) | Final of string

type data = {
	name : string;
	alphabet : CharSet.t;
	blank : char;
	table : state array;
	state_register : state_index;
}

(* exctract : json filename -> turing data *)
val extract : string -> data 