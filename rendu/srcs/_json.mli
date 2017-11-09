(* module CharSet : Set.S with type elt = char *)

type action = Left | Right
type state_index = int
<<<<<<< HEAD:srcs/_json.mli
type transition = Defined of char * action * state_index | Undefined
=======

(* transition : Defined (write, action, to_state) *)
type transition = Defined of char * Turing.action * state_index | Undefined

(* state : Normal (name, transition array indexed by readed char) OR Final (name) *)
>>>>>>> origin:srcs/json.mli
type state = Normal of (string * transition array) | Final of string
(* 
type data = {
	name : string;
	alphabet : CharSet.t;
	blank : char;
	table : state array;
	state_register : state_index;
} *)

<<<<<<< HEAD:srcs/_json.mli
val extract : string -> data
=======
(* exctract : json filename -> turing data *)
val extract : string -> data 
>>>>>>> origin:srcs/json.mli
