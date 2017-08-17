type tape = { data : string; head : int; size : int; }
val create : string -> tape
val get : tape -> char
val geti : tape -> int -> char
val set : tape -> Core.Std.Char.t -> tape
val right : tape -> tape
val left : tape -> tape
val to_string : ?color:bool -> tape -> string
