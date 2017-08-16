type tape = { data : string; head : int; size : int; }
type exn = Ok of tape | Error of string
val create : string -> tape
val get : tape -> char
val geti : tape -> int -> char
val set : tape -> Core.Std.Char.t -> tape
val right : tape -> exn
val left : tape -> exn
val to_string : ?color:bool -> tape -> string
