type t
val create : string -> t
val to_string : t -> string
val _to_string : ?color:bool -> t -> string
val get : t -> char
val set : t -> char -> unit
val left : t -> unit
val right : t -> unit

val print : t -> unit
val color_print : t -> unit
val print_infos : t -> unit
