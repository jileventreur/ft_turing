type tape = {
	data: string;
	head: int;
	size: int;
}

type exn = Ok of tape | Error of string

let create str = {
	data = str;
	head = 0;
	size = String.length str;
}

let get tape = String.get tape.data tape.head
let geti tape i = String.get tape.data i
let set tape _char =
	let rec _loop ?(i = 0) ?(acc = "") () =
		if i < tape.size then
			if i = tape.head then
				_loop ~i:(i + 1) ~acc:(acc ^ (Core.Std.Char.to_string _char)) ()
			else
				_loop ~i:(i + 1) ~acc:(acc ^ (Core.Std.Char.to_string (geti tape i))) ()
		else { tape with data = acc }
	in _loop ()

let right tape =
	if tape.head + 1 < tape.size then Ok { tape with head = tape.head + 1 }
	else Error "Out of bound"

let left tape =
	if tape.head - 1 >= 0 then Ok { tape with head = tape.head - 1 }
	else Error "Out of bound"

let to_string ?(color = false) tape =
	let rec _loop ?(i = 0) ?(acc = "") () =
		let _partial_loop = _loop ~i:(i + 1) in
		if i < tape.size then
			if i = tape.head then
				if color then
					_partial_loop ~acc:(acc ^ "\x1B[33m" ^ (Core.Std.Char.to_string (geti tape i)) ^ "\x1B[0m") ()
				else
					_partial_loop ~acc:(acc ^ "<" ^ (Core.Std.Char.to_string (geti tape i)) ^ ">") ()
			else
				_partial_loop ~acc:(acc ^ (Core.Std.Char.to_string (geti tape i))) ()
		else
			acc
	in _loop ()

(* let () =
	let t = create "111+11=" in
	print_endline (to_string t);
	print_char (get t) *)
