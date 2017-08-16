open Printf

type t = {
	mutable data : bytes;
	mutable head : int;
	mutable size : int;
	mutable lbound : int;
	mutable rbound : int;
}

let extend_size = 256
let blank = '.'

let create str = {
	data = Bytes.of_string str;
	head = 0;
	size = String.length str;
	lbound = 0;
	rbound = String.length str;
}

let to_string tape =
	let start = if (tape.head < tape.lbound) then tape.head else tape.lbound in
	let stop = if (tape.head >= tape.rbound) then tape.head + 1 else tape.rbound in
	Bytes.sub_string tape.data start (stop - start)

let get tape =
	Bytes.get tape.data tape.head

let set tape c =
	Bytes.set tape.data tape.head c;
	if (tape.head < tape.lbound) then
		tape.lbound <- tape.head
	else if (tape.head >= tape.rbound) then
		tape.rbound <- tape.head + 1

let right tape =
	if (tape.head = tape.size - 1) then begin
		tape.data <- Bytes.extend tape.data 0 extend_size;
		Bytes.fill tape.data tape.size extend_size blank;
		tape.size <- tape.size + extend_size
	end;
	tape.head <- tape.head + 1

let left tape =
	if (tape.head = 0) then begin
		tape.data <- Bytes.extend tape.data extend_size 0;
		Bytes.fill tape.data 0 extend_size blank;
		tape.size <- tape.size + extend_size;
		tape.head <- tape.head + extend_size;
		tape.lbound <- tape.lbound + extend_size;
		tape.rbound <- tape.rbound + extend_size
	end;
	tape.head <- tape.head - 1

let print tape = 
	printf "Tape : %s\n" (Bytes.to_string tape.data)

let color_print tape =
	let start = if (tape.head < tape.lbound) then tape.head else tape.lbound in
	let stop = if (tape.head >= tape.rbound) then tape.head + 1 else tape.rbound in
	let rec loop i =
		if (i >= stop) then
			print_char '\n'
		else begin
			begin if (i = tape.head) then
				printf "\x1B[33m%c\x1B[0m" (Bytes.get tape.data i)
			else
				print_char (Bytes.get tape.data i)
			end;
			loop (i + 1)
		end
	in
	print_string "Tape : ";
	loop start

let _to_string ?(color = false) tape =
	let start = if (tape.head < tape.lbound) then tape.head else tape.lbound in
	let stop = if (tape.head >= tape.rbound) then tape.head + 1 else tape.rbound in
	let rec loop i acc =
		if (i >= stop) then
			acc
		else begin
			if (i = tape.head) then
				if color then
					loop (i + 1) (acc ^ "\x1B[33m" ^ (String.make 1 (Bytes.get tape.data i)) ^ "\x1B[0m")
				else
					loop (i + 1) (acc ^ "<" ^ (String.make 1 (Bytes.get tape.data i)) ^ ">")
			else
				loop (i + 1) (acc ^ (String.make 1 (Bytes.get tape.data i)))
		end
	in
	loop start ""

let print_infos tape =
	Printf.printf "----- TAPE -----\n";
	Printf.printf "data : %s\n" (to_string tape);
	Printf.printf "head : %d\n" tape.head;
	Printf.printf "size : %d\n" tape.size;
	Printf.printf "lbound : %d\n" tape.lbound;
	Printf.printf "rbound : %d\n" tape.rbound;
	Printf.printf "----------------\n";
