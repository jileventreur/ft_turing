open Utils
let blank = '.'

let tape_test_sample () =
	let tape = Tape.create "coucou" in
	Unit.test (String.equal (Tape.to_string tape) "coucou")
	&& Unit.test (Tape.get tape = 'c')
	&& begin 
		Tape.right tape;
		Unit.test (Tape.get tape = 'o') 
	end && begin
		Tape.set tape 'e';
		Unit.test (Tape.get tape = 'e') 
	end && Unit.test (String.equal (Tape.to_string tape) "ceucou")

let tape_test_extension () =
	let tape = Tape.create "c" in
	begin 
		Tape.left tape;
		Unit.test (Tape.get tape = blank)
	end && begin
		loopf Tape.right tape 2;
		Unit.test (Tape.get tape = blank)
	end && begin
		loopf Tape.right tape 1000;
		Unit.test (Tape.get tape = blank)
	end && begin
		loopf Tape.left tape 1001;
		Unit.test (Tape.get tape = 'c')
	end && begin
		loopf Tape.left tape 1000;
		Unit.test (Tape.get tape = blank)
	end && begin
		loopf Tape.right tape 1000;
		Unit.test (Tape.get tape = 'c')
	end

let tape_test_modulable_length () = 
	let tape = Tape.create "abc" in
	begin 
		Tape.left tape;
		Unit.test (String.equal (Tape.to_string tape) ".abc")
	end && begin
		loopf Tape.right tape 4;
		Unit.test (String.equal (Tape.to_string tape) "abc.")
	end && begin
		loopf Tape.right tape 3;
		Tape.set tape 'd';
		Unit.test (String.equal (Tape.to_string tape) "abc...d")
	end && begin
		loopf Tape.left tape 8;
		Tape.set tape 'e';
		Unit.test (String.equal (Tape.to_string tape) "e.abc...d")
	end && begin
		loopf Tape.left tape 2;
		Unit.test (String.equal (Tape.to_string tape) "..e.abc...d")
	end && begin
		loopf Tape.right tape 2;
		Unit.test (String.equal (Tape.to_string tape) "e.abc...d")
	end

let main () = 
	Printf.printf "---------- TAPE ----------\n";
	Unit.unit_test 
		[("tape_test_sample", tape_test_sample);
		("tape_test_extension", tape_test_extension);
		("tape_test_modulable_length", tape_test_modulable_length)];
	Printf.printf "--------------------------\n"
