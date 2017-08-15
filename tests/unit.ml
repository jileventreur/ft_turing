open Unix
open Printf
open Utils

let blank = '.'

let test_call = ref 0
let max_len = ref 0

(* test functions that may quit main program *)
let exit_test f expected_ret = 
	if (fork () = 0) then begin
		let dnull = open_out "/dev/null" in
		dup2 (descr_of_out_channel dnull) stdout;
		dup2 (descr_of_out_channel dnull) stderr;
		begin try f () with
		| _ -> exit (crash)
		end; 
		exit (success)
	end
	else begin
		let (other , returncode) = wait () in
		match returncode with
		| WEXITED ret when ret = crash -> failwith "Crash test"
		| WEXITED ret -> ret = expected_ret
		| _ -> false
	end

(* To use on each boolean test to achieve display *)
let test res =
	begin if res then
		printf "\x1B[32m"
	else
		printf "\x1B[31m"
	end;
	printf ".\x1B[0m";
	incr test_call;
	res

(* expect a test (name, function) list; execute them and display results *)
let unit_test flist =
	max_len := max !max_len (List.fold_left (fun lmax (str, _) -> max lmax (String.length str)) 0 flist);
	let test_f (name, f) = 
		printf "%s %*s: " name (!max_len - (String.length name)) "";
		begin
			let spaces_number () = 12 - !test_call in
			try begin if (f ()) then
				printf "%*s[\x1B[32mOK\x1B[0m]\n" (spaces_number ()) ""
			else
				printf "%*s[\x1B[31mKO\x1B[0m]\n" (spaces_number ()) ""
			end
			with 
				| _ -> printf "%*s[\x1B[33mCRASH\x1B[0m]\n" (spaces_number ()) ""
		end;
		test_call := 0
	in
	List.iter test_f flist