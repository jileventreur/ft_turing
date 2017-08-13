open Unix
open Json
open Utils

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

let json_test_open_file () =
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "nonexistent file")) fail &&
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "json/no_rights.json")) fail &&
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "json/bad_format.json")) fail &&
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "json/unary_sub.json")) success

let main () = 
	Printf.printf "---------- JSON ----------\n";
	Unit.unit_test 
		[("json_test_open_file", json_test_open_file)];
	Printf.printf "--------------------------\n"