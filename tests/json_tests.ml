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
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "json/bad_format.json")) fail &&
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "json/unary_sub.json")) success

let json_test_members () =
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "bad_members.json")) fail &&
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "bad_members.json")) fail &&
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "bad_type1.json")) fail &&
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "bad_type2.json")) fail &&
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "bad_type3.json")) fail &&
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "bad_type4.json")) fail &&
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "blank_error.json")) fail &&
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "finals_error.json")) fail &&
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "initial_error.json")) fail &&
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "dup_members.json")) fail

let json_test_bad_transition () =
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "transition_bad_action.json")) fail &&
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "transition_bad_read.json")) fail &&
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "transition_bad_to_state.json")) fail &&
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "transition_bad_type.json")) fail &&
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "transition_bad_write.json")) fail &&
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "transition_read_dup.json")) fail &&
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "transition_dup_state.json")) fail &&
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "transition_dup_member.json")) fail &&
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "transition_bad_member.json")) fail &&
	Unit.test @@ exit_test (fun () -> ignore (Json.extract "transition_unkwow_state.json")) fail

let main () = 
	Printf.printf "---------- JSON ----------\n";
	Unit.unit_test 
		[("json_test_open_file", json_test_open_file);
		("json_test_members", json_test_members);
		("json_test_bad_transition", json_test_bad_transition)];
	Printf.printf "--------------------------\n"
(* 
			[("tape_test_sample", tape_test_sample);
		("tape_test_extension", tape_test_extension);
		("tape_test_modulable_length", tape_test_modulable_length)];
 *)
