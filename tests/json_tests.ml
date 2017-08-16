open Json
open Utils
open Unit

let json_test_open_file () =
	test @@ exit_test (fun () -> ignore (Json.extract "nonexistent file")) fail &&
	test @@ exit_test (fun () -> ignore (Json.extract "json/bad_format.json")) fail &&
	test @@ exit_test (fun () -> ignore (Json.extract "json/unary_sub.json")) success

let json_test_members () =
	test @@ exit_test (fun () -> ignore (Json.extract "bad_members.json")) fail &&
	test @@ exit_test (fun () -> ignore (Json.extract "bad_members.json")) fail &&
	test @@ exit_test (fun () -> ignore (Json.extract "bad_type1.json")) fail &&
	test @@ exit_test (fun () -> ignore (Json.extract "bad_type2.json")) fail &&
	test @@ exit_test (fun () -> ignore (Json.extract "bad_type3.json")) fail &&
	test @@ exit_test (fun () -> ignore (Json.extract "bad_type4.json")) fail &&
	test @@ exit_test (fun () -> ignore (Json.extract "blank_error.json")) fail &&
	test @@ exit_test (fun () -> ignore (Json.extract "finals_error.json")) fail &&
	test @@ exit_test (fun () -> ignore (Json.extract "initial_error.json")) fail &&
	test @@ exit_test (fun () -> ignore (Json.extract "dup_members.json")) fail

let json_test_bad_transition () =
	test @@ exit_test (fun () -> ignore (Json.extract "transition_bad_action.json")) fail &&
	test @@ exit_test (fun () -> ignore (Json.extract "transition_bad_read.json")) fail &&
	test @@ exit_test (fun () -> ignore (Json.extract "transition_bad_to_state.json")) fail &&
	test @@ exit_test (fun () -> ignore (Json.extract "transition_bad_type.json")) fail &&
	test @@ exit_test (fun () -> ignore (Json.extract "transition_bad_write.json")) fail &&
	test @@ exit_test (fun () -> ignore (Json.extract "transition_read_dup.json")) fail &&
	test @@ exit_test (fun () -> ignore (Json.extract "transition_dup_state.json")) fail &&
	test @@ exit_test (fun () -> ignore (Json.extract "transition_dup_member.json")) fail &&
	test @@ exit_test (fun () -> ignore (Json.extract "transition_bad_member.json")) fail &&
	test @@ exit_test (fun () -> ignore (Json.extract "transition_unkwow_state.json")) fail

let _create_sub_table () = 
	let table = Array.make 2 (Final("HALT")) in
	let scanright = 
		let tab = Array.make 256 (Undefined) in
		Array.set tab (int_of_char '.') (Defined('.', Turing.Right, 0));
		tab
	in
	Array.set table 0 (Normal("scanright", scanright));
	table

let json_test_data () =
	let data = extract "json/test_data.json" in
	let refdata = {
		name = "unary_sub"; 
		alphabet = CharSet.of_list ['1'; '.'; '-'; '='];
		blank = '.';
		table = _create_sub_table ();
		state_register = 0;}
	in
	test @@ (data = refdata) &&
	test @@ not @@ (data == refdata)

let main () = 
	Printf.printf "---------- JSON ----------\n";
	unit_test 
		[("json_test_open_file", json_test_open_file);
		("json_test_members", json_test_members);
		("json_test_bad_transition", json_test_bad_transition);
		("json_test_data", json_test_data)];
	Printf.printf "--------------------------\n"