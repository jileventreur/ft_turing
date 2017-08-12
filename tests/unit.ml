let blank = '.'

let test_call = ref 0
let max_len = ref 0

let test res =
	begin if res then
		Printf.printf "\x1B[32m"
	else
		Printf.printf "\x1B[31m"
	end;
	Printf.printf ".\x1B[0m";
	incr test_call;
	res

let unit_test flist =
	max_len := max !max_len (List.fold_left (fun lmax (str, _) -> max lmax (String.length str)) 0 flist);
	let test_f (name, f) = 
		Printf.printf "%s %*s: " name (!max_len - (String.length name)) "";
		begin
			let spaces_number () = 10 - !test_call in
			try begin if (f ()) then
				Printf.printf "%*s[\x1B[32mOK\x1B[0m]\n" (spaces_number ()) ""
			else
				Printf.printf "%*s[\x1B[31mKO\x1B[0m]\n" (spaces_number ()) ""
			end
			with 
				| _ -> Printf.printf "%*s[\x1B[33mCRASH\x1B[0m]\n" (spaces_number ()) ""
		end;
		test_call := 0
	in
	List.iter test_f flist