let () =
	let a = [ "a" ; "b" ; "c" ] in
	let b = [ 1 ; 2 ; 3] in

	let c = List.flatten (List.map ( fun x -> List.map (fun y -> x ^ (string_of_int y)) b ) a) in
	List.iter print_endline c
