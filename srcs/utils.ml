let rec loopf f arg n =
	if (n > 0) then begin
		f arg;
		loopf f arg (n - 1)
	end

let print_list lst = 
	Printf.printf "---------- LIST ----------\n";
	List.iter (fun s -> Printf.printf "%s\n" s) lst;
	Printf.printf "--------------------------\n"