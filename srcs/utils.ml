let rec loopf f arg n =
	if (n > 0) then begin
		f arg;
		loopf f arg (n - 1)
	end

let fail = 0
let success = 1
let crash = 2

let is_char str = String.length str = 1
let equal s1 s2 = String.compare s1 s2 = 0
let lstfst lst = let (fst, _) = List.split lst in fst 
let lstsnd lst = let (_, snd) = List.split lst in snd 

(* expect string list *)
let print_list lst = 
	Printf.printf "---------- LIST ----------\n";
	List.iter (fun s -> Printf.printf "%s\n" s) lst;
	Printf.printf "--------------------------\n"