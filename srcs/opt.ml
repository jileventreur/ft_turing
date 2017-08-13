type arg = Mandatory | Optional
type args = {
	help: bool;
	jsonfile: string;
	input: string;
}

let getopt av =
	let opts = [|
		("-h", Optional) ;
		("jsonfile", Mandatory) ;
		("input", Mandatory)
	|] in
	let usage () =
		print_string "usage: " ;
		let print_usage x = begin match x with
			| (x, Optional) -> print_string ("[" ^ x ^ "]")
			| (x, Mandatory) -> print_string x
		end ; print_string " "
		in Array.iter print_usage opts ; print_endline "" ; exit 1
	in
	let rec _usage av i args =
		if i < Array.length av then
			if av.(i) = "-h" then
				_usage av (i + 1) ({ args with help = true })
			else match i with
			| 1 -> _usage av (i + 1) ({ args with jsonfile = av.(i) })
			| 2 -> _usage av (i + 1) ({ args with input = av.(i) })
			| _ -> args
		else
			args
	in
	let args = _usage av 1 { help = false ; jsonfile = "" ; input = "" } in
	begin match args with
	| { jsonfile="" }  -> usage ()
	| { input="" } -> usage ()
	| _ -> ()
	end ;
	args
