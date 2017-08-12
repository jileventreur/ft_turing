let get_json filename = 
	try Yojson.Basic.from_file filename with
	| Yojson.Json_error str -> Printf.printf "ft_turing : %s : %s\n" filename str ; exit 2
	| Sys_error str -> Printf.printf "ft_turing : %s\n" str ; exit 2
	| _ -> Printf.printf "ft_turing : %s : Error opening json file\n" filename ; exit 2

let extract filename = 
	let open Yojson.Basic.Util in
	try 
	get_json filename
	with
	| _ -> Printf.printf "ft_turing : %s : Error opening json file\n" filename ; exit 2





(* 
condition de parsing :
	- Fichier existant et bien formate (pas d'exception a l'ouverture)
	- Toutes les keys attendue sont presentes (pas + pas -) sont uniques et sont du type attendu
 	- chaque etats de states correspond a une transition sauf les transitions finales
 	- blank appartient a alphabet
 	- initial et finals appartiennent a states
 	- transitions a etudier
 preparer la structure de stocakge des donnes parsees : 
 	- name : string
 	- alphabet : set ?
 	- blank : char
 	- states : map key string ; val transition 
 	- transition : : map key : char ; val (string, char, tape -> unit)
 *)