# @Author: Debray Arnaud <adebray>
# @Date:   2017-08-26T01:32:00+02:00
# @Email:  adebray@student.42.fr
# @Last modified by:   adebray
# @Last modified time: 2017-08-27T02:00:38+02:00

ocamlfind ocamlc -thread -package str,yojson -linkpkg ft_compiler.ml -o ft_compiler &&
./ft_compiler $1 $2 &&
ocamlfind ocamlopt -thread -package core,yojson -I ./compiler -c compiler/compiler.ml &&
ocamlfind ocamlopt -linkpkg -thread -package core,yojson -o compiler/compile ./compiler/compiler.cmx &&
rm -rf machines/complete_turing.json && ./compiler/compile &&
mv complete_turing.json machines

node <<EOF
let check = require("./sim_ispair.json")
let exec = require("./machines/complete_turing.json")

check.transitions["ERASE"] = check.transitions["ERASE"].map( e => {
	if (e.to_state == "TEST")
		e.to_state = exec.initial
	return e
})

check.states = check.states.concat(exec.states).filter(function(item, pos, a) {
    return a.indexOf(item) == pos;
})
check.transitions = Object.assign(check.transitions, exec.transitions)

require('fs').writeFileSync('test.json', JSON.stringify(check, null, "  "))

EOF
