# @Author: Debray Arnaud <adebray>
# @Date:   2017-08-26T01:32:00+02:00
# @Email:  adebray@student.42.fr
# @Last modified by:   adebray
# @Last modified time: 2017-08-27T20:23:01+02:00

ocamlfind ocamlc -thread -package str,yojson -linkpkg compiler/ft_compiler.ml -o compiler/ft_compiler &&
./compiler/ft_compiler $1 $2 &&

ocamlfind ocamlopt -thread -package core,yojson -I ./compiler -c compiler/compiler.ml &&
ocamlfind ocamlopt -linkpkg -thread -package core,yojson -o compiler/compile ./compiler/compiler.cmx &&
rm -rf machines/complete_turing.json &&
./compiler/compile &&

node res/do_compile.js complete_turing.json sim_${1:t} &&
./ft_turing $3 $4 ./test.json `cat input_${1:t}` &&
# rm -rf complete_turing.json
