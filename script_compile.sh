# @Author: Debray Arnaud <adebray>
# @Date:   2017-08-27T00:50:57+02:00
# @Email:  adebray@student.42.fr
# @Last modified by:   adebray
# @Last modified time: 2017-08-27T01:03:53+02:00

ocamlfind ocamlopt -thread -package core,yojson -I ./compiler -c compiler/compiler.ml &&
ocamlfind ocamlopt -linkpkg -thread -package core,yojson -o compiler/compile ./compiler/compiler.cmx &&
rm -rf machines/complete_turing.json && ./compiler/compile &&
mv complete_turing.json machines && ruby pretty_json.rb machines/complete_turing.json
