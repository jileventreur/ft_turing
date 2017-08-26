ocamlfind ocamlopt -thread -package core,yojson -I ./compiler -c compiler/compiler.ml &&
ocamlfind ocamlopt -linkpkg -thread -package core,yojson -o compiler/compile ./compiler/compiler.cmx &&
rm -rf machines/complete_turing.json && ./compiler/compile &&
mv complete_turing.json machines && ruby pretty_json.rb machines/complete_turing.json
