# @Author: Debray Arnaud <adebray>
# @Date:   2017-08-26T01:32:00+02:00
# @Email:  adebray@student.42.fr
# @Last modified by:   adebray
# @Last modified time: 2017-08-26T01:32:32+02:00

ocamlfind ocamlc -thread -package str,yojson -linkpkg compiler.ml -o compiler && ./compiler $1 $2
