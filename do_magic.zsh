# @Author: Debray Arnaud <adebray>
# @Date:   2017-08-26T01:32:00+02:00
# @Email:  adebray@student.42.fr
# @Last modified by:   adebray
# @Last modified time: 2017-08-27T00:57:57+02:00

ocamlfind ocamlc -thread -package str,yojson -linkpkg compiler.ml -o ft_compiler \
&& ./ft_compiler $1 $2 \
&& ./ft_turing $3 $4 sim_${1:t} `cat input_${1:t}`
