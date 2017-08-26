# @Author: Debray Arnaud <adebray>
# @Date:   2017-08-26T01:32:00+02:00
# @Email:  adebray@student.42.fr
# @Last modified by:   adebray
# @Last modified time: 2017-08-26T20:13:59+02:00

ocamlfind ocamlc -thread -package str,yojson -linkpkg compiler.ml -o compiler \
&& ./compiler $1 $2 \
&& ./ft_turing $3 $4 sim_${1:t} "|0yn.|.|123|1|32|102.R1.3yR201.R2.3nR|000|"
