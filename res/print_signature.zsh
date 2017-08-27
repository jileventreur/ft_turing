# @Author: Debray Arnaud <adebray>
# @Date:   2017-08-14T16:14:28+02:00
# @Email:  adebray@student.42.fr
# @Last modified by:   adebray
# @Last modified time: 2017-08-14T16:24:51+02:00

if [[ $# != 0 ]]; then
	files=$*
else
	files=(srcs/*.ml)
fi

for x in $files; do
	echo -n "---- ---- " ;
	echo -n $x ;
	echo " ---- ----" ;
	ocamlfind ocamlopt -i -thread -package core,yojson -I ./srcs -c $x ;
done
