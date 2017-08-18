# @Author: Debray Arnaud <adebray>
# @Date:   2017-08-17T00:51:46+02:00
# @Email:  adebray@student.42.fr
# @Last modified by:   adebray
# @Last modified time: 2017-08-18T03:40:18+02:00

NAME = ft_turing

MLI = #tape.mli
ML = utils.ml \
 	json_utils.ml \
	table.ml \
	tape.ml \
	json.ml \
 	turing.ml \
	main.ml

SRCDIR = ./srcs

SRCS = $(addprefix $(SRCDIR)/,$(ML))
INTS = $(addprefix $(SRCDIR)/,$(MLI))

OBJS = $(SRCS:.ml=.cmo)
OPTOBJS = $(SRCS:.ml=.cmx)
CMI = $(INTS:.mli=.cmi)

FLAGS = -thread -package core,yojson
LD_FLAGS = -linkpkg
LD_FLAGS_BYT = $(LD_FLAGS:.cmxa=.cma)
LD_FLAGS_OPT = $(LD_FLAGS)

all: depend $(NAME)

$(NAME): opt byt
	@ln -sf $(NAME).opt $(NAME)

opt: $(NAME).opt
byt: $(NAME).byt

$(NAME).byt: $(CMI) $(OBJS)
	ocamlfind ocamlc $(LD_FLAGS_BYT) $(FLAGS) -o $(NAME).byt $(OBJS)

$(NAME).opt: $(CMI) $(OPTOBJS)
	ocamlfind ocamlopt $(LD_FLAGS_OPT) $(FLAGS) -o $(NAME).opt $(OPTOBJS)


.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	ocamlfind ocamlc $(FLAGS) -I $(SRCDIR) -c $<

.mli.cmi:
	ocamlfind ocamlc $(FLAGS) -I $(SRCDIR) $< -o $@

.ml.cmx:
	ocamlfind ocamlopt $(FLAGS) -I $(SRCDIR) -c $<

clean:
	rm -f $(SRCDIR)/*.cm[iox] $(SRCDIR)/*.o
	rm -f $(SRCDIR)/$(NAME).o
	make -f Maketest clean

fclean: clean
	rm -f $(NAME)
	rm -f $(NAME).opt
	rm -f $(NAME).byt
	make -f Maketest fclean

depend:
	ocamldep $(SRCDIR) $(INTS) $(SRCS) > .depend

include .depend

install_libs: #with a working brew on macos
	hash -r
	type ocaml || brew install --build-from-source ocaml
	hash -r
	type opam ||\
		(brew install opam &&\
		opam init -n &&\
		(~/.opam/opam-init/init.zsh > /dev/null 2>/dev/null || true) &&\
		eval `opam config env` &&\
		opam switch 4.02.3 &&\
		(~/.opam/opam-init/init.zsh > /dev/null 2>/dev/null || true) &&\
		eval `opam config env`\
		)
	opam install -y core.113.00.00
	ls -d ~/.opam/4.02.3/lib/yojson || opam install -y yojson
	hash -r
	type ocamlfind || opam install -y ocamlfind
	hash -r
	type gnuplot || brew install gnuplot
	ls -d ./gnuplot-ocaml ||\
		(git clone https://github.com/Ngoguey42/gnuplot-ocamlFORK gnuplot-ocaml &&\
		$(MAKE) -C gnuplot-ocaml\
		)

re: fclean all
