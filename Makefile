NAME = ft_turing

SRCSFILES = utils.ml tape.ml json.ml main.ml
INTSFILES = tape.mli

SRCDIR = ./srcs

CAMLC = ocamlc
CAMLOPT = ocamlopt

FLAGS = -thread -package core,yojson

LD_FLAGS = -linkpkg
LD_FLAGS_BYT = $(LD_FLAGS:.cmxa=.cma)
LD_FLAGS_OPT = $(LD_FLAGS)

all: $(NAME)

$(NAME): opt byt
	@ln -sf $(NAME).opt $(NAME)

opt: $(NAME).opt
byt: $(NAME).byt

SRCS = $(addprefix $(SRCDIR)/,$(SRCSFILES))
INTS = $(addprefix $(SRCDIR)/,$(INTSFILES))

OBJS = $(SRCS:.ml=.cmo)
OPTOBJS = $(SRCS:.ml=.cmx)
CMI = $(INTS:.mli=.cmi)

$(NAME).byt: $(CMI) $(OBJS)
	ocamlfind ocamlc $(LD_FLAGS_BYT) $(FLAGS) -o $(NAME).byt $(OBJS)

$(NAME).opt: $(CMI) $(OPTOBJS)
	ocamlfind ocamlopt $(LD_FLAGS_OPT) $(FLAGS) -o $(NAME).opt $(OPTOBJS)

.SUFFIXES:
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

unit : tests/*
	make -f Maketest

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
# ls -d  ~/.opam/4.02.3/lib/core || opam install -y core
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
