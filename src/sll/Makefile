CAMLC = ocamlopt.opt

INCLUDE =

LIBS =

CAMLC_FLAGS = \
	-c

CAMLD_FLAGS =

all:
	make arithmtest

arithmtest: sll.cmx interpret.cmx arithm.cmx interpret_arithmtest.cmx
	${CAMLC} ${INCLUDE} ${CAMLD_FLAGS} ${LIBS} $^ -o $@

.depend:
	ocamldep *.ml > .depend

clean:
	rm -f arithmtest.c
	rm -f *.cm[ix]
	rm -f *.o
	rm -f .depend

.SUFFIXES: .ml .cmx .mli .cmi

.mli.cmi:
	${CAMLC} ${INCLUDE} ${CAMLC_FLAGS} $<

.ml.cmx:
	${CAMLC} ${INCLUDE} ${CAMLC_FLAGS} $<

include .depend
