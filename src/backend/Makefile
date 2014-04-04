CAMLC = ocamlopt.opt

INCLUDE = \
	-I ../sll

LIBS = \
	../sll/sll.cmx \
	../sll/arithm.cmx

CAMLC_FLAGS = \
	-c

CAMLD_FLAGS =

all:
	make -B -C ../sll
	make arithmtest

arithmtest: arithmtest_emit runtime.c module_prolog.c module_epilog.c runtime.h
	./arithmtest_emit > arithmtest.c
	gcc -std=c99 -pedantic -Wall -O2 arithmtest.c runtime.c -o $@

arithmtest_emit: emit_c.cmx arithmtest_emit.cmx
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
