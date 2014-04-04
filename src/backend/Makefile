CAMLC = ocamlopt.opt
CC = gcc
CFLAGS = -std=c99 -pedantic -Wall -g

INCLUDE = \
	-I ${OOPS_SRC}/sll

LIBS = \
	${OOPS_SRC}/sll/sll.cmx \
	${OOPS_SRC}/sll/arithm.cmx

CAMLC_FLAGS = \
	-c

CAMLD_FLAGS =

all:
	make -C ${OOPS_SRC}/sll
	make arithmtest

arithmtest: arithmtest.o runtime.o

arithmtest.c: arithmtest_emit module_prolog.c module_epilog.c runtime.h
	./arithmtest_emit > arithmtest.c

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
