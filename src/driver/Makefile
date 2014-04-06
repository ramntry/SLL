CAMLC = ocamlopt.opt
CC = gcc
CFLAGS = -std=c99 -pedantic -Wall -O2

INCLUDE = \
	-I ${OOPS_SRC}/typeutil/src \
	-I ${OOPS_SRC}/ostap/src \
	-I ${OOPS_SRC}/ostap/util \
	-I ../sll \
	-I ../parser \
	-I ../backend

LIBS = \
	typeutil.cmxa \
	str.cmxa \
	unix.cmxa \
	${OOPS_SRC}/ostap/util/ostap.cmx \
	../sll/sll.cmx \
	../sll/arithm.cmx \
	../sll/interpret.cmx \
	../parser/parser.cmx \
	../backend/emit_c.cmx

CAMLC_FLAGS = \
	-c -rectypes

CAMLD_FLAGS =

all:
	make -C ../backend
	make -C ../parser
	make driver

driver: driver.cmx
	${CAMLC} ${INCLUDE} ${CAMLD_FLAGS} ${LIBS} $^ -o $@

test: test.sll
	./driver ./test.sll

.depend:
	ocamldep *.ml > .depend

clean:
	rm -f arithmtest.c
	rm -f *.cm[ix]
	rm -f *.o
	rm -f .depend
	make clean -C ../backend
	make clean -C ../parser
	make clean -C ../sll

.SUFFIXES: .ml .cmx .mli .cmi

.mli.cmi:
	${CAMLC} ${INCLUDE} ${CAMLC_FLAGS} $<

.ml.cmx:
	${CAMLC} ${INCLUDE} ${CAMLC_FLAGS} $<

include .depend
