CAMLC = ocamlopt.opt

INCLUDE = \
	-I ${OOPS_SRC}/typeutil/src \
	-I ${OOPS_SRC}/ostap/src \
	-I ${OOPS_SRC}/ostap/util \
        -I ../sll

LIBS = \
	typeutil.cmxa \
	str.cmxa \
	${OOPS_SRC}/ostap/util/ostap.cmx \
        ../sll/sll.cmx \
        ../sll/arithm.cmx \
        ../sll/interpret.cmx \

CAMLC_FLAGS = \
	-c -rectypes \
	-pp "camlp5o ${OOPS_SRC}/ostap/camlp5/pa_ostap.cmo -I ${OOPS_SRC}/ostap/camlp5"

CAMLD_FLAGS = \
	-rectypes

parser: parser.cmx
	${CAMLC} ${INCLUDE} ${CAMLD_FLAGS} ${LIBS} $^ -o $@

.SUFFIXES: .ml .cmx

.ml.cmx:
	${CAMLC} ${INCLUDE} ${CAMLC_FLAGS} $<

clean:
	rm -f *.cm[ix]
	rm -f *.o
