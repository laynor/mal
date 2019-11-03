SH ?= bash
MAPL ?= mapl
SRCS = step0_repl.dyalogbuild \
       step1_read_print.dyalogbuild \
       step2_eval.dyalogbuild \
       step3_env.dyalogbuild \
       step4_if_fn_do.dyalogbuild \
       step5_tco.dyalogbuild
BINS = $(SRCS:%.dyalogbuild=Distribution/%.dws)
SRCDIR=$(dir $(realpath $(firstword $(MAKEFILE_LIST))))

all : $(BINS)

$(BINS):  Distribution/%.dws: %.dyalogbuild %.dyalog $(OTHER_SRCS)
	$(SH) build.sh "$*"

clean:
	rm -f $(BINS)
