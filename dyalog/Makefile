SH ?= bash
MAPL ?= mapl
SRCS = step0_repl.dyalogbuild \
       step1_read_print.dyalogbuild \
       step2_eval.dyalogbuild \
       step3_env.dyalogbuild \
       step4_if_fn_do.dyalogbuild \
       step5_tco.dyalogbuild \
       step6_file.dyalogbuild \
       step7_quote.dyalogbuild \
       step8_macros.dyalogbuild \
       step9_try.dyalogbuild \
       stepA_mal.dyalogbuild
BINS = $(SRCS:%.dyalogbuild=Distribution/%.dws)
SRCDIR=$(dir $(realpath $(firstword $(MAKEFILE_LIST))))

OTHER_SRCS= Chars.dyalog \
            core.dyalog \
            debug.dyalog \
            display.dyalog \
            Env.dyalog \
            errors.dyalog \
            Errors.dyalog \
            mal.dyalog \
            notes.dyalog \
            Printer.dyalog \
            Reader.dyalog \
            Types.dyalog

all : $(BINS)

$(BINS):  Distribution/%.dws: %.dyalogbuild %.dyalog $(OTHER_SRCS)
	$(SH) build.sh "$*"

clean:
	rm -f $(BINS)
