MAPL ?= mapl
SRCS = step0_repl.dyalogbuild step1_read_print.dyalogbuild
BINS = $(SRCS:%.dyalogbuild=%.dws)
SRCDIR=$(dir $(realpath $(firstword $(MAKEFILE_LIST))))

all : $(BINS)

$(BINS):  %.dws: %.dyalogbuild %.dyalog $(OTHER_SRCS)
	WSPATH=${SRCDIR} MAL_STEP="$@" ${MAPL} build

clean:
	rm -f $(BINS)
