MAPL ?= mapl
SRCS = step0_repl.dyalogbuild step1_read_print.dyalogbuild
BINS = $(SRCS:%.dyalogbuild=%)

all : $(BINS)

$(BINS):  %: %.dyalogbuild %.dyalog $(OTHER_SRCS)
	MAL_STEP="$@" ${MAPL} build

clean:
	rm -f $(BINS)
	
