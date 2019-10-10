MAPL ?= mapl
SRCS = step0_repl.dyalogbuild mal.dyalogbuild
BINS = $(SRCS:%.dyalogbuild=%)

all : $(BINS)

$(BINS):  %: %.dyalogbuild $(OTHER_SRCS)
	MAL_STEP="$@" ${MAPL} build

clean:
	rm -f $(BINS)
