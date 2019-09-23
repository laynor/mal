SRCS = step0_repl.idr
OTHER_SRCS =
BINS = $(SRCS:%.idr=%)

#####################

all: $(BINS)

$(BINS): %: %.idr $(OTHER_SRCS)
	idris --build $@.ipkg

clean:
	rm -f $(BINS) mal *.hi *.o
