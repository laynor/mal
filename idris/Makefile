SRCS = step0_repl.idr step1_read_print.idr step2_eval.idr
OTHER_SRCS = Reader.idr
BINS = $(SRCS:%.idr=%)

#####################

all: $(BINS)

$(BINS): %: %.idr $(OTHER_SRCS)
	idris --build $@.ipkg

clean:
	rm -f $(BINS) mal *.hi *.o
