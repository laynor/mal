SRCS = step0_repl.dyalogbuild
BINS = $(SRCS:%.dyalogbuild=%.dws)

all : $(BINS)

$(BINS):  %: %.dyalogbuild $(OTHER_SRCS)
	bulid $@.dyalogbuild

clean:
	rm -f $(BINS)
