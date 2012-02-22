include Makefile.in

MAKECMD=make -C

all:
	$(MAKECMD) accessibility
	$(MAKECMD) alloc
	$(MAKECMD) atomic
	$(MAKECMD) barrier
	$(MAKECMD) broadcast
	$(MAKECMD) collect
	$(MAKECMD) reductions
	$(MAKECMD) transfer

run:
	$(MAKECMD) accessibility run
	$(MAKECMD) alloc run
	$(MAKECMD) atomic run
	$(MAKECMD) barrier run
	$(MAKECMD) broadcast run
	$(MAKECMD) collect run
	$(MAKECMD) reductions run
	$(MAKECMD) transfer run

clean:
	$(MAKECMD) accessibility clean
	$(MAKECMD) alloc clean
	$(MAKECMD) atomic clean
	$(MAKECMD) barrier clean
	$(MAKECMD) broadcast clean
	$(MAKECMD) collect clean
	$(MAKECMD) reductions clean
	$(MAKECMD) transfer clean
