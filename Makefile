# Makefile for sim1802

SHELL=/bin/bash
REBAR3=$(shell type -p rebar3 || echo ./rebar3)
REBAR3_GIT=https://github.com/erlang/rebar3.git
REBAR3_VSN=3.25.1
TRIPLET=cdp1802-unknown-elf

build:	$(REBAR3)
	$(REBAR3) do compile, xref, dialyzer, escriptize
	mkdir -p bin
	ln -sf ../_build/default/bin/sim1802 bin/

test:	$(REBAR3)
	$(REBAR3) eunit

install:	$(BUILD)
	mkdir -p $(PREFIX)/bin
	cp _build/default/bin/sim1802 $(PREFIX)/bin/$(TRIPLET)-sim
	mkdir -p $(PREFIX)/dejagnu
	cp dejagnu/cdp1802-sim.exp $(PREFIX)/dejagnu/
	@if [ ! -f "${HOME}/.dejagnurc" ]; then \
	    echo creating "${HOME}/.dejagnurc" ; \
	    touch "${HOME}/.dejagnurc" ; \
	fi
	@if ! grep -q "lappend boards_dir \"${PREFIX}/dejagnu\"" "${HOME}/.dejagnurc" ; then \
	    echo updating "${HOME}/.dejagnurc" ; \
	    echo "lappend boards_dir \"${PREFIX}/dejagnu\"" >> "${HOME}/.dejagnurc" ; \
	fi

distclean realclean:	clean
	rm -f ./rebar3

clean:	$(REBAR3)
	$(REBAR3) clean
	rm -rf _build

./rebar3:
	mkdir -p _build; \
	cd _build; \
	git clone --quiet $(REBAR3_GIT); \
	cd rebar3; \
	git checkout --quiet $(REBAR3_VSN); \
	./bootstrap; \
	mv rebar3 ../../; \
	cd ../..; \
	rm -rf _build/rebar3
