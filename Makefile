PACKAGE         ?= dvv
VERSION         ?= $(shell git describe --tags)
BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
REBAR            = rebar3
MAKE						 = make

.PHONY: deps test 

all: compile

##
## Compilation targets
##

compile:
	$(REBAR) compile

clean: packageclean
	$(REBAR) clean

packageclean:
	rm -fr *.deb
	rm -fr *.tar.gz

##
## Test targets
##

check: test dialyzer

test: ct 

eunit:
	${REBAR} as test eunit

dialyzer:
	${REBAR} dialyzer

xref:
	${REBAR} xref

ct:
	${REBAR} ct
	${REBAR} cover

shell:
	${REBAR} shell --apps dvv

##
## Release targets
##

