PACKAGE         ?= lsim
VERSION         ?= $(shell git describe --tags)
BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
REBAR            = $(shell pwd)/rebar3
MAKE						 = make

.PHONY: test eqc

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

check: test xref dialyzer lint

test: ct eunit

lint:
	${REBAR} as lint lint

eqc:
	${REBAR} as test eqc

eunit:
	${REBAR} as test eunit

ct: state-based op-based simulations

state-based:
	${REBAR} as test ct --suite=test/lsim_state_based_modes_SUITE

op-based:
	${REBAR} as test ct --suite=test/lsim_op_based_modes_SUITE

simulations:
	${REBAR} as test ct --suite=test/lsim_simulations_SUITE

cover:
	pkill -9 beam.smp; ${REBAR} as test ct --cover ; \
		${REBAR} cover

shell:
	${REBAR} shell --apps lsim

##
## Release targets
##

stage:
	${REBAR} release -d

##
## Simulation targets
##

logs:
	  tail -F priv/lager/*/log/*.log

DIALYZER_APPS = kernel stdlib erts sasl eunit syntax_tools compiler crypto

include tools.mk
