PACKAGE         ?= lsim
VERSION         ?= $(shell git describe --tags)
BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
REBAR            = $(shell pwd)/rebar3
MAKE						 = make

.PHONY: test

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

lint: erl-lint shell-lint docker-lint

erl-lint:
	${REBAR} as lint lint

shell-lint:
	ls -d bin/* | grep -v ".erl" | xargs shellcheck

docker-lint:
	for f in $(ls -d Dockerfiles/*); do dockerlint $f; done

eunit:
	${REBAR} as test eunit

ct: state-based driven-based simulations

state-based:
	${REBAR} as test ct --suite=test/lsim_state_based_modes_SUITE

driven-based:
	${REBAR} as test ct --suite=test/lsim_driven_based_modes_SUITE

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

run: stage
	  _build/default/rel/lsim/bin/env

DIALYZER_APPS = kernel stdlib erts sasl eunit syntax_tools compiler crypto

include tools.mk
