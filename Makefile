PACKAGE         ?= exp
REBAR            = $(shell pwd)/rebar3

.PHONY: test rel

all: compile

compile:
	$(REBAR) compile

check: test xref dialyzer lint

test: ct eunit
	${REBAR} cover -v

lint: erl-lint

erl-lint:
	${REBAR} as lint lint

shell-lint:
	ls -d bin/* | grep -v ".erl" | xargs shellcheck

docker-lint:
	for f in $$(ls -d Dockerfiles/*); do dockerlint $$f; done

eunit:
	${REBAR} eunit

ct:
	${REBAR} ct --readable=false --verbose

xref:
	${REBAR} xref skip_deps=true

dialyzer:
	${REBAR} dialyzer

cover: test
	open _build/test/cover/index.html

shell:
	${REBAR} shell --apps ${PACKAGE}

rel:
	rm -rf _build/default/rel/
	${REBAR} release

modes:
	pkill -9 beam.smp ; rm -rf priv/lager ; ${REBAR} ct --readable=false --verbose --suite exp_modes_SUITE

simulations:
	pkill -9 beam.smp ; rm -rf priv/lager ; ${REBAR} ct --readable=false --verbose --suite exp_simulations_SUITE

logs:
	  tail -F priv/lager/*/log/*.log

run:
	  _build/default/rel/${PACKAGE}/bin/env
