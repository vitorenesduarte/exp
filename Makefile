PACKAGE         ?= exp
REBAR            = $(shell pwd)/rebar3

.PHONY: test

all: compile

compile:
	$(REBAR) compile

check: test xref dialyzer lint

test: ct eunit
	${REBAR} cover -v

lint: erl-lint #shell-lint docker-lint

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

stage:
	${REBAR} release -d

logs:
	  tail -F priv/lager/*/log/*.log

run:
	  _build/default/rel/${PACKAGE}/bin/env
