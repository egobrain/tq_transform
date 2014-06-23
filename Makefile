all:
	@rebar compile skip_deps=true

compile:
	@rebar compile

deps:
	@rebar get-deps
	@rebar compile

clean:
	@rm -Rf ebin .eunit log

test: all
	@rebar eunit skip_deps=true

.PHONY: deps compile all test clean
