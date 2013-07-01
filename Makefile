all:
	@rebar compile skip_deps=true

compile:
	@rebar compile

deps:
	@rebar get-deps
	@rebar compile


eunit:
	@rebar eunit skip_deps=true

.PHONY: deps compile all test eunit
