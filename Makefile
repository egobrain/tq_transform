all:
	@rebar compile skip_deps=true
	
compile:
	@rebar compile

deps:
	@rebar get-deps
	@rebar compile
	
test:
	@mkdir -p logs
	@ct_run -dir test -logdir logs -cover test/cover.spec -pa ebin/

eunit:
	@rebar eunit skip_deps=true

.PHONY: deps compile all test eunit
