-module(db_init).

-compile({parse_transform, tq_record_transform}).

%% Test
-field({counter,
		[
		 required,
		 {type, binary},
		 record, get, set,
		 {default, 1}
		]}).

-model([
		{init, init}
	   ]).

init(Model) ->
	Model:set_counter(Model:counter()*3).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

init_test_() ->
	C = fun(V) ->
				{ok, Model} = from_proplist([{counter, V}]),
				Model#?MODULE{'$is_new$'=false}
		end,
	[fun() -> ?assertEqual(C(3), (constructor([]))([])) end,
	 fun() -> ?assertEqual(C(15), (constructor([counter]))([5])) end].

-endif.

