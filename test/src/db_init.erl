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
		{init, init1},
		{init, init2}
	   ]).

init1(Model) ->
	Model:set_counter(Model:counter()*3).
init2(Model) ->
	Model:set_counter(Model:counter()*10).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

init_test_() ->
	C = fun(V) ->
				{ok, Model} = from_proplist([{counter, V}]),
				Model#?MODULE{'$is_new$'=false}
		end,
	[fun() -> ?assertEqual(C(30), (constructor([]))([])) end,
	 fun() -> ?assertEqual(C(150), (constructor([counter]))([5])) end].

changed_test() ->
	Model = new(),
	Model2 = Model:set_counter(15),
	?assertEqual(Model2:is_changed(counter), true),
	?assertEqual(Model2:counter(), 15).

-endif.
