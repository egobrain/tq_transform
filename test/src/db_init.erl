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

-field({init_field,
	   [
		required,
		{type, integer},
		record, get, set,
		{default, 1},
		{init, field_init1},
		{init, field_init2}
	   ]}).

-model([
		{init, init1},
		{init, init2}
	   ]).

init1(Model) ->
	case Model:counter() of
		undefined ->
			Model;
		Counter ->
			Model:set_counter(Counter*3)
	end.

init2(Model) ->
	case Model:counter() of
		undefined ->
			Model;
		Counter ->
			Model:set_counter(Counter*10)
	end.

field_init1(FieldVal) ->
	FieldVal*3.
field_init2(FieldVal) ->
	FieldVal*10.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

init_test_() ->
	C = fun(V) ->
				{ok, Model} = from_proplist([{counter, V}]),
				Model#?MODULE{'$is_new$'=false}
		end,
	[fun() -> ?assertEqual(C(30), (constructor([]))([])) end,
	 fun() -> ?assertEqual(C(150), (constructor([counter]))([5])) end].

init_field_test_() ->
	G = fun(M) ->
				M:init_field()
		end,
	[fun() -> ?assertEqual(1, G((constructor([]))([]))) end,
	 fun() -> ?assertEqual(150, G((constructor([init_field]))([5]))) end].

changed_test() ->
	Model = new(),
	Model2 = Model:set_counter(15),
	?assertEqual(Model2:is_changed(counter), true),
	?assertEqual(Model2:counter(), 15).

-endif.
