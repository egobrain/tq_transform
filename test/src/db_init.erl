-module(db_init).
-compile({parse_transform, tq_db_transform}).

%% Test
-field({id,
		[
		 index,
		 {db_type, integer},
		 {type, integer}
		]}).
-field({counter,
		[
		 required,
		 {type, binary},
		 record, get, set,
		 {default, 1}
		]}).

-model([{table, <<"test">>}]).

-init(init).

init(Model) ->
	Model:set_counter(Model:counter()*3).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(join(A, B), list_to_atom(atom_to_list(A)++"_"++atom_to_list(B))).
-define(prefix_set(A), ?join(set, A)).

init_test_() ->
	C = fun(V) ->
				{ok, Model} = from_proplist([{counter, V}]),
				Model#?MODULE{'$is_new$'=false}
		end,
	[fun() -> ?assertEqual(C(3), (constructor([]))([])) end,
	 fun() -> ?assertEqual(C(15), (constructor([counter]))([5])) end].
	
	
-endif.

