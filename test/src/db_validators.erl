-module(db_validators).
-compile({parse_transform, tq_record_transform}).

-export([more_then/2,
		 fail_on_2/1,
		 fail_on/2]).

%% Test

-field({non_neg_integer,
		[
		 {type, non_neg_integer}
		]}).

-field({more_then_10,
		[
		 {type, non_neg_integer},
		 {validators,
		  [
		   {more_then, [10]}
		  ]}
		]}).

-field({more_then_100,
		[
		 {type, non_neg_integer},
		 {validators,
		  [
		   {more_then, [10]},
		   {?MODULE, more_then, [100]}
		  ]}
		]}).

-field({required,
		[required,
		 {type, integer},
		 {default, 100}
		]}).

-field({model_val,
		[
		 {type, integer}
		]}).

-field({string,
		[required,
		 {type, non_empty_binary},
		 {default, <<"string">>}
		]}).

-model([{validators,
		 [
		  fail_on_1,
		  {?MODULE, fail_on_2},
		  {fail_on, [3]},
		  {?MODULE, fail_on, [4]}
		 ]
		}]).

more_then(A, Val) ->
	case Val > A of
		true ->
			ok;
		false ->
			{error, {less_then, A}}
	end.

fail_on_1(Model) ->
	fail_on(1, Model).

fail_on_2(Model) ->
	fail_on(2, Model).

fail_on(A, Model) ->
	case Model:model_val() of
		A-> {error, A};
		_ -> ok
	end.
	

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

non_neg_integer_test_() ->
	Tests = [{-1, {error, [{non_neg_integer, {less_then, 0}}]}},
			 {0, ok},
			 {1, ok}
			],
	[fun() ->
			 {ok, Model} = from_proplist([{non_neg_integer, D}]),
			 ?assertEqual(Model:valid(), R)
	 end || {D, R} <- Tests].

more_then_10_test_() ->
	Tests = [{-1, {error, [{more_then_10, {less_then, 0}}]}},
			 {0, {error, [{more_then_10, {less_then, 10}}]}},
			 {1, {error, [{more_then_10, {less_then, 10}}]}},
			 {10, {error, [{more_then_10, {less_then, 10}}]}},
			 {100, ok}
			],
	[fun() ->
			 {ok, Model} = from_proplist([{more_then_10, D}]),
			 ?assertEqual(Model:valid(), R)
	 end || {D, R} <- Tests].

more_then_100_test_() ->
	Tests = [{-1, {error, [{more_then_100, {less_then, 0}}]}},
			 {0, {error, [{more_then_100, {less_then, 10}}]}},
			 {1, {error, [{more_then_100, {less_then, 10}}]}},
			 {10, {error, [{more_then_100, {less_then, 10}}]}},
			 {100, {error, [{more_then_100, {less_then, 100}}]}},
			 {1000, ok}
			],
	[fun() ->
			 {ok, Model} = from_proplist([{more_then_100, D}]),
			 ?assertEqual(Model:valid(), R)
	 end || {D, R} <- Tests].

required_test_() ->
	Tests = [{undefined, {error, [{required, required}]}},
			 {100, ok}],
	[fun() ->
			 {ok, Model} = from_proplist([{required, D}]),
			 ?assertEqual(Model:valid(), R)
	 end || {D, R} <- Tests].

empty_test_() ->
	Tests = [{<<"">>, {error, [{string, empty}]}},
			 {<<"test">>, ok}],
	[fun() ->
			 {ok, Model} = from_proplist([{string, D}]),
			 ?assertEqual(Model:valid(), R)
	 end || {D, R} <- Tests].

field_from_binary_test_() ->
	Tests = [
			 {<<"">>, more_then_10, {error, wrong_format}},
			 {<<"a">>, more_then_10, {error, wrong_format}},
			 {<<"-11">>, more_then_10, {error, {less_then, 0}}},
			 {<<"1">>, more_then_10, {error, {less_then, 10}}}
			],
	[fun() -> R = field_from_binary(F, D) end || {D, F, R} <- Tests].

model_test_() ->
	Tests = [{1, {error, 1}},
			 {2, {error, 2}},
			 {3, {error, 3}},
			 {4, {error, 4}},
			 {5, ok}],
	[fun() ->
			 {ok, Model} = from_proplist([{model_val, D}]),
			 ?assertEqual(Model:valid(), R)
	 end || {D, R} <- Tests].

-endif.
