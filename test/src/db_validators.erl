-module(db_validators).
-compile({parse_transform, tq_record_transform}).

-export([more_then/2,
		 fail_on_2/1,
		 fail_on/2]).

%% Test

-field({non_neg_integer,
		[
		 {type, non_neg_integer},
		 {type_constructor, any_type_constructor},
		 {validators,
		  [
		   non_neg_integer_validator
		  ]}
		]}).

-field({more_then_10,
		[
		 {type, non_neg_integer},
		 {type_constructor, any_type_constructor},
		 {validators,
		  [
		   non_neg_integer_validator,
		   {more_then, [10]}
		  ]}
		]}).

-field({more_then_100,
		[
		 {type, non_neg_integer},
		 {type_constructor, any_type_constructor},
		 {validators,
		  [
		   non_neg_integer_validator,
		   {more_then, [10]},
		   {?MODULE, more_then, [100]}
		  ]}
		]}).

-field({required,
		[required,
		 {type, integer}
		]}).

-field({model_val,
		[
		 {type, integer}
		]}).

-model([{validators,
		 [
		  fail_on_1,
		  {?MODULE, fail_on_2},
		  {fail_on, [3]},
		  {?MODULE, fail_on, [4]}
		 ]
		}]).

any_type_constructor(A) -> A.	

non_neg_integer_validator(Val) when Val >= 0 ->
	ok;
non_neg_integer_validator(_) ->
	{error, negative}.

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

-define(ReqModel, element(2, from_proplist([{required, 10}]))).

non_neg_integer_test_() ->
	Tests = [{-1, {error, [{non_neg_integer, negative}]}},
			 {0, ok},
			 {1, ok}
			],
	[fun() ->
			 {ok, Model} = from_proplist([{non_neg_integer, D}], ?ReqModel),
			 ?assertEqual(Model:valid(), R)
	 end || {D, R} <- Tests].

more_then_10_test_() ->
	Tests = [{-1, {error, [{more_then_10, negative}]}},
			 {0, {error, [{more_then_10, {less_then, 10}}]}},
			 {1, {error, [{more_then_10, {less_then, 10}}]}},
			 {10, {error, [{more_then_10, {less_then, 10}}]}},
			 {100, ok}
			],
	[fun() ->
			 {ok, Model} = from_proplist([{more_then_10, D}], ?ReqModel),
			 ?assertEqual(Model:valid(), R)
	 end || {D, R} <- Tests].

more_then_100_test_() ->
	Tests = [{-1, {error, [{more_then_100, negative}]}},
			 {0, {error, [{more_then_100, {less_then, 10}}]}},
			 {1, {error, [{more_then_100, {less_then, 10}}]}},
			 {10, {error, [{more_then_100, {less_then, 10}}]}},
			 {100, {error, [{more_then_100, {less_then, 100}}]}},
			 {1000, ok}
			],
	[fun() ->
			 {ok, Model} = from_proplist([{more_then_100, D}], ?ReqModel),
			 ?assertEqual(Model:valid(), R)
	 end || {D, R} <- Tests].

required_test_() ->
	Tests = [{new(), {error, [{required, required}]}},
			 {?ReqModel, ok}],
	[fun() ->
			 ?assertEqual(Model:valid(), R)
	 end || {Model, R} <- Tests].


model_test_() ->
	Tests = [{1, {error, 1}},
			 {2, {error, 2}},
			 {3, {error, 3}},
			 {4, {error, 4}},
			 {5, ok}],
	[fun() ->
			 {ok, Model} = from_proplist([{model_val, D}], ?ReqModel),
			 ?assertEqual(Model:valid(), R)
	 end || {D, R} <- Tests].

-endif.
