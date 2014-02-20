-module(db_validators).
-compile({parse_transform, tq_record_transform}).

-export([more_than/2,
         fail_on_2/1,
         fail_on/2]).

%% Test

-field({non_neg_integer,
        [
         {type, non_neg_integer}
        ]}).

-field({more_than_10,
        [
         {type, non_neg_integer},
         {validators,
          [
           {more_than, [10]}
          ]}
        ]}).

-field({more_than_100,
        [
         {type, non_neg_integer},
         {validators,
          [
           {more_than, [10]},
           {?MODULE, more_than, [100]}
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

more_than(A, Val) ->
    case Val > A of
        true ->
            ok;
        false ->
            {error, {less_than, A}}
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
    Tests = [{-1, {error, [{non_neg_integer, {less_than, 0}}]}},
             {0, ok},
             {1, ok}
            ],
    [fun() ->
             {ok, Model} = from_proplist([{non_neg_integer, D}]),
             ?assertEqual(Model:valid(), R)
     end || {D, R} <- Tests].

more_than_10_test_() ->
    Tests = [{-1, {error, [{more_than_10, {less_than, 0}}]}},
             {0, {error, [{more_than_10, {less_than, 10}}]}},
             {1, {error, [{more_than_10, {less_than, 10}}]}},
             {10, {error, [{more_than_10, {less_than, 10}}]}},
             {100, ok}
            ],
    [fun() ->
             {ok, Model} = from_proplist([{more_than_10, D}]),
             ?assertEqual(Model:valid(), R)
     end || {D, R} <- Tests].

more_than_100_test_() ->
    Tests = [{-1, {error, [{more_than_100, {less_than, 0}}]}},
             {0, {error, [{more_than_100, {less_than, 10}}]}},
             {1, {error, [{more_than_100, {less_than, 10}}]}},
             {10, {error, [{more_than_100, {less_than, 10}}]}},
             {100, {error, [{more_than_100, {less_than, 100}}]}},
             {1000, ok}
            ],
    [fun() ->
             {ok, Model} = from_proplist([{more_than_100, D}]),
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

field_from_ext_test_() ->
    Tests = [
             {<<"">>, more_than_10, {error, wrong_format}},
             {<<"a">>, more_than_10, {error, wrong_format}},
             {<<"-11">>, more_than_10, {error, {less_than, 0}}},
             {<<"1">>, more_than_10, {error, {less_than, 10}}}
            ],
    [fun() -> R = field_from_ext(F, D) end || {D, F, R} <- Tests].

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
