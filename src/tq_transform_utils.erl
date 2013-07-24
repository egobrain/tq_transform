%% Copyright (c) 2011-2013, Jakov Kozlov <xazar.studio@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(tq_transform_utils).

-export([error_writer_foldl/3,
		 error_writer_map/2
		]).

-export([valid/1]).

-export([to_integer/1,
		 to_float/1]).

-export([more/2,
		 more_or_eq/2,
		 less/2,
		 less_or_eq/2,
		 non_empty_binary/1]).

-spec error_writer_foldl(Fun, State, List) -> {ok, NewState} | {error, Reasons} when
	  List :: [Elem],
	  Fun :: fun((Elem, State) -> {ok, NewState} | {error, Reason}),
	  Reasons :: [Reason].
error_writer_foldl(Fun, InitState, Opts) ->
	{ResultState, ResultErrors} =
		lists:foldl(fun(Val, {State, Errors}) ->
							case Fun(Val, State) of
								{ok, State2} ->
									{State2, Errors};
								{error, Reason} ->
									{State, [Reason | Errors]}
						   end
				   end,
				   {InitState, []},
				   Opts),
	case ResultErrors of
		[] -> {ok, ResultState};
		_ -> {error, lists:reverse(ResultErrors)}
	end.

-spec error_writer_map(Fun, ArgsList) -> {ok, ResultList} | {error, Errors} when
	  Fun :: fun((Arg) -> {ok, Result} | {error, Error}),
	  ArgsList :: [Arg],
	  ResultList :: [Result],
	  Errors :: [Error].
error_writer_map(Fun, List) when is_list(List) ->
	MapFun = fun(Item, Acc) ->
					 case Fun(Item) of
						 {ok, Res} ->
							 {ok, [Res | Acc]};
						 {error, Reason} ->
							 {error, Reason}
					 end
			 end,
	case error_writer_foldl(MapFun, [], List) of
		{ok, Result} ->
			{ok, lists:reverse(Result)};
		{error, _} = Err -> Err
	end.

-spec valid(ListToValid) -> ok | {error, Reasons} when
	  ListToValid :: [{Field, Validator, Value}],
	  Validator :: fun((Value) -> ok | {error, Reason}),
	  Reasons :: [{Field, Reason}].
valid(List) ->
	Res = error_writer_foldl(fun({Field, Validator, Value}, State) ->
							   case Validator(Value) of
								   ok -> {ok, State};
								   {error, Reason} -> {error, {Field, Reason}}
							   end
					   end, ok, List),
	case Res of
		{ok, ok} -> ok;
		{error, _Reason} = Err ->
			Err
	end.

%% Converters

to_integer(Int) when is_integer(Int) ->
	{ok, Int};
to_integer(Bin) when is_binary(Bin) ->
	case string:to_integer(binary_to_list(Bin)) of
		{Res, []} -> {ok, Res};
		_ -> {error, wrong_format}
	end.

to_float(Float) when is_float(Float) ->
	Float;
to_float(Bin) when is_binary(Bin) ->
	case string:to_float(binary_to_list(Bin)) of
		{Res, []} -> {ok, Res};
		_ -> to_integer({error, wrong_format})
	end.

%% Default validators

more(A, Val) when Val > A -> ok;
more(A, _Val) -> {error, {less_then, A}}.

more_or_eq(A, Val) when Val >= A -> ok;
more_or_eq(A, _Val) -> {error, {less_then, A}}.

less(A, Val) when Val < A -> ok;
less(A, _Val) -> {error, {more_then, A}}.

less_or_eq(A, Val) when Val =< A -> ok;
less_or_eq(A, _Val) -> {error, {more_then, A}}.

non_empty_binary(<<"">>) -> {error, empty};
non_empty_binary(_Val) -> ok.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

error_writer_foldl_test_() ->
	Sum = fun (Int, Acc) when is_integer(Int) -> {ok, Acc+Int};
			  (Other, _Acc) -> {error, {not_integer, Other}}
		  end,
	Tests = [
			 {[1, 2, 3, 4], {ok, 10}},
			 {[1, 2, 3, e1, 4, 5, e2], {error, [{not_integer, e1}, {not_integer, e2}]}},
			 {[1, 2, 3, error], {error, [{not_integer, error}]}},
			 {[], {ok, 0}},
			 {[error], {error, [{not_integer, error}]}},
			 {[1], {ok, 1}}
			],
	F = fun(D, R) -> R = error_writer_foldl(Sum, 0, D) end,
	[fun() -> F(From, To) end || {From, To} <- Tests].

error_writer_map_test_() ->
	Sum = fun (Int) when is_integer(Int) -> {ok, Int+10};
			  (Other) -> {error, {not_integer, Other}}
		  end,
	Tests = [
			 {[1, 2, 3, 4], {ok, [11, 12, 13, 14]}},
			 {[1, 2, 3, e1, 4, 5, e2], {error, [{not_integer, e1}, {not_integer, e2}]}},
			 {[1, 2, 3, error], {error, [{not_integer, error}]}},
			 {[], {ok, []}},
			 {[error], {error, [{not_integer, error}]}},
			 {[1], {ok, [11]}}
			],
	F = fun(D, R) -> R = error_writer_map(Sum, D) end,
	[fun() -> F(From, To) end || {From, To} <- Tests].

-endif.
