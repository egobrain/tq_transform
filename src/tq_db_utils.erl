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

-module(tq_db_utils).

-export([error_writer_foldl/3]).

-export([valid/1]).

-export([binary_to_integer/1,
		 binary_to_float/1]).

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

binary_to_integer(Bin) when is_binary(Bin) ->
	case string:to_integer(binary_to_list(Bin)) of
		{Res, []} -> {ok, Res};
		_ -> {error, wrong_format}
	end.

binary_to_float(Bin) when is_binary(Bin) ->
	case string:to_float(binary_to_list(Bin)) of
		{Res, []} -> {ok, Res};
		_ -> {error, wrong_format}
	end.

valid(List) ->
	error_writer_foldl(fun({Field, Validator, Value}, State) ->
							   case Validator(Value) of
								   ok -> {ok, State};
								   {error, Reason} -> {error, {Reason, Field}}
							   end
					   end, ok, List).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

error_writer_foldl_test_() ->
	Sum = fun (Int, Acc) when is_integer(Int) -> {ok, Acc+Int};
			  (Other, _Acc) -> {error, {not_integer, Other}}
		  end,
	Tests = [
			 {[1,2,3,4], {ok, 10}},
			 {[1,2,3, e1, 4,5, e2], {error, [{not_integer, e2}, {not_integer, e1}]}},
			 {[1,2,3,error], {error, [{not_integer, error}]}},
			 {[], {ok, 0}},
			 {[error], {error, [{not_integer, error}]}},
			 {[1], {ok, 1}}
			],
	F = fun(D, R) -> R = error_writer_foldl(Sum, 0, D) end,
	[fun() -> F(From, To) end || {From, To} <- Tests].

-endif.
