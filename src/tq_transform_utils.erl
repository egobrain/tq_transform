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

-include("include/access_mode.hrl").

-export([
         mode_to_acl/1,
         check_acl/2
        ]).

-export([
         error_writer_foldl/3,
         error_writer_map/2,
         error_map_funs/2
        ]).

-export([
         valid/1
        ]).

-export([
         print_module/1,
         pretty_print/1
        ]).

-spec mode_to_acl(Mode) -> #access_mode{} when
      Mode :: r | w | rw | sr | sw | srsw | rsw | srw.
mode_to_acl(r)    -> #access_mode{r=true,  sr=true,  w=false, sw=false};
mode_to_acl(w)    -> #access_mode{r=false, sr=false, w=true,  sw=true};
mode_to_acl(rw)   -> #access_mode{r=true,  sr=true,  w=true,  sw=true};
mode_to_acl(sr)   -> #access_mode{r=false, sr=true,  w=false, sw=false};
mode_to_acl(sw)   -> #access_mode{r=false, sr=false, w=false, sw=true};
mode_to_acl(srsw) -> #access_mode{r=false, sr=true,  w=false, sw=true};
mode_to_acl(rsw)  -> #access_mode{r=true,  sr=true,  w=false, sw=true};
mode_to_acl(srw)  -> #access_mode{r=false, sr=true,  w=true,  sw=true}.

-spec check_acl(FieldAcl, AccessAcl) -> boolean() when
      FieldAcl :: Acl,
      AccessAcl :: Acl,
      Acl :: #access_mode{}.
check_acl(FieldAcl, AccessAcl) ->
    [_|L1] = tuple_to_list(FieldAcl),
    [_|L2] = tuple_to_list(AccessAcl),
    List = lists:zipwith(fun(_, false) -> true;
                            (A1, true) -> A1
                         end, L1, L2),
    lists:foldl(fun(A1, A2) -> A1 andalso A2 end, true, List).

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
                         ok ->
                             {ok, Acc};
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

-spec error_map_funs(Funs, State) -> {ok, State} | {error, Reason} when
      Funs :: [Fun],
      Fun :: fun((State) -> {ok, State} | {error, Reason}).
error_map_funs([], State) ->
    {ok, State};
error_map_funs([Fun|Rest], State) ->
    case Fun(State) of
        {ok, State2} ->
            error_map_funs(Rest, State2);
        {error, _Reason} = Err ->
            Err
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

%% Pretty print

pretty_print(Forms0) ->
    Forms = epp:restore_typed_record_fields(revert(Forms0)),
    [io_lib:fwrite("~s~n",
                   [lists:flatten([erl_pp:form(Fm) ||
                                      Fm <- Forms])])].

revert(Tree) ->
    [erl_syntax:revert(T) || T <- lists:flatten(Tree)].

print_module(Module) ->
    BeamFileName = code:which(Module),
    case beam_lib:chunks(BeamFileName, [abstract_code]) of
        {ok, {_, [{abstract_code, {raw_abstract_v1,Forms}}]}} ->
            Code = pretty_print(Forms),
            io:format("~s~n", [Code]);
        Error ->
            Error
    end.

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

error_map_funs_test_() ->
    Succ = fun(A) -> {ok, A+1} end,
    Err = fun(A) -> {error, A} end,
    Tests = [
             {[], {ok, 0}},
             {[Succ], {ok, 1}},
             {[Succ, Succ], {ok, 2}},
             {[Succ, Succ, Succ], {ok, 3}},
             {[Err], {error, 0}},
             {[Err, Succ, Succ], {error, 0}},
             {[Succ, Err, Succ], {error, 1}},
             {[Succ, Succ, Err], {error, 2}}
            ],
    [fun() -> R = error_map_funs(Funs, 0) end || {Funs, R} <- Tests].

-endif.
