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

-export([mode_to_acl/1,
         check_acl/2
        ]).

-export([error_writer_foldl/3,
         error_writer_map/2,
         error_map_funs/2
        ]).

-export([valid/1]).

-export([to_integer/1,
         to_float/1,
         to_boolean/1,
         to_date/1,
         to_time/1,
         to_datetime/1
        ]).

-export([bin_to_integer/1,
         bin_to_float/1,
         bin_to_boolean/1,
         bin_to_date/1, bin_to_date/2,
         bin_to_time/1, bin_to_time/2,
         bin_to_datetime/1, bin_to_datetime/2
        ]).

-export([more/2,
         more_or_eq/2,
         less/2,
         less_or_eq/2,
         non_empty_binary/1]).

-export([print_module/1,
         pretty_print/1]).

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

%% Converters

to_integer(Int) when is_integer(Int) ->
    {ok, Int};
to_integer(Bin) when is_binary(Bin)  ->
    bin_to_integer(Bin);
to_integer(_) ->
    {error, wrong_format}.

bin_to_integer(Bin) ->
    case catch binary_to_integer(Bin) of
        Int when is_integer(Int) -> {ok, Int};
        _ -> {error, wrong_format}
    end.

to_float(Int) when is_integer(Int) ->
    {ok, Int*1.0};
to_float(Float) when is_float(Float) ->
    {ok, Float};
to_float(Bin) when is_binary(Bin) ->
    bin_to_float(Bin);
to_float(_) ->
    {error, wrong_format}.

bin_to_float(Bin) ->
    case catch binary_to_float(Bin) of
        Float when is_float(Float) -> {ok, Float};
        _ ->
            case bin_to_integer(Bin) of
                {ok, Int} ->
                    {ok, Int * 1.0}; % Cast to float
                Err ->
                    Err
            end
    end.

to_boolean(Bool) when is_boolean(Bool) ->
    {ok, Bool};
to_boolean(Bin) when is_binary(Bin) ->
    bin_to_boolean(Bin);
to_boolean(_) ->
    {error, wrong_format}.

bin_to_boolean(<<T,R,U,E>>) when
      (T =:= $t orelse T =:= $T) andalso
      (R =:= $r orelse R =:= $R) andalso
      (U =:= $u orelse U =:= $U) andalso
      (E =:= $e orelse E =:= $E) ->
    {ok, true};
bin_to_boolean(<<F,A,L,S,E>>) when
      (F =:= $f orelse F =:= $F) andalso
      (A =:= $a orelse A =:= $A) andalso
      (L =:= $l orelse L =:= $L) andalso
      (S =:= $s orelse S =:= $S) andalso
      (E =:= $e orelse E =:= $E) ->
    {ok, false};
bin_to_boolean(_) ->
    {error, wrong_format}.

to_date({Y, M, D} = Date) when is_integer(Y), is_integer(M), is_integer(D) ->
    case calendar:valid_date(Date) of
        false ->
            {error, invalid_date};
        true ->
            {ok, Date}
    end;
to_date(Bin) when is_binary(Bin) ->
    bin_to_date(Bin);
to_date(_) ->
    {error, wrong_format}.

bin_to_date(Bin) ->
    Re = "(?<y>\\d{4})-(?<m>\\d{1,2})-(?<d>\\d{1,2})",
    bin_to_date(Re, Bin).
bin_to_date(Re, Bin) when is_binary(Bin) ->
    case re:run(Bin, Re, [{capture, [y, m, d], binary}]) of
        {match, List} ->
            Date = list_to_tuple([binary_to_integer(E) || E <- List]),
            case calendar:valid_date(Date) of
                false ->
                    {error, invalid_date};
                true ->
                    {ok, Date}
            end;
        _ ->
            {error, wrong_format}
    end.

to_time({Hh, Mm, Ss}=Time) when is_integer(Hh), is_integer(Mm), is_integer(Ss) ->
    case valid_time(Time) of
        false ->
            {error, invalid_time};
        true ->
            {ok, Time}
    end;
to_time(Bin) when is_binary(Bin) ->
    bin_to_time(Bin);
to_time(_) ->
    {error, wrong_format}.

bin_to_time(Bin) ->
    Re = "(?<hh>\\d{1,2}):(?<mm>\\d{1,2}):(?<ss>\\d{1,2})",
    bin_to_time(Re, Bin).
bin_to_time(Re, Bin) when is_binary(Bin) ->
    case re:run(Bin, Re, [{capture, [hh, mm, ss], binary}]) of
        {match, List} ->
            Time = list_to_tuple([binary_to_integer(E) || E <- List]),
            case valid_time(Time) of
                false ->
                    {error, invalid_time};
                true ->
                    {ok, Time}
            end;
        _ ->
            {error, wrong_format}
    end.

to_datetime({{Y, M, D}=Date, {Hh, Mm, Ss}=Time}=DateTime) when
      is_integer(Y), is_integer(M), is_integer(D),
      is_integer(Hh), is_integer(Mm), is_integer(Ss) ->
    case calendar:valid_date(Date) of
        true ->
            case valid_time(Time) of
                false ->
                    {error, invalid_time};
                true ->
                    {ok, DateTime}
            end;
        false ->
            {error, invalid_date}
    end;
to_datetime(Bin) when is_binary(Bin) ->
    bin_to_datetime(Bin);
to_datetime(_) ->
    {error, wrong_format}.

bin_to_datetime(Bin) ->
    Re = "(?<y>\\d{4})-(?<m>\\d{1,2})-(?<d>\\d{1,2})[Tt](?<hh>\\d{1,2}):(?<mm>\\d{1,2}):(?<ss>\\d{1,2})(?<ms>\.\\d{1,}){0,1}(?<offset>[Zz]|[+-]\\d{2}:\\d{2})",
    bin_to_datetime(Re, Bin).
bin_to_datetime(Re, Bin) ->
    case re:run(Bin, Re, [{capture, [y, m, d, hh, mm, ss, ms, offset], binary}]) of
        {match, [Y, M, D, Hh, Mm, Ss, _Ms, Offset]} ->
            TimeDiff = case Offset of
                           _ when Offset =:= <<"z">>; Offset =:= <<"Z">> ->
                               0;
                           <<>> ->
                               0;
                           <<Sign, OffsetH:2/binary, ":", OffsetM:2/binary>> ->
                               SecOffset = binary_to_integer(OffsetH)*60*60+binary_to_integer(OffsetM)*60,
                               case Sign of
                                   $+ -> -SecOffset;
                                   $- -> SecOffset
                               end
                       end,
            Date = {binary_to_integer(Y),
                    binary_to_integer(M),
                    binary_to_integer(D)},

            Time = {binary_to_integer(Hh),
                    binary_to_integer(Mm),
                    binary_to_integer(Ss)},

            case calendar:valid_date(Date) of
                true ->
                    case valid_time(Time) of
                        false ->
                            {error, invalid_time};
                        true ->
                            DateTime0 = calendar:datetime_to_gregorian_seconds({Date, Time}) + TimeDiff,
                            DateTime = calendar:gregorian_seconds_to_datetime(DateTime0),
                            {ok, DateTime}
                    end;
                false ->
                    {error, invalid_date}
            end;
        _ ->
            {error, wrong_format}
    end.

valid_time({Hh, Mm, Ss}) ->
    case Hh < 0 orelse Hh >= 24 orelse
        Mm < 0 orelse Mm >= 60 orelse
        Ss < 0 orelse Ss >= 60
    of
        true ->
            false;
        false ->
            true
    end.

%% Default validators

more(A, Val) when Val > A -> ok;
more(A, _Val) -> {error, {less_than, A}}.

more_or_eq(A, Val) when Val >= A -> ok;
more_or_eq(A, _Val) -> {error, {less_than, A}}.

less(A, Val) when Val < A -> ok;
less(A, _Val) -> {error, {more_than, A}}.

less_or_eq(A, Val) when Val =< A -> ok;
less_or_eq(A, _Val) -> {error, {more_than, A}}.

non_empty_binary(<<"">>) -> {error, empty};
non_empty_binary(_Val) -> ok.


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

valid_time_test_() ->
    Tests = [
             {{0,0,0}, true},
             {{23,59,59}, true},
             {{24,0,0}, false},
             {{0,60,0}, false},
             {{0,0,60}, false},
             {{-1, 0, 0}, false},
             {{0, -1, 0}, false},
             {{0, 0, -1}, false}
            ],
    [fun() -> To = valid_time(From) end || {From, To} <- Tests].

bin_to_integer_test_() ->
    Tests = [
             {<<"1">>, {ok, 1}},
             {<<"-1">>, {ok, -1}},
             {<<"0">>, {ok, 0}},
             {<<"123">>, {ok, 123}},
             {<<"a123">>, {error, wrong_format}}
            ],
    [fun() -> To = bin_to_integer(From) end || {From, To} <- Tests].


bin_to_float_test_() ->
    Tests = [
             {<<"1">>, {ok, 1.0}},
             {<<"-1">>, {ok, -1.0}},
             {<<"0">>, {ok, 0.0}},
             {<<"123">>, {ok, 123.0}},
             {<<"1.2">>, {ok, 1.2}},
             {<<"-1.2">>, {ok, -1.2}},
             {<<"0.1">>, {ok, 0.1}},
             {<<"123.45">>, {ok, 123.45}},
             {<<"a123.456">>, {error, wrong_format}}
            ],
    [fun() -> To = bin_to_float(From) end || {From, To} <- Tests].

bin_to_boolean_test_() ->
    TrueData = [<<T,R,U,E>> || T <- [$t, $T], R <- [$r, $R], U <- [$u, $U], E <- [$e, $E]],
    FalseData = [<<F,A,L,S,E>> || F <- [$f, $F], A <- [$a, $A], L <- [$l, $L], S <- [$s, $S], E <- [$e, $E]],
    EData = [<<" true">>, <<"false ">>, <<"wrong data">>],
    Tests =
        [{T, {ok, true}} || T <- TrueData] ++
        [{F, {ok, false}} || F <- FalseData] ++
        [{E, {error, wrong_format}} || E <- EData],
    [fun() -> R = bin_to_boolean(D) end || {D, R} <- Tests].


bin_to_date_test_() ->
    Tests = [
             {<<"2001-1-1">>, {ok, {2001, 1, 1}}},
             {<<"2999-12-31">>, {ok, {2999, 12, 31}}},
             {<<"2999-12-31">>, {ok, {2999, 12, 31}}},
             {<<"2001-13-01">>, {error, invalid_date}},
             {<<"01-13-01">>, {error, wrong_format}},
             {<<"2001/13/01">>, {error, wrong_format}}
            ],
    [fun() -> To = bin_to_date(From) end || {From, To} <- Tests].


bin_to_time_test_() ->
    Tests = [
             {<<"0:0:0">>, {ok, {0,0,0}}},
             {<<"00:00:00">>, {ok, {0,0,0}}},
             {<<"01:02:03">>, {ok, {1,2,3}}},
             {<<"24:0:0">>, {error, invalid_time}},
             {<<"0::0:0">>, {error, wrong_format}}
            ],
    [fun() -> To = bin_to_time(From) end || {From, To} <- Tests].

bin_to_datetime_test_() ->
    Tests = [
             {<<"2001-01-01T00:00:00z">>, {ok, {{2001, 01, 01}, {0, 0, 0}}}},
             {<<"2001-01-01T18:50:00-04:00">>, {ok, {{2001, 01, 01}, {22, 50, 0}}}},
             {<<"2001-12-31T22:50:00-04:00">>, {ok, {{2002, 01, 01}, {2, 50, 0}}}},
             {<<"2001-01-01T22:50:00+04:00">>, {ok, {{2001, 01, 01}, {18, 50, 0}}}},
             {<<"2002-01-01T2:50:00+04:00">>, {ok, {{2001, 12, 31}, {22, 50, 0}}}},
             {<<"2999-12-31t00:00:00.001z">>, {ok, {{2999, 12, 31}, {0, 0, 0}}}},
             {<<"2999-12-31T01:02:03z">>, {ok, {{2999, 12, 31}, {1, 2, 3}}}},
             {<<"2001-13-01T00:00:00Z">>, {error, invalid_date}},
             {<<"2001-12-01T24:00:00Z">>, {error, invalid_time}},
             {<<"01-13-01 0:0:0">>, {error, wrong_format}},
             {<<"2001/12/01 0:0:0">>, {error, wrong_format}},
             {<<"2001-12-1  0:0:0">>, {error, wrong_format}}
            ],
    [fun() -> To = bin_to_datetime(From) end || {From, To} <- Tests].

-endif.
