-module(tq_converter).

%% External api
-export([
         to_boolean/1,
         to_binary/1,
         to_integer/1,
         to_float/1,
         to_date/1,
         to_time/1,
         to_datetime/1
        ]).

%% =============================================================================
%%% Internal functions
%% =============================================================================

to_binary(Bin) when is_binary(Bin) ->
    {ok, Bin};
to_binary(_) ->
    {error, wrong_format}.

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
    Re =
        "(?<y>\\d{4})-(?<m>\\d{1,2})-(?<d>\\d{1,2})[Tt](?<hh>\\d{1,2})"
        ":(?<mm>\\d{1,2}):(?<ss>\\d{1,2})(?<ms>\.\\d{1,}){0,1}"
        "(?<offset>[Zz]|[+-]\\d{2}:\\d{2})",
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
                               SecOffset =
                                   binary_to_integer(OffsetH) * 60 * 60+
                                   binary_to_integer(OffsetM) * 60,
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

%% =============================================================================
%%% Tests
%% =============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

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
