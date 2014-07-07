-module(db_converter).

-compile({parse_transform, tq_record_transform}).

-field({date,
        [
         {type, date},
         {validators, [date_validator]}
        ]}).

-field({int,
        [
         {default, 1},
         {type, non_neg_integer},
         required
        ]}).

date_validator({_, _, _}) -> ok;
date_validator(_) -> {error, wrong_date}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

null_test() ->
    M = ?MODULE:new(),

    %% Check that validators are ignored for null value
    ok = M:valid(),
    {error, [{date, wrong_date}]} = (M:set_date(123)):valid(),

    {ok, _} = M:from_ext_proplist([{<<"date">>, null}]),
    {error, [{date, wrong_format}]} = M:from_ext_proplist([{<<"date">>, <<"wrong_date">>}]),

    {ok, null} = field_from_ext(date, null),
    {error, wrong_format} = field_from_ext(date, <<"wrong_date">>),

    Date = {2014, 01, 13},
    [_|_] = (M:set_date(Date)):to_ext_proplist(),
    _ = field_to_ext(date, Date),

    {error, [{int, required}]} = (M:set_int(null)):valid(),
    {error, [{int, required}]} = M:from_ext_proplist([{<<"int">>, null}]),
    {error, required} = field_from_ext(int, null).

-endif.
