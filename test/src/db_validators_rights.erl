-module(db_validators_rights).

-compile({parse_transform, tq_record_transform}).

-define(FIELD(Name),
        {Name,
         [
          {type, integer},
          {mode, Name},
          {validators, [{err, [Name]}]}
         ]}).

-field(?FIELD(r)).
-field(?FIELD(w)).
-field(?FIELD(rw)).
-field(?FIELD(sr)).
-field(?FIELD(sw)).
-field(?FIELD(srsw)).
-field(?FIELD(rsw)).
-field(?FIELD(srw)).

err(Tag, _Val) ->
    {error, Tag}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(FIELDS, [r, w, rw, sr, sw, srsw, rsw, srw]).

validator_existence_test_() ->
    Tests =
        [{F, ok} || F <- ?FIELDS],
    T = fun(F, R) ->
                fun() ->
                        R =
                            try
                                validator(F),
                                ok
                            catch _ ->
                                    error
                            end
                end
        end,
    [T(F, R) || {F, R} <- Tests].

valid_test() ->
    Proplist = lists:zip(?FIELDS, lists:seq(1, length(?FIELDS))),
    {ok, Model} = from_proplist(Proplist, [unsafe, ignore_unknown]),
    {error, Reason} = Model:valid(),
    R = [{F, F} || F <- [w, rw, sw, srsw, rsw, srw]],

    Reason2 = lists:keysort(1, Reason),
    R2 = lists:keysort(1, R),
    ?assertEqual(Reason2, R2).


-endif.
