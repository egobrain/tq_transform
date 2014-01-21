-module(db_access).

-compile({parse_transform, tq_record_transform}).

%% Test
-field({r,[{type, integer}, {mode, r}]}).
-field({w,[{type, integer}, {mode, w}]}).
-field({rw,   [{type, integer}, {mode, rw}]}).
-field({sr,   [{type, integer}, {mode, sr}]}).
-field({sw,   [{type, integer}, {mode, sw}]}).
-field({srsw, [{type, integer}, {mode, srsw}]}).
-field({rsw,  [{type, integer}, {mode, rsw}]}).
-field({srw,  [{type, integer}, {mode, srw}]}).

-field({r_default,[{type, integer}, {default, 100}, {mode, r}]}).
-field({w_default,[{type, integer}, {default, 200}, {mode, w}]}).
-field({rw_default,   [{type, integer}, {default, 300}, {mode, rw}]}).
-field({sr_default,   [{type, integer}, {default, 400}, {mode, sr}]}).
-field({sw_default,   [{type, integer}, {default, 500}, {mode, sw}]}).
-field({srsw_default, [{type, integer}, {default, 600}, {mode, srsw}]}).
-field({rsw_default,  [{type, integer}, {default, 700}, {mode, rsw}]}).
-field({srw_default,  [{type, integer}, {default, 800}, {mode, srw}]}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(Opts, [r, w, rw, sr, sw, srsw, rsw, srw]).

write_only_stumb_test_() ->
    Model = new(),
    Tests = [
             {{w, #?MODULE.w}, undefined},
             {{sw, #?MODULE.sw}, undefined},
             {{w_default, #?MODULE.w_default}, 200},
             {{sw_default, #?MODULE.sw_default}, 500}
            ],
    [{atom_to_list(F), fun() -> R = element(Id, Model) end} || {{F, Id}, R} <- Tests].

from_proplist_test_() ->
    Data = [{Opt, 1} || Opt <- ?Opts],
    [{"unsafe", fun() ->
                        ?assertEqual({error, [{r, unknown},
                                              {sr, unknown}]},
                                     from_proplist(Data, [unsafe], new()))
                end},
     {"safe", fun() ->
                      ?assertEqual({error, [{r, unknown},
                                            {sr, unknown},
                                            {sw, unknown},
                                            {srsw, unknown},
                                            {rsw, unknown}]},
                                   from_proplist(Data, [], new()))
              end},
     {"ignore_unknown", fun() ->
                                {ok, _} = from_proplist(Data, [ignore_unknown], new())
                        end}
    ].

from_ext_proplist_test_() ->
    Data = [{list_to_binary(atom_to_list(Opt)), <<"1">>} || Opt <- ?Opts],
    [{"unsafe", fun() ->
                        ?assertEqual({error, [{<<"r">>, unknown},
                                              {<<"sr">>, unknown}]},
                                     from_ext_proplist(Data, [unsafe], new()))
                end},
     {"safe", fun() ->
                      ?assertEqual({error, [{<<"r">>, unknown},
                                            {<<"sr">>, unknown},
                                            {<<"sw">>, unknown},
                                            {<<"srsw">>, unknown},
                                            {<<"rsw">>, unknown}]},
                                   from_ext_proplist(Data, [], new()))
              end},
     {"ignore_unknown", fun() ->
                                {ok, _} = from_ext_proplist(Data, [ignore_unknown], new())
                        end}
    ].

test_model() ->
    WOpts = [%% {r, 1},
             {w, 2},
             {rw, 3},
             %% {sr, 4},
             {sw, 5},
             {srsw, 6},
             {rsw, 7},
             {srw, 8},
             %% {r_default, 9},
             {w_default, 10},
             {rw_default, 11},
             %% {sr_default, 12},
             {sw_default, 13},
             {srsw_default, 14},
             {rsw_default, 15},
             {srw_default, 16}],
    from_proplist(WOpts).

to_proplist_test() ->
    {ok, Model} = test_model(),
    ROpts = [{r, undefined}, %% field is read only
             %% {w, 2},
             {rw, 3},
             {sr, undefined}, %% field is read only
             %% {sw, 5},
             {srsw, 6},
             {rsw, 7},
             {srw, 8},
             {r_default, 100}, %% field is read only
             %% {w_default, 10},
             {rw_default, 11},
             {sr_default, 400}, %% field is read only
             %% {sw_default, 13},
             {srsw_default, 14},
             {rsw_default, 15},
             {srw_default, 16}],

    Proplist = to_proplist([unsafe], Model),
    ?assertEqual(lists:keysort(1, ROpts), lists:keysort(1, Proplist)).

fields_tests() ->
    [
     {[r], {ok, [{r, undefined}]}, [safe]},
     {[r], {ok, [{r, undefined}]}, [unsafe]},
     {[w], {error, [{w, forbidden}]}, [safe]},
     {[w], {error, [{w, forbidden}]}, [unsafe]},
     {[rw], {ok, [{rw, 3}]}, [safe]},
     {[rw], {ok, [{rw, 3}]}, [unsafe]},
     {[sr], {error, [{sr, unknown}]}, [safe]},
     {[sr], {ok, [{sr, undefined}]}, [unsafe]},
     {[sw], {error, [{sw, unknown}]}, [safe]},
     {[sw], {error, [{sw, forbidden}]}, [unsafe]},
     {[srsw], {error, [{srsw, unknown}]}, [safe]},
     {[srsw], {ok, [{srsw, 6}]}, [unsafe]},
     {[rsw], {ok, [{rsw, 7}]}, [safe]},
     {[rsw], {ok, [{rsw, 7}]}, [unsafe]},
     {[srw], {error, [{srw, forbidden}]}, [safe]},
     {[srw], {ok, [{srw, 8}]}, [unsafe]},
     {[unknown_field], {error, [{unknown_field, unknown}]}, [safe]},
     {[unknown_field], {error, [{unknown_field, unknown}]}, [unsafe]},

     {[unknown_field], {ok, []}, [safe, ignore_unknown]},
     {[unknown_field], {ok, []}, [unsafe, ignore_unknown]},

     {[r, rw], {ok, [{r, undefined}, {rw, 3}]}, [safe]},
     {[r, rw], {ok, [{r, undefined}, {rw, 3}]}, [unsafe]},
     {[r, srw], {error, [{srw, forbidden}]}, [safe]},
     {[r, srw], {ok, [{r, undefined}, {srw, 8}]}, [unsafe]}
    ].

binary_fields_tests() ->
    [{[list_to_binary(atom_to_list(F)) || F <- Fields],
      case R of
          {error, Reasons} ->
              {error, [{list_to_binary(atom_to_list(F)), E} || {F, E} <- Reasons]};
          _ ->
              R
      end,
      Opts ++ [binary_key]}
     || {Fields, R, Opts} <- fields_tests()].

test_fields_function(Tests, Fun) ->
    {ok, Model} = test_model(),
    [fun() ->
             ?assertEqual(R, Fun(F, Opts, Model))
     end || {F, R, Opts} <- Tests].

fields_test_() ->
    test_fields_function(fields_tests(), fun fields/3).

ext_fields_test_() ->
    test_fields_function(fields_tests(), fun ext_fields/3).

binary_fields_test_() ->
    test_fields_function(binary_fields_tests(), fun fields/3).

ext_binary_fields_test_() ->
    test_fields_function(binary_fields_tests(), fun ext_fields/3).

-endif.
