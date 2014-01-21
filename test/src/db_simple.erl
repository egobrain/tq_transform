-module(db_simple).
-compile({parse_transform, tq_record_transform}).

%% Test

-field({id,
        [
         {type, integer},
         {to_ext, {tag, [id]}}
        ]}).
-field({name,
        [
         required,
         {type, binary},
         {to_ext, {tag, [name]}},
         record, get, set,
         {default, <<"Default name">>},
         {mode, rw}
        ]}).

-field({custom_in_record,
        [
         {type, integer},
         {to_ext, {tag, [custom_in_record]}},
         {record, true},
         {get, custom},
         {set, custom}
        ]}).

-field({custom_not_in_record,
        [
         {type, integer},
         {to_ext, {tag, [custom_not_in_record]}},
         {record, false},
         {get, custom},
         {set, custom}
        ]}).

tag(Tag, Value) ->
    {Tag, Value}.

set_custom_in_record(V, Model) ->
    Model2 = Model#?MODULE{custom_in_record = V},
    Model2.

custom_in_record(Model) ->
    Model#?MODULE.custom_in_record.

set_custom_not_in_record(V, Model) ->
    put(test, V),
    Model.

custom_not_in_record(_Model) ->
    get(test).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(join(A, B), list_to_atom(atom_to_list(A)++"_"++atom_to_list(B))).
-define(prefix_set(A), ?join(set, A)).

getter_setters_test_() ->
    Model = new(),
    Tests = [{id, 1},
             {name, 2},
             {custom_in_record, 3},
             {custom_not_in_record, 4}],
    [{atom_to_list(F), fun() ->
                               SF = ?prefix_set(F),
                               Model2 = Model:SF(V),
                               V = Model2:F()
                       end} || {F, V} <- Tests].

proplist_test() ->
    Proplist = lists:keysort(1, [{id, 1},
                                 {name, <<"test">>},
                                 {custom_in_record, 10},
                                 {custom_not_in_record, 20}]),

    {ok, Model} = from_proplist(Proplist),
    Proplist = lists:keysort(1, Model:to_proplist()).

from_ext_proplist_test() ->
    Proplist = lists:keysort(1, [{id, 1},
                                 {name, <<"test">>},
                                 {custom_in_record, 10},
                                 {custom_not_in_record, 20}]),
    BinProplist = lists:keysort(1, [{<<"id">>, <<"1">>},
                                    {<<"name">>, <<"test">>},
                                    {<<"custom_in_record">>, <<"10">>},
                                    {<<"custom_not_in_record">>, <<"20">>}]),
    {ok, Model} = from_ext_proplist(BinProplist),
    Proplist = lists:keysort(1, Model:to_proplist()).

to_ext_proplist_test() ->
    Proplist = lists:keysort(1, [{id, 1},
                                 {name, <<"test">>},
                                 {custom_in_record, 10},
                                 {custom_not_in_record, 20}]),
    ExtProplist = [{K, {K, V}} || {K, V} <- Proplist],
    {ok, Model} = from_proplist(Proplist),
    ?assertEqual(ExtProplist, lists:keysort(1, Model:to_ext_proplist())).

-endif.
