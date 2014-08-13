-module(db_ext).

-compile({parse_transform, tq_record_transform}).

%% Test

-field({id,
        [
         {ext_name, <<"Id">>},
         {type, integer},
         {to_ext, {tag, [id]}}
        ]}).
-field({name,
        [
         required,
         {ext_name, <<"Name">>},
         {type, binary},
         {to_ext, {tag, [name]}},
         record, get, set,
         {default, <<"Default name">>},
         {mode, rw}
        ]}).

-field({custom_in_record,
        [
         {ext_name, <<"CustomInRecord">>},
         {type, integer},
         {to_ext, {tag, [custom_in_record]}},
         {record, true},
         {get, custom},
         {set, custom}
        ]}).

-field({custom_not_in_record,
        [
         {ext_name, <<"CustomNotInRecord">>},
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
-define(KEYS, [id, name, custom_in_record, custom_not_in_record]).

from_ext_proplist_test() ->
    Proplist = lists:keysort(1, [{id, 1},
                                 {name, <<"test">>},
                                 {custom_in_record, 10},
                                 {custom_not_in_record, 20}]),
    BinProplist = lists:keysort(1, [{<<"Id">>, <<"1">>},
                                    {<<"Name">>, <<"test">>},
                                    {<<"CustomInRecord">>, <<"10">>},
                                    {<<"CustomNotInRecord">>, <<"20">>}]),
    {ok, Model} = from_ext_proplist(BinProplist),
    Proplist = lists:keysort(1, Model:to_proplist()).

to_ext_proplist_test() ->
    Proplist = lists:keysort(1, [{id, 1},
                                 {name, <<"test">>},
                                 {custom_in_record, 10},
                                 {custom_not_in_record, 20}]),
    ExtProplist = [{'$meta'({ext_key, K}), {K, V}} || {K, V} <- Proplist],
    {ok, Model} = from_proplist(Proplist),
    ?assertEqual(ExtProplist, lists:keysort(1, Model:to_ext_proplist())).

meta_test() ->
    ExtKeys = [key_to_ext(K) || K <- ?KEYS],
    Result = ['$meta'({ext_key, K}) || K <- ?KEYS],
    ?assertEqual(ExtKeys, Result).

ext_fields_test() ->
    Proplist = lists:keysort(1, [{id, 1},
                                 {name, <<"test">>},
                                 {custom_in_record, 10},
                                 {custom_not_in_record, 20}]),
    ExtProplist = [{'$meta'({ext_key, K}), {K, V}} || {K, V} <- Proplist],
    {ok, Model} = from_proplist(Proplist),
    {ok, ExtFields} = Model:ext_fields([ K || {K, _} <- Proplist]),
    ?assertEqual(ExtProplist, ExtFields).

key_to_ext(id) -> <<"Id">>;
key_to_ext(name) -> <<"Name">>;
key_to_ext(custom_in_record) -> <<"CustomInRecord">>;
key_to_ext(custom_not_in_record) -> <<"CustomNotInRecord">>.

-endif.
