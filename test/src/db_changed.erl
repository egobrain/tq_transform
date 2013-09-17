-module(db_init).

-compile({parse_transform, tq_record_transform}).

%% Test
-field({counter,
        [
         required,
         {type, binary},
         record, get, set,
         {default, 1}
        ]}).

-model([]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

changed_test() ->
    Model = new(),
    Model2 = Model:set_counter(15),
    ?assertEqual(Model2:is_changed(counter), true),
    ?assertEqual(Model2:counter(), 15).

-endif.
