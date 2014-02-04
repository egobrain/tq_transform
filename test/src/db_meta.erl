-module(db_meta).

-compile({parse_transform, tq_record_transform}).

%% Test
-field({field,
        [
         {type, binary}
        ]}).

-model([]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

module_meta_test() ->
    ?MODULE = ?MODULE:'$meta'(module).

record_index_meta_test() ->
    Index = ?MODULE:'$meta'({record_index, field}),
    ?assertEqual(Index > 1, true).

-endif.
