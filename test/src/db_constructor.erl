-module(db_constructor).

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


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Opts [r, w, rw, sr, sw, srsw, rsw, srw]

constructor_fail_test_() ->
    Tests = [r, sr],
    [fun() -> ?_assertException(error, function_clause, constructor([F])) end || F <- Tests].

constructor_test_() ->
    Tests = [w, rw, sw, srsw, rsw, srw],
    [fun() ->  constructor([F]) end || F <- Tests].

constructor_changed_fields_test() ->
    Opts = [w, rw, sw, srsw, rsw, srw],
    Constructor = constructor(Opts),
    Model = Constructor(Opts),
    ?assertEqual(Model:get_changed_fields(), []).

-endif.
