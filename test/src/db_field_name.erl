-module(db_field_name).

-compile({parse_transform, tq_record_transform}).
-compile(export_all).

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

-define(MODES, [r, w, rw, sr, sw, srsw, rsw, srw]).

check_mode(FieldMode, AccessMode) ->
    FieldAcl = tq_transform_utils:mode_to_acl(FieldMode),
    AccessAcl = tq_transform_utils:mode_to_acl(AccessMode),
    tq_transform_utils:check_acl(FieldAcl, AccessAcl).

%% Modes = [r, w, rw, sr, sw, srw, rsw, srsw].
%% [{SM, [M || M <- Modes, check_mode(M, SM)]} || SM <- Modes].
%% [{r,[r,rw,rsw]},
%%  {w,[w,rw,srw]},
%%  {rw,[rw]},
%%  {sr,[r,rw,sr,srw,rsw,srsw]},
%%  {sw,[w,rw,sw,srw,rsw,srsw]},
%%  {srw,[rw,srw]},
%%  {rsw,[rw,rsw]},
%%  {srsw,[rw,srw,rsw,srsw]}]
test_fields(T) ->
    [
     begin
         TM = T(M),
         case check_mode(M, SM) of
             true -> {TM, SM, {ok, M}};
             false -> {TM, SM, {error, {TM, unknown}}}
         end
     end
     || M <- ?MODES, SM <- ?MODES
    ].

binary_test_() ->
    T = fun(A) -> list_to_binary(atom_to_list(A)) end,
    Tests = test_fields(T),
    [{lists:flatten(io_lib:format("~s/~p", [F, M])),
      fun() ->
              R = get_field_name(F, [{mode, M}, binary_key])
      end} || {F, M, R} <- Tests].

atom_success_test_() ->
    T = fun(A) -> A end,
    Tests = test_fields(T),
    [{lists:flatten(io_lib:format("~p/~p", [F, M])),
      fun() ->
              R = get_field_name(F, [{mode, M}])
      end} || {F, M, R} <- Tests].

-endif.
