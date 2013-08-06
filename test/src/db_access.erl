-module(db_access).

-compile({parse_transform, tq_record_transform}).

%% Test
-field({r,	[{type, integer}, {mode, r}]}).
-field({w,	[{type, integer}, {mode, w}]}).
-field({rw,   [{type, integer}, {mode, rw}]}).
-field({sr,   [{type, integer}, {mode, sr}]}).
-field({sw,   [{type, integer}, {mode, sw}]}).
-field({srsw, [{type, integer}, {mode, srsw}]}).
-field({rsw,  [{type, integer}, {mode, rsw}]}).
-field({srw,  [{type, integer}, {mode, srw}]}).

-field({r_default,	[{type, integer}, {default, 100}, {mode, r}]}).
-field({w_default,	[{type, integer}, {default, 200}, {mode, w}]}).
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
			 {{w, #?MODULE.w}, '$write_only_stumb$'},
			 {{sw, #?MODULE.sw}, '$write_only_stumb$'},
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

from_bin_proplist_test_() ->
	Data = [{list_to_binary(atom_to_list(Opt)), <<"1">>} || Opt <- ?Opts],
	[{"unsafe", fun() ->
					  ?assertEqual({error, [{<<"r">>, unknown},
										    {<<"sr">>, unknown}]},
								   from_bin_proplist(Data, [unsafe], new()))
			  end},
	 {"safe", fun() ->
						?assertEqual({error, [{<<"r">>, unknown},
											  {<<"sr">>, unknown},
											  {<<"sw">>, unknown},
											  {<<"srsw">>, unknown},
											  {<<"rsw">>, unknown}]},
									 from_bin_proplist(Data, [], new()))
				end},
	 {"ignore_unknown", fun() ->
								{ok, _} = from_bin_proplist(Data, [ignore_unknown], new())
						end}
	].

to_proplist_test() ->
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
	ROpts = [{r, undefined}, %% mustn't by no set by proplist
			 %% {w, 2},
			 {rw, 3},
			 {sr, undefined}, %% mustn't by no set by proplist
			 %% {sw, 5},
			 {srsw, 6},
			 {rsw, 7},
			 {srw, 8},
			 {r_default, 100}, %% mustn't by no set by proplist
			 %% {w_default, 10},
			 {rw_default, 11},
			 {sr_default, 400}, %% mustn't by no set by proplist
			 %% {sw_default, 13},
			 {srsw_default, 14},
			 {rsw_default, 15},
			 {srw_default, 16}],
	{ok, Model} = from_proplist(WOpts),
	Proplist = to_proplist([unsafe], Model),
	?assertEqual(lists:keysort(1, ROpts), lists:keysort(1, Proplist)).

-endif.
