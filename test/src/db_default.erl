-module(db_default).

-compile({parse_transform, tq_record_transform}).
-export([
		 def_value/0,
		 def_value/1
		]).

%% Test
-field({def1,
        [
         {type, integer},
         {default, 1}
        ]}).

-field({def2,
        [
         {type, integer},
         {default_call, def_value}
        ]}).

-field({def3,
        [
         {type, integer},
         {default_call, {def_value, [3]}}
        ]}).

-field({def4,
        [
         {type, integer},
         {default_call, {?MODULE, def_value, [4]}}
        ]}).

-field({def5,
        [
         {type, integer},
         {default_call, {?MODULE, def_value}}
        ]}).

def_value() -> 2.
def_value(A) -> A.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

default_test_() ->
	Model = new(),
	Tests =
		[
		 {def1, 1},
		 {def2, 2},
		 {def3, 3},
		 {def4, 4},
		 {def5, 2}
		],
	[fun() -> ?assertEqual(Model:F(), V) end
	 || {F, V} <- Tests].

-endif.
