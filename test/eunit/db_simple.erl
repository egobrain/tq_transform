-module(db_simple).

%% Test
-field({id, [
			 index,
			 {db_type, integer}
			]}).
-field({name,
		[
		 required,
		 {db_alias, <<"db_name">>},
		 {db_type, string},
		 {type, binary},
		 db, record, init, get, set, 
		 {default, <<"Default name">>},
		 {mode, rw}
		]}).

-field({custom,
		[
		 {record, false},
		 {get, false},
		 {set, false}
		]}).

-model([
		{table, <<"test">>}
	   ]).
