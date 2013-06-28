%% Copyright (c) 2011-2013, Jakov Kozlov <xazar.studio@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(tq_db_transform).

-export([parse_transform/2]).

-include("include/records.hrl").
-include("include/ast_helpers.hrl").

-define(DBG(F, D), io:format("~p:~p "++F, [?FILE, ?LINE| D])).

parse_transform(Ast, _Options)->
	try
		%% ?DBG("~n~p~n=======~n", [Ast]),
		%% ?DBG("~n~s~n=======~n", [pretty_print(Ast)]),
		{Ast2, Model} = lists:mapfoldl(fun transform_node/2, #model{}, Ast),
		{ModuleBlock, InfoBlock, FunctionsBlock} = split_ast(Ast2),
		Ast3 = case valid_model(Model) of
				   {ok, Model} ->
					   case tq_db_generator:build_model(Model) of
						   {ok, {IBlock, FBlock}} ->
							   IBlock2 = lists:reverse(IBlock),
							   FBlock2 = lists:reverse(FBlock),
							   revert(lists:flatten([ModuleBlock, IBlock2, InfoBlock, FBlock2, FunctionsBlock]));
						   {error, Reasons} ->
							   Ast2 ++ [{error,{1, erl_parse, R}} || R <- Reasons]
					   end;
				   {error, Reasons} ->
					   Ast2 ++ [{error,{1, erl_parse, R}} || R <- Reasons]
			   end,
		%% ?DBG("~n~p~n<<<<~n", [Ast3]),
		Ast3 = lists:flatten(lists:filter(fun(Node)-> Node =/= nil end, Ast3)),
		?DBG("~n~s~n>>>>~n", [pretty_print(Ast3)]),
		%% ?DBG("Model:~n ~p~n", [Model]),
		Ast3
	catch T:E ->
			Reason = io_lib:format("~p:~p | ~p ~n", [T, E, erlang:get_stacktrace()]),
			[{error, {1, erl_parse, Reason}} | Ast]
	end.

split_ast(Ast) ->
	{ok, {ModuleBlock, RestBlock}} = ast_split_with(fun({attribute, _, module, _}) -> true;
													   (_) -> false
													end, Ast, 'after'),
	{ok, {InfoBlock, FunctionsBlock}} = ast_split_with(fun({function, _, _, _, _}) -> true;
														  (_) -> false
													   end, RestBlock, 'before'),
	{ModuleBlock, InfoBlock, FunctionsBlock}.

transform_node(Node={attribute, Line, model, Opts}, Model) ->
	case is_list(Opts) of
		true ->
			OptionFun = fun(Val, M) ->
								case normalize_option(Val) of
									{ok, {OptionName, Data}} -> model_option(OptionName, Data, M);
									{error, _} = Err -> Err
								end
						end,
			case tq_db_utils:error_writer_foldl(OptionFun, Model, Opts) of
				{ok, Model2} ->
					{Node, Model2};
				{error, _} = Err ->
					{error_ast(Line, Err), Model}
			end;
		false ->
			Node2 = error_ast(Line, "Wrong model spec"),
			{Node2, Model}
	end;
transform_node(Node={attribute, _Line, module, Module}, Model) ->
	Model2 = Model#model{module = Module},
	{Node, Model2};
transform_node(Node={attribute, Line, field, FieldOpts}, #model{fields=Fields} = Model) ->
	case FieldOpts of
		{Name, Opts} when is_atom(Name) andalso is_list(Opts) ->
			case create_field(Name, Opts) of
				{ok, Field} ->
					Model2 = Model#model{fields=[Field | Fields]},
					{Node, Model2};
				{error, Reason} ->
					Node2 = error_ast(Line, Reason),
					{Node2, Model}
			end;
		_ ->
			{error_ast(Line, "Wrong field spec"), Model}
	end;
transform_node(Node, State) ->
	{Node, State}.

%% Model
model_option(table, Table, Model) ->
	Model2 = Model#model{table = Table},
	{ok, Model2};
model_option(Name, _, _) ->
	{error, {unknown, Name}}.

create_field(Name, Opts) ->
	Field = #field{name = Name},
	OptionFun = fun(Val, State) ->
						case normalize_option(Val) of
							{ok, {OptionName, Data}} -> option(OptionName, Data, State);
							{error, _} = Err -> Err
						end
				end,
	case tq_db_utils:error_writer_foldl(OptionFun, Field, Opts) of
		{ok, Field2} ->
			Field3 = normalize_field(Field2),
			{ok, Field3};
		{error, _} = Err ->
			Err
	end.	

normalize_option({Name, Value}) when is_atom(Name) ->
	{ok, {Name, Value}};
normalize_option(Name) when is_atom(Name) ->
	{ok, {Name, true}};
normalize_option(Val) ->
	{error, {"Wrong option spec", Val}}.

option(index, Value, Field) ->
	Field2 = Field#field{is_index = Value},
	{ok, Field2};
option(required, Value, Field) ->
	Field2 = Field#field{is_required = Value},
	{ok, Field2};
%% DB
option(db, StoresInDB, Field) ->
	Field2 = Field#field{stores_in_database = StoresInDB},
	{ok, Field2};
option(db_alias, Alias, #field{db_options=DbOptions} = Field) ->
	DbOptions2 = DbOptions#db_options{alias=Alias},
	Field2 = Field#field{db_options=DbOptions2},
	{ok, Field2};
option(db_type, Alias, #field{db_options=DbOptions} = Field) ->
	DbOptions2 = DbOptions#db_options{type=Alias},
	Field2 = Field#field{db_options=DbOptions2},
	{ok, Field2};
%% Record
option(default, DefaultValue, #field{record_options=RecOptions} = Field) ->
	RecOptions2 = RecOptions#record_options{default_value = DefaultValue},
	Field2 = Field#field{record_options=RecOptions2},
	{ok, Field2};
option(init, Init, #field{record_options=RecOptions} = Field) ->
	RecOptions2 = RecOptions#record_options{init = Init},
	Field2 = Field#field{record_options=RecOptions2},
	{ok, Field2};
option(mode, Mode, #field{record_options=RecOptions} = Field) ->
	RecOptions2 = RecOptions#record_options{mode = mode_to_acl(Mode)},
	Field2 = Field#field{record_options = RecOptions2},
	{ok, Field2};
option(type, Type, #field{record_options=RecOptions} = Field) ->
	RecOptions2 = RecOptions#record_options{type = Type},
	Field2 = Field#field{record_options = RecOptions2},
	{ok, Field2};
option(get, Getter, #field{record_options=RecOptions} = Field) ->
	RecOptions2 = RecOptions#record_options{getter = Getter},
	Field2 = Field#field{record_options = RecOptions2},
	{ok, Field2};
option(set, Setter, #field{record_options=RecOptions} = Field) ->
	RecOptions2 = RecOptions#record_options{setter = Setter},
	Field2 = Field#field{record_options = RecOptions2},
	{ok, Field2};
option(record, StoresInRecord, Field) ->
	Field2 = Field#field{stores_in_record = StoresInRecord},
	{ok, Field2};
option(Name,_,_) ->
	{error, {"Unknown option", Name}}.

mode_to_acl(r) -> #access_mode{r=true, sr=true};
mode_to_acl(w) -> #access_mode{w=true, sw=true};
mode_to_acl(rw) -> #access_mode{r=true, sr=true, w=true, sw=true};
mode_to_acl(sr) -> #access_mode{sr=true};
mode_to_acl(sw) -> #access_mode{sw=true};
mode_to_acl(srsw) -> #access_mode{sr=true, sw=true};
mode_to_acl(rsw) -> #access_mode{r=true, sr=true, sw=true};
mode_to_acl(srw) -> #access_mode{sr=true, w=true, sw=true}.

%% Field rules.

normalize_field(Field) ->
	Rules = [
			 fun index_is_required_rule/1,
			 fun default_alias_name_rule/1,
			 fun get_set_needs_record_rule/1
			],
	lists:foldl(fun(F, D) -> F(D) end, Field, Rules).

index_is_required_rule(Field=#field{is_index=true}) ->
	Field#field{is_required = true};
index_is_required_rule(Field) -> Field.

default_alias_name_rule(Field=#field{name=Name, stores_in_database = true, db_options=DbOptions}) when
	  DbOptions#db_options.alias =:= undefined ->
	DbOptions2 = DbOptions#db_options{alias = atom_to_binary(Name)},
	Field#field{db_options=DbOptions2};
default_alias_name_rule(Field) -> Field.

get_set_needs_record_rule(Field=#field{record_options=#record_options{getter=Getter, setter=Setter}}) when
	  Getter orelse Setter ->
	Field#field{stores_in_record=true};
get_set_needs_record_rule(Field) -> Field.


%% Validators.

valid_model(Model) ->
	Validators = [
				  fun table_required_validator/1
				 ],
	tq_db_utils:error_writer_foldl(fun(F, M) -> F(M) end, Model, Validators).

table_required_validator(#model{table=undefined}) ->
	{error, "Table name required"};
table_required_validator(Model) -> {ok, Model}.

%% Internal helpers.

-spec ast_split_with(Fun, List, 'before') -> {List1, List2} when
	  Fun :: fun((E) -> boolean()),
	  List :: [E], List1 :: [E], List2 :: [E];
					(Fun, List, 'after') ->   {List1, List2} | {error, not_found} when
	  Fun :: fun((E) -> boolean()),
	  List :: [E], List1 :: [E], List2 :: [E].
ast_split_with(Fun, List, Opt) ->
	ast_split_with(Fun, List, Opt, []).
ast_split_with(Fun, [E|Rest] = List, Opt, Acc) ->
	case Fun(E) of
		true ->
			case Opt of
				'before' ->
					{ok, {lists:reverse(Acc), List}};
				'after' ->
					{ok, {lists:reverse([E|Acc]), Rest}}
			end;
		false ->
			ast_split_with(Fun, Rest, Opt, [E|Acc])
	end;
ast_split_with(_Fun, [], Opt, Acc) ->
	case Opt of
		'before' ->
			{ok, {lists:reverse(Acc), []}};
		'after' ->
			{error, not_found}
	end.

-spec error_ast(Line, Reason) -> {error, {Line, Reason}} when
	  Line  :: non_neg_integer(),
	  Reason :: any().
error_ast(Line, Reason) ->
	{error, {Line, Reason}}.

-spec revert(parse_trans:forms()) ->
					parse_trans:forms().
revert(Tree) ->
    [erl_syntax:revert(T) || T <- lists:flatten(Tree)].

-spec pretty_print(parse_trans:forms()) -> iolist().
pretty_print(Forms0) ->
	Forms = epp:restore_typed_record_fields(revert(Forms0)),
    [io_lib:fwrite("~s~n",
				   [lists:flatten([erl_pp:form(Fm) ||
									  Fm <- Forms])])].

atom_to_binary(Atom) ->
	list_to_binary(atom_to_list(Atom)).

%% Tests.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

index_is_required_rule_test_() ->
	C = fun({IsIndex, IsRequired}) -> #field{is_index=IsIndex, is_required=IsRequired} end,
	Tests = [
			 {{true, true}, {true, true}},
			 {{true, false}, {true, true}},
			 {{false, true}, {false, true}},
			 {{false, false}, {false, false}}
			],
	F = fun(From, To) ->
				R = index_is_required_rule(C(From)),
				R = C(To)
		end,
	[fun() -> F(From, To) end || {From, To} <- Tests].

default_alias_name_rule_test_() ->
	C = fun({Name, InDb, Alias}) ->
				#field{name=Name, stores_in_database=InDb, db_options=#db_options{alias=Alias}}
		end,
	Tests = [
			 {{name, true, undefined}, {name, true, <<"name">>}},
			 {{name, false, undefined}, {name, false, undefined}},
			 {{name, true, <<"test">>}, {name, true, <<"test">>}},
			 {{name, false, <<"test">>}, {name, false, <<"test">>}}
			],
	F = fun(From, To) ->
				R = default_alias_name_rule(C(From)),
				R = C(To)
		end,
	[fun() -> F(From, To) end || {From, To} <- Tests].



get_set_needs_record_rule_test_() ->
	C = fun({Stored, Getter, Setter}) -> #field{stores_in_record=Stored, record_options=#record_options{getter=Getter, setter=Setter}} end,
	Tests = [
			 {{false, true, true}, {true, true, true}},
			 {{false, true, false}, {true, true, false}},
			 {{false, false, true}, {true, false, true}},
			 {{false, false, false}, {false, false, false}},
			 {{true, true, true}, {true, true, true}},
			 {{true, true, false}, {true, true, false}},
			 {{true, false, true}, {true, false, true}},
			 {{true, false, false}, {true, false, false}}
			],
	F = fun(From, To) ->
				R = get_set_needs_record_rule(C(From)),
				R = C(To)
		end,
	[fun() -> F(From, To) end || {From, To} <- Tests].

ast_split_test_() ->
	Tests = [
			 {[{1,1}, {2,2}, {3,3}], {'before', 1}, {ok, {[], [{1,1}, {2,2}, {3,3}]}}},
			 {[{1,1}, {2,2}, {3,3}], {'after',  1}, {ok, {[{1,1}], [{2,2}, {3,3}]}}},
			 {[{1,1}, {2,2}, {3,3}], {'before', 2}, {ok, {[{1,1}], [{2,2}, {3,3}]}}},
			 {[{1,1}, {2,2}, {3,3}], {'after',  2}, {ok, {[{1,1}, {2,2}], [{3,3}]}}},
			 {[{1,1}, {2,2}, {3,3}], {'before', 3}, {ok, {[{1,1}, {2,2}], [{3,3}]}}},
			 {[{1,1}, {2,2}, {3,3}], {'after',  3}, {ok, {[{1,1}, {2,2}, {3,3}], []}}},
			 {[{1,1}, {2,2}, {3,3}], {'before', 4}, {ok, {[{1,1}, {2,2}, {3,3}], []}}},
			 {[{1,1}, {2,2}, {3,3}], {'after',  4}, {error, not_found}}
			],
	F = fun(D, {Opt, Key},  R) ->
				Fun = fun(E) -> element(1, E) =:= Key end,
				R = ast_split_with(Fun, D, Opt)
		end,
	[fun() -> F(List, Opts, Res) end || {List, Opts, Res} <- Tests].

-endif.

