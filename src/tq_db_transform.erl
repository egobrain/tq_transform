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

-record(db_options,{
		  type :: string | integer | datetime,
		  alias :: binary() % Name of field in DB
		 }).

-record(access_mode,{
		  r = false :: boolean(),
		  sr = false :: boolean(),
		  w = false :: boolean(),
		  sw = false :: boolean()
		 }).

-record(record_options,{
		  type :: field_type(), % Record type. Usefull for dializer
		  default_value :: any(), % Default value
		  mode :: #access_mode{}, % From 'r | w | rw | sr | sw | srsw | rsw | srw'
		  getter :: true | false | custom, % create getter or use custom
		  setter :: true | false | custom, % create setter or use custom
		  init :: true | false % Fields which are needed for to init
		  %% validators :: [validator_function()]
		 }).

-record(field,{
		  name :: binary(), % Field name which will be used to access property

		  is_index = false :: boolean(),
		  is_required = false :: boolean(),

		  stores_in_record = true :: boolean(), % Set to true if field value stores in state record
		  record_options :: #record_options{}, % Editional record options

		  stores_in_database = true :: boolean(), % Set to true if field value stores in state DB
		  db_options :: #db_options{} % Editional DB record options
		 }).

-record(state, {
		  module :: atom(),
		  fields = [] :: [#field{}]
		 }).

-type field_type() :: {atom(),atom()} | integer | non_neg_integer | binary.

-define(DBG(F, D), io:format("~p:~p "++F, [?FILE, ?LINE| D])).

parse_transform(Ast, _Options)->
    %% ?DBG("~p~n=======~n", [Ast]),
    %% ?DBG("~s~n=======~n", [pretty_print(Ast)]),
	{Ast2, _State} = lists:mapfoldl(fun transform_node/2, #state{}, Ast),
	Ast3 = lists:flatten(lists:filter(fun(Node)-> Node =/= nil end, Ast2)),
    %% ?DBG("~p~n<<<<~n", [Res]),
    ?DBG("~s~n>>>>~n", [pretty_print(Ast3)]),
    Ast3.

transform_node(Node={attribute, _Line, module, Module}, State) ->
	State2 = State#state{module = Module},
	{Node, State2};
transform_node(Node={attribute, Line, field, FieldOpts}, #state{fields=Fields} = State) ->
	case normalize_fieldopts(FieldOpts) of
		{Name, DbType, Opts} ->
			case create_field(Name, DbType, Opts) of
				{ok, Field} ->
					State2 = State#state{fields=[Field | Fields]},
					{nil, State2};
				{error, Reason} ->
					Node2 = error_ast(Line, Reason),
					{Node2, State}
			end;
		_ ->
			{error_ast(Line, "Wrong field spec"), State}
	end;
transform_node(Node, State) ->
	{Node, State}.

normalize_fieldopts({Name, DbType}) ->
	{Name, DbType, []};
normalize_fieldopts(Rest) ->
	Rest.

create_field(Name, DbType, Opts) ->
	Field = #field{
			   name = Name
			  },
	{ok, Field}.


%% Internal helpers.

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
