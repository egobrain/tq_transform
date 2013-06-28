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

-module(tq_db_generator).

-include("include/records.hrl").
-include("include/ast_helpers.hrl").

-export([build_model/1]).

-define(atom_join(A, B), list_to_atom(atom_to_list(A) ++ "_" ++ atom_to_list(B))).
-define(prefix_set(A), ?atom_join(set, A)).
-define(changed_suffix(A), ?atom_join(A, '$changed')).

%% Build result ast.

-spec build_model(Model) -> {ok, {InfoAst, FunctionsAst}} | {error, Reason} when
	  Model :: #model{},
	  InfoAst :: erl_syntax:syntaxTree(),
	  FunctionsAst :: erl_syntax:syntaxTree(),
	  Reason :: any().
build_model(Model) ->
	Builders = [
				fun build_main_record/1,
				fun build_getter_and_setters/1,
				fun build_from_proplist/1,
				fun build_internal_functions/1,
				fun build_validators/1
			   ],
	tq_db_utils:error_writer_foldl(fun(F, {IBlock, FBlock}) ->
										   case F(Model) of
											   {ok, {IB, FB}} ->
												   {ok, {[IB|IBlock], [FB|FBlock]}};
											   {error, _} = Err ->
												   Err
										   end
								   end, {[], []}, Builders).

build_main_record(#model{module=Module, fields=Fields}) ->
	FieldsInRecord = [F || F <- Fields, F#field.stores_in_record],
	RecordFieldNames = [case F#field.record_options#record_options.default_value of
							undefined -> F#field.name;
							Val -> {F#field.name, Val}
						end || F <- Fields, F#field.stores_in_record],
	DbFieldNames =  [{?changed_suffix(F#field.name),
					  F#field.record_options#record_options.default_value =/= undefined}
					 || F <- FieldsInRecord, F#field.stores_in_database],
	RecordFields = lists:flatten([{'$is_new$', true},
								  RecordFieldNames,
								  DbFieldNames]),
	Attribute = def_record(Module, RecordFields),
	{ok, {[Attribute], []}}.

build_getter_and_setters(#model{module=Module, fields=Fields}) ->
	NewFun = ?function(new, [?clause([], none, [?record(Module,[])])]),
	NewExport = ?export(new, 0),
	GetterFields = [F || F <- Fields, F#field.record_options#record_options.getter],
	GetterFuns = [getter(Module, F) || F <- GetterFields],
	GetterExports = export_funs(GetterFuns),
	SetterFields = [F || F <- Fields, F#field.record_options#record_options.setter],
	SetterFuns = [setter(Module, F) || F <- SetterFields],
	SetterExports = export_funs(SetterFuns),

	IsNewFun = ?function(is_new, [?clause([?var('Model')], none, [?access(?var('Model'), Module, '$is_new$')])]),
	IsNewExport = ?export(is_new, 1),
	Funs = [NewFun, GetterFuns, SetterFuns, IsNewFun],
	Exports = [NewExport, GetterExports, SetterExports, IsNewExport],
	{ok, {Exports, Funs}}.

getter(Module, #field{name=Name}) ->
	  ?function(Name, [?clause([?var('Model')], none, [?access(?var('Model'), Module, Name)])]).
setter(Module, #field{name=Name, stores_in_database=InDb}) ->
	FunctionName = ?prefix_set(Name),
	Setter = case InDb of
				 true ->
					 ?cases(?eeq(?var('Val'),?access(?var('Model'), Module, Name)),
							[?clause([?atom(true)], none,
									 [?ok(?var('Model'))]),
							 ?clause([?atom(false)], none,
									 [?ok(?record(?var('Model'),Module,
												  [?field(Name,?var('Val')),
												   ?field(?changed_suffix(Name),?atom(true))]))])]);
				 false ->
					 ?ok(?record(?var('Model'), Module, [?field(Name,?var('Val'))]))
			 end,
	?function(FunctionName, [?clause([?var('Val'),?var('Model')], none, [Setter])]).

build_from_proplist(Model) ->
	Funs = lists:flatten(
			 [to_proplist_function(Model),
			  from_proplist_functions(Model),
			  from_bin_proplist_function(Model)
			 ]),
	Exports = export_funs(Funs),
	{ok, {Exports, Funs}}.

from_proplist_functions(#model{fields=Fields}) ->	
	Fun1 = ?function(from_proplist,
					 [?clause([?var('Proplist')], none,
							  [?apply(from_proplist,[?var('Proplist'), ?apply(new, [])])])]),
	Fun2 = ?function(from_proplist,
					 [?clause([?var('Proplist'),?var('Model')], none,
							  [?apply(tq_db_utils,error_writer_foldl,
									  [?func(from_proplist_,2),
									   ?var('Model'),
									   ?var('Proplist')])])]),
	SetterFieldNames = [F#field.name || F <- Fields,
										F#field.record_options#record_options.setter,
										F#field.record_options#record_options.mode#access_mode.sw],

	DefaultClasuse = [?clause([?tuple([?var('Field'),?underscore]), ?underscore], none,
							  [?error(?atom(unknown), ?var('Field'))])],
	Fun_ = ?function(from_proplist_,
					 [?clause(
						 [?tuple([?atom(F),?var('Val')]), ?var('Model')], none,
						 [?apply(?prefix_set(F), [?var('Val'), ?var('Model')])])
					  || F <- SetterFieldNames] ++ DefaultClasuse),
	[Fun1, Fun2, Fun_].

from_bin_proplist_function(#model{fields=Fields}) ->
	Fun1 = ?function(from_bin_proplist,
					 [?clause([?var('BinProplist')], none,
							  [?apply(from_bin_proplist, [?var('BinProplist'), ?apply(new, [])])])]),
	Fun2 = ?function(from_bin_proplist,
					 [?clause([?var('BinProplist'),?var('Model')], none,
							  [?apply(tq_db_utils,error_writer_foldl,
									  [?func(from_bin_proplist_,2),
									   ?var('Model'),
									   ?var('BinProplist')])])]),
	SetterFields = [F || F <- Fields,
							 F#field.record_options#record_options.setter,
							 F#field.record_options#record_options.mode#access_mode.w],
	DefaultClasuse = [?clause([?tuple([?var('Field'),?underscore]), ?underscore], none,
							  [?error(?atom(unknown), ?var('Field'))])],
	EClause = fun(F) -> ?clause([?error(?var('Reason'))], none,
								[?error(?tuple([?var('Reason'), ?atom(F#field.name)]))])
			  end,
	Fun_ = ?function(from_bin_proplist_,
					 [?clause(
						 [?tuple([?abstract(atom_to_binary(F#field.name)),?var('Bin')]), ?var('Model')], none,
						 [?cases(type_constructor(F,?var('Bin')),
								 [?clause([?ok(?var('Val'))], none,
										  [?cases(?apply_(?apply(validator,[?atom(F#field.name)]),[?var('Val')]),
												  [?clause([?ok(?var('Val2'))], none,
														   [?apply(?prefix_set(F#field.name), [?var('Val2'), ?var('Model')])]),
												   EClause(F)])]),
								  EClause(F)])])
					  || F <- SetterFields] ++ DefaultClasuse),
	[Fun1, Fun2, Fun_].

type_constructor(Field, A) ->
	case Field#field.record_options#record_options.type_constructor of
		{M, F} ->
			?apply(M, F, [A]);
		F ->
			?apply(F, [A])
	end.

to_proplist_function(#model{fields=Fields}) ->
	?function(to_proplist,
			  [?clause([?var('Model')], none,
					   [?list([?tuple([?atom(F#field.name),?apply(F#field.name, [?var('Model')])]) ||
								  F <- Fields,
								  F#field.record_options#record_options.mode#access_mode.sr])])]).
build_internal_functions(Model) ->
	Funs = [changed_fields_function(Model),
			meta1_function(Model),
			meta2_function(Model),
			field_constructor_function(Model),
			constructor0_function(),
			constructor1_function(Model)
		   ],
	Exports = export_funs(Funs),
	{ok, {Exports, Funs}}.

changed_fields_function(#model{module=Module, fields=Fields}) ->
	AllowedFields = [F#field.name || F <- Fields,
									 F#field.stores_in_database,
									 F#field.record_options#record_options.setter,
									 F#field.record_options#record_options.mode#access_mode.sw],
	ListAst = ?list([?tuple([?atom(F),
							 ?access(?var('Model'), Module, F),
							 ?access(?var('Model'), Module, ?changed_suffix(F))
							])
					 || F <- AllowedFields]),
	?function(get_changed_fields,
			  [?clause([?var('Model')], none,
					   [?list_comp(?tuple([?var('A'), ?var('B')]),
								   [?generator(?tuple([?var('A'), ?var('B'), ?var('C')]),
											   ListAst),
									?var('C')]
								  )])]).
constructor0_function() ->
	?function(constructor,
			  [?clause([], none,
					   [?apply(constructor, [?apply('$meta$', [?atom(db_fields)])])])]).

constructor1_function(#model{module=Module}) ->
	?function(constructor,
			  [?clause([?var('Fields')], none,
					   [?match(?var('Constructors'),
							   ?list_comp(?apply(field_constructor,[?var('F')]),
										  [?generator(?var('F'), ?var('Fields'))])),
						?func([?clause([?var('Tuple')], none,
								  [?match(?var('Model'),
										  ?apply(tq_db_utils, constructors_foldl,
												 [?var('Constructors'),
												  ?apply(new, []),
												  ?apply(tuple_to_list,[?var('Tuple')])])),
								   ?record(?var('Model'), Module, [?field('$is_new$', ?atom(false))])
								  ]
									  )])])]).

meta1_function(#model{table=Table, fields=Fields}) ->
	Clauses = [?clause([?atom(table)], none, [?abstract(Table)]),
			   ?clause([?atom(db_fields)], none, [?list([?atom(F#field.name) || F <- Fields,
																				F#field.stores_in_database])]),
			   ?clause([?atom(record_fields)], none, [?list([?atom(F#field.name) || F <- Fields,
																					F#field.stores_in_record])])
			  ],
	?function('$meta$', Clauses).

meta2_function(#model{fields=Fields}) ->
	Clauses = [?clause([?atom(db_type), ?var('Field')], none,
					   [?cases(?var('Field'),
							   [?clause([?atom(F#field.name)], none,
										[?atom(F#field.db_options#db_options.type)])
								|| F <- Fields, F#field.stores_in_record])]),
			   ?clause([?atom(db_alias), ?var('Field')], none,
					   [?cases(?var('Field'),
							   [?clause([?atom(F#field.name)], none,
										[?abstract(F#field.db_options#db_options.alias)])
								|| F <- Fields, F#field.stores_in_record])])
			  ],		  
	?function('$meta$', Clauses).

field_constructor_function(#model{fields=Fields}) ->
	DefaultClasuse = ?clause([?var('Fun')], [?nif_is_function(?var('Fun'))], [?var('Fun')]),
	?function(field_constructor,
			  [?clause([?atom(F#field.name)], none,
					   [?func([?clause([?var('Val'), ?var('Model')], none,
									   [?apply(?prefix_set(F#field.name), [?var('Val'), ?var('Model')])])])]) ||
				  F <- Fields,
				  F#field.stores_in_database,
				  F#field.record_options#record_options.setter
			  ] ++ [DefaultClasuse]).


build_validators(#model{module=Module, fields=Fields}) ->
	ValidatorFun = ?function(validator,
							 [?clause([?atom(F#field.name)], none,
									  [validator([],
												 F#field.is_required,
												 is_write_only(F))]) || F <- Fields]),
	ValidFun = ?function(valid,
						 [?clause([?var('Model')], none,
								  [?match(?var('Data'),
										  ?list([?tuple(
													[?atom(F#field.name),
													 ?apply(validator, [?atom(F#field.name)]),
													 ?access(?var('Model'), Module, F#field.name)])
												 || F <- Fields,
													F#field.stores_in_record])),
								   ?apply(tq_db_utils, valid, [?var('Data')])
								  ])]),
	Funs = [ValidatorFun, ValidFun],
	Exports = export_funs(Funs),
	{ok, {Exports, Funs}}.

validator(_Validators, IsRequired, IsWriteOnly) ->
	WO_clause = ?clause([?atom('$write_only_stumb$')], none, [?atom(ok)]),
	Req_clause = ?clause([?atom(undefined)], none, [?error(?atom(required))]),
	Main_clause = ?clause([?underscore], none, [?atom(ok)]),
	Clauses = acc_if(IsWriteOnly, WO_clause,
					 acc_if(IsRequired, Req_clause,
							[Main_clause])),
	?func(Clauses).
					   
%% Internal helpers.

is_write_only(Field) ->
	AccessMode = Field#field.record_options#record_options.mode,
	not AccessMode#access_mode.sw.

def_record(Name, Fields) ->
	?def_record(Name, [case F of
						   Atom when is_atom(F) -> ?field(Atom);
						   {Atom, Value} when is_atom(Atom) -> ?field(Atom, ?abstract(Value))
					   end || F <- Fields]).

export_funs(Funs) ->
	?export_all([{erl_syntax:atom_value(erl_syntax:function_name(F)),
				  erl_syntax:function_arity(F)} || F <- Funs]).

acc_if(true, Val, Acc) -> [Val|Acc];
acc_if(false, _, Acc) -> Acc.

atom_to_binary(Atom) ->
	list_to_binary(atom_to_list(Atom)).
