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

-module(tq_record_generator).

-include_lib("tq_transform/include/access_mode.hrl").
-include_lib("tq_transform/include/record_model.hrl").
-include_lib("tq_transform/include/ast_helpers.hrl").

-export([build_model/1,
         meta_clauses/1
        ]).

-define(atom_join(A, B), list_to_atom(atom_to_list(A) ++ "_" ++ atom_to_list(B))).
-define(prefix_set(A), ?atom_join(set, A)).
-define(changed_suffix(A), ?atom_join(A, '$changed')).
-define(MODES, [r, w, rw, sr, sw, srw, rsw, srsw]).

%% Build result ast.

-spec build_model(Model) -> {InfoAst, FunctionsAst} when
      Model :: #record_model{},
      InfoAst :: erl_syntax:syntaxTree(),
      FunctionsAst :: erl_syntax:syntaxTree().
build_model(Model) ->
    Builders = [
                %% fun hooks_args_to_abstract/1,
                fun build_main_record/1,
                fun build_new/1,
                fun build_getter_and_setters/1,
                fun build_proplists/1,
                fun build_internal_functions/1,
                fun build_validators/1,
                fun build_is_changed/1,
                fun build_get_field_name/1
               ],
    lists:foldl(fun(F, {IBlock, FBlock}) ->
                        {IB, FB} = F(Model),
                        {[IB | IBlock], [FB | FBlock]}
                end, {[], []}, Builders).

meta_clauses(#record_model{module=Module, fields=Fields}) ->
    RecordIndexClause =
        ?clause([?tuple([?atom(record_index), ?var('Field')])], none,
                [?cases(?var('Field'),
                        [?clause([int_key(F)], none,
                                 [?record_index(Module, F#record_field.name)])
                         || F <- Fields, F#record_field.stores_in_record]
                       )
                ]),
    ExtKeys =
        ?clause([?tuple([?atom(ext_key), ?var('Field')])], none,
                [?cases(?var('Field'),
                        [?clause([int_key(F)], none,
                                 [ext_key(F)])
                         || F <- Fields]
                       )
                ]),
    ModuleClause =
        ?clause([?atom(module)], none,
                [?atom(Module)]),
    [
     RecordIndexClause,
     ModuleClause,
     ExtKeys
    ].

build_main_record(#record_model{module=Module, fields=Fields}) ->
    RecordFields =
        [
         [
          Name,
          {?changed_suffix(Name), {value, false}}
         ]
         || #record_field{
               name=Name,
               stores_in_record=true
              } <- Fields],
    RecordFields2 =
        lists:flatten([{'$is_new$', {value, false}},
                       RecordFields]),
    RecordFieldsAst =
        [case F of
             Atom when is_atom(F) -> ?field(Atom, ?atom(null));
             {Atom, Data} when is_atom(Atom) ->
                 ?field(Atom,
                        case Data of
                            {value, Val} -> ?abstract(Val);
                            {func, MFA} -> function_call(MFA, [])
                        end)
         end || F <- RecordFields2],
    Attribute = ?def_record(Module, RecordFieldsAst),
    {[Attribute], []}.

build_new(#record_model{module=Module, fields=Fields}) ->
    DefaultFields =
        [
         [
          {Name, Default},
          {?changed_suffix(Name), {value, true}}
         ] || #record_field{name=Name, default=Default} <- Fields,
              Default =/= undefined
        ],
    FieldsData = lists:flatten([{'$is_new$', {value, true}},
                                DefaultFields]),
    RecordFieldsAst =
        [
         ?field(Name,
                case Data of
                    {value, Val} -> ?abstract(Val);
                    {func, MFA} -> function_call(MFA, [])
                end)
         || {Name, Data} <- FieldsData
        ],
    NewFun = ?function(new, [?clause([], none, [?record(Module, RecordFieldsAst)])]),
    NewExport = ?export(new, 0),
    {[NewExport], [NewFun]}.

build_getter_and_setters(#record_model{module=Module, fields=Fields}) ->
    GetterFields = [F || F <- Fields, F#record_field.getter =:= true],
    GetterFuns = [getter(Module, F) || F <- GetterFields],
    GetterExports = ?export_funs(GetterFuns),
    CustomGettersExports = ?export_all([{F#record_field.name, 1} || F <- Fields, F#record_field.getter =:= custom]),

    SetterFields = [F || F <- Fields, F#record_field.setter =:= true],
    SetterFuns = [setter(Module, F) || F <- SetterFields],
    SetterExports = ?export_funs(SetterFuns),
    CustomSettersExports = ?export_all([{?prefix_set(F#record_field.name), 2} || F <- Fields, F#record_field.setter =:= custom]),

    IsNewFun = ?function(is_new, [?clause([?var('Model')], none, [?access(?var('Model'), Module, '$is_new$')])]),
    IsNewExport = ?export(is_new, 1),
    Funs = [GetterFuns, SetterFuns, IsNewFun],
    Exports = [GetterExports, CustomGettersExports, SetterExports, CustomSettersExports, IsNewExport],
    {Exports, Funs}.

getter(Module, #record_field{name=Name}) ->
    ?function(Name, [?clause([?var('Model')], none, [?access(?var('Model'), Module, Name)])]).
setter(Module, #record_field{name=Name}) ->
    ?function(?prefix_set(Name),
              [?clause([?var('Val'), ?var('Model')], none,
                       [?cases(?eeq(?var('Val'), ?access(?var('Model'), Module, Name)),
                               [?clause([?atom(true)], none,
                                        [?var('Model')]),
                                ?clause([?atom(false)], none,
                                        [?record(?var('Model'), Module,
                                                 [?field(Name, ?var('Val')),
                                                  ?field(?changed_suffix(Name), ?atom(true))])])])])]).

build_proplists(Model) ->
    Funs = [to_proplist_function(Model),
            to_ext_proplist_function(Model),
            from_proplist_functions(Model),
            from_ext_proplist_function(Model),
            fields_function(Model),
            ext_fields_function(Model)
           ],
    {Public0, Private0} = lists:foldl(fun({P, Pr}, {Pub, Priv}) ->
                                              {[P | Pub], [Pr | Priv]};
                                         (P, {Pub, Priv}) ->
                                              {[P | Pub], Priv}
                                      end, {[], []}, Funs),
    {Public, Private} = {lists:flatten(Public0), lists:flatten(Private0)},
    Exports = ?export_funs(Public),
    {Exports, Public ++ Private}.

to_proplist_function(Model) ->
    to_proplist_function_(
      to_proplist,
      Model,
      fun(_F, Ast) -> Ast end,
      fun int_key/1
     ).

to_ext_proplist_function(Model) ->
    to_proplist_function_(
      to_ext_proplist,
      Model,
      fun to_ext_hook/2,
      fun ext_key/1
     ).

to_ext_hook(F, Ast) ->
    ?cases(Ast,
           [
            ?clause([?atom(null)], none, [?atom(null)]),
            ?clause([?var('NotNull')], none, [to_ext_hook_(F, ?var('NotNull'))])
           ]).

to_ext_hook_(#record_field{to_ext=none}, Ast) ->
    Ast;
to_ext_hook_(#record_field{to_ext=ToExt}, Ast) ->
    function_call(ToExt, [Ast]).

to_proplist_function_(FName, #record_model{fields=Fields}, ArgModifierFun, KeyFun) ->
    Fun_ = fun(AccessModeOpt) ->
                   ?list([?tuple(
                             [KeyFun(F),
                              ArgModifierFun(F, ?apply(F#record_field.name, [?var('Model')]))]
                            ) ||
                             F <- Fields,
                             element(AccessModeOpt, F#record_field.mode),
                             F#record_field.getter =/= false
                         ])
           end,
    Fun1 = ?function(FName,
                     [?clause([?var('Model')], none,
                              [?apply(FName, [?abstract([]), ?var('Model')])])]),
    Fun2 = ?function(FName,
                     [?clause([?var('Opts'), ?var('Model')], none,
                              [?cases(?apply(lists, member, [?atom(unsafe), ?var('Opts')]),
                                      [?clause([?atom(true)], none,
                                               [Fun_(#access_mode.sr)]),
                                       ?clause([?atom(false)], none,
                                               [Fun_(#access_mode.r)])])])]),
    [Fun1, Fun2].

from_proplist_functions(#record_model{fields=Fields}) ->
    DefaultOpts = ?abstract([unsafe]),
    Fun1 = ?function(from_proplist,
                     [?clause([?var('Proplist')], none,
                              [?apply(from_proplist, [?var('Proplist'), DefaultOpts, ?apply(new, [])])])]),
    Fun2 = ?function(from_proplist,
                     [?clause([?var('Proplist'), ?var('Opts')], [?apply(is_list,[?var('Opts')])],
                              [?apply(from_proplist, [?var('Proplist'), ?var('Opts'), ?apply(new, [])])]),
                      ?clause([?var('Proplist'), ?var('Model')], none,
                              [?apply(from_proplist, [?var('Proplist'), DefaultOpts, ?var('Model')])])]),
    Fun3 = ?function(from_proplist,
                     [?clause([?var('Proplist'), ?var('Opts'), ?var('Model')], none,
                              [?match(?var('Fun'), ?cases(?apply(lists, member, [?atom(unsafe), ?var('Opts')]),
                                                          [?clause([?atom(true)], none,
                                                                   [implicit_func(from_proplist_unsafe_, 3)]),
                                                           ?clause([?atom(false)], none,
                                                                   [implicit_func(from_proplist_safe_, 3)])])),
                               ?match(?var('IgnoreUnknown'), ?apply(lists, member, [?atom(ignore_unknown), ?var('Opts')])),
                               ?match(?var('Fun2'), ?func([?clause([?var('E'), ?var('M')], none,
                                                                   [?apply_(?var('Fun'), [?var('E'), ?var('M'), ?var('IgnoreUnknown')])])])),
                               ?apply(tq_transform_utils, error_writer_foldl, [?var('Fun2'), ?var('Model'), ?var('Proplist')])])]),
    DefaultClasuse = [?clause([?tuple([?var('_Field'), ?underscore]), ?var('Model'), ?atom(true)], none,
                              [?ok(?var('Model'))]),
                      ?clause([?tuple([?var('Field'), ?underscore]), ?var('_Model'), ?atom(false)], none,
                              [?error(?var('Field'), ?atom(unknown))])],
    Fun_ = fun(Suffix, AccessModeOpt) ->
                   ?function(?atom_join(from_proplist, Suffix),
                             [?clause(
                                 [?tuple([int_key(F), ?var('Val')]), ?var('Model'), ?underscore], none,
                                 [?ok(?apply(?prefix_set(F#record_field.name), [?var('Val'), ?var('Model')]))])
                              || F <- Fields,
                                 F#record_field.setter =/= false,
                                 element(AccessModeOpt, F#record_field.mode)] ++ DefaultClasuse)
           end,
    FunUnsafe_ = Fun_(unsafe_, #access_mode.sw),
    FunSafe_ = Fun_(safe_, #access_mode.w),
    {[Fun1, Fun2, Fun3], [FunUnsafe_, FunSafe_]}.

from_ext_proplist_function(#record_model{fields=Fields}) ->
    DefaultOpts = ?abstract([]),
    Fun1 = ?function(from_ext_proplist,
                     [?clause([?var('BinProplist')], none,
                              [?apply(from_ext_proplist, [?var('BinProplist'), DefaultOpts, ?apply(new, [])])])]),
    Fun2 = ?function(from_ext_proplist,
                     [?clause([?var('BinProplist'), ?var('Opts')], [?apply(is_list,[?var('Opts')])],
                              [?apply(from_ext_proplist, [?var('BinProplist'), ?var('Opts'), ?apply(new, [])])]),
                      ?clause([?var('BinProplist'), ?var('Model')], none,
                              [?apply(from_ext_proplist, [?var('BinProplist'), DefaultOpts, ?var('Model')])])]),
    Fun3 = ?function(from_ext_proplist,
                     [?clause([?var('BinProplist'), ?var('Opts'), ?var('Model')], none,
                              [?match(?var('Fun'), ?cases(?apply(lists, member, [?atom(unsafe), ?var('Opts')]),
                                                          [?clause([?atom(true)], none,
                                                                   [implicit_func(from_ext_proplist_unsafe_, 3)]),
                                                           ?clause([?atom(false)], none,
                                                                   [implicit_func(from_ext_proplist_safe_, 3)])])),
                               ?match(?var('IgnoreUnknown'), ?apply(lists, member, [?atom(ignore_unknown), ?var('Opts')])),
                               ?match(?var('Fun2'), ?func([?clause([?var('E'), ?var('M')], none,
                                                                   [?apply_(?var('Fun'), [?var('E'), ?var('M'), ?var('IgnoreUnknown')])])])),
                               ?apply(tq_transform_utils, error_writer_foldl, [?var('Fun2'), ?var('Model'), ?var('BinProplist')])])]),
    DefaultClasuse = [?clause([?tuple([?var('_Field'), ?underscore]), ?var('Model'), ?atom(true)], none,
                              [?ok(?var('Model'))]),
                      ?clause([?tuple([?var('Field'), ?underscore]), ?var('_Model'), ?atom(false)], none,
                              [?error(?var('Field'), ?atom(unknown))])],
    SetterClause = fun(F, Var) -> ?ok(?apply(?prefix_set(F#record_field.name), [Var, ?var('Model')])) end,
    Cases = fun(F, A) -> ?cases(A,
                                [?clause([?ok(?var('Val'))], none,
                                         [SetterClause(F, ?var('Val'))]),
                                 ?clause([?error(?var('Reason'))], none,
                                         [?error(?tuple([int_key(F), ?var('Reason')]))])])
            end,
    IgnoreNullHook =
        fun (F, Var, Ast) ->
                ?cases(Var,
                       [?clause([?atom(null)], none, [SetterClause(F, Var)]),
                        ?clause([?underscore], none, [Ast])])
        end,
    Fun_ = fun(Suffix, AccessModeOpt) ->
                   ?function(?atom_join(from_ext_proplist, Suffix),
                             [?clause(
                                 [?tuple([ext_key(F), ?var('Bin')]), ?var('Model'), ?underscore], none,
                                 [IgnoreNullHook(
                                    F,
                                    ?var('Bin'),
                                    case F#record_field.from_ext of
                                        none ->
                                            SetterClause(F, ?var('Bin'));
                                        FromExt ->
                                            Cases(F, function_call(FromExt, [?var('Bin')]))
                                    end
                                   )])
                              || F <- Fields,
                                 F#record_field.setter =/= false,
                                 element(AccessModeOpt, F#record_field.mode)] ++ DefaultClasuse)
           end,
    FunUnsafe_ = Fun_(unsafe_, #access_mode.sw),
    FunSafe_ = Fun_(safe_, #access_mode.w),
    {[Fun1, Fun2, Fun3], [FunUnsafe_, FunSafe_]}.

fields_function(Model) ->
    fields_function_(
      fields, [unsafe],
      Model,
      fun(_F, Ast) -> Ast end,
      fun int_key/1
     ).

ext_fields_function(Model) ->
    fields_function_(
      ext_fields, [],
      Model,
      fun to_ext_hook/2,
      fun ext_key/1
     ).

fields_function_(FName, DefaultOpts, #record_model{fields=Fields}, ArgModifierFun, ResultKeyFun) ->
    SafeFName = ?atom_join(FName, safe),
    SafeBinaryKeyFName = ?atom_join(SafeFName, ext_key),
    UnsafeFName = ?atom_join(FName, unsafe),
    UnsafeBinaryKeyFName = ?atom_join(UnsafeFName, ext_key),
    Fun2 = ?function(FName,
                     [?clause([?var('Fields'), ?var('Model')], none,
                              [?apply(FName, [?var('Fields'), ?abstract(DefaultOpts), ?var('Model')])])]),
    Fun3 =
        ?function(FName,
                  [?clause([?var('Fields'), ?var('Opts'), ?var('Model')], none,
                           [?match(?var('IsUnsafe'), ?apply(lists, member, [?atom(unsafe), ?var('Opts')])),
                            ?match(?var('KeyIsBinary'), ?apply(lists, member, [?atom(ext_key), ?var('Opts')])),
                            ?match(?var('Fun'),
                                   ?cases(?tuple([?var('IsUnsafe'), ?var('KeyIsBinary')]),
                                          [?clause([?tuple([?atom(true), ?atom(true)])], none,
                                                   [implicit_func(UnsafeBinaryKeyFName, 3)]),
                                           ?clause([?tuple([?atom(true), ?atom(false)])], none,
                                                   [implicit_func(UnsafeFName, 3)]),
                                           ?clause([?tuple([?atom(false), ?atom(true)])], none,
                                                   [implicit_func(SafeBinaryKeyFName, 3)]),
                                           ?clause([?tuple([?atom(false), ?atom(false)])], none,
                                                   [implicit_func(SafeFName, 3)])])),
                            ?match(?var('IgnoreUnknown'), ?apply(lists, member, [?atom(ignore_unknown), ?var('Opts')])),
                            ?match(?var('Fun2'), ?func([?clause([?var('F')], none,
                                                                [?apply_(?var('Fun'), [?var('F'), ?var('Model'), ?var('IgnoreUnknown')])])])),
                            ?apply(tq_transform_utils, error_writer_map,
                                   [?var('Fun2'), ?var('Fields')])])]),
    Ok = fun(#record_field{name=FieldName} = F, KeyFun) ->
                 ?clause([KeyFun(F), ?var('Model'), ?var('_Opts')], none,
                         [?ok(?tuple([ResultKeyFun(F), ArgModifierFun(F, ?apply(FieldName, [?var('Model')]))]))])
         end,
    Err = fun(FieldNameAst, Reason) ->
                  ?clause([FieldNameAst, ?var('_Model'), ?var('_Opts')], none,
                          [?error(?tuple([FieldNameAst, ?atom(Reason)]))])
          end,
    AccessField = fun(#record_field{mode=Mode, getter=Getter} = F, Safe, KeyFun) ->
                          case Safe of
                              safe ->
                                  case Mode of
                                      #access_mode{r=true} when Getter =/= false ->
                                          Ok(F, KeyFun);
                                      #access_mode{w=true} ->
                                          Err(KeyFun(F), forbidden);
                                      _ ->
                                          Err(KeyFun(F), unknown)
                                  end;
                              unsafe ->
                                  case Mode of
                                      #access_mode{sr=true} when Getter =/= false ->
                                          Ok(F, KeyFun);
                                      _ ->
                                          Err(KeyFun(F), forbidden)
                                  end
                          end
                  end,
    DefaultClause = [?clause([?var('_Field'), ?var('_Model'), ?atom(true)], none,
                              [?atom(ok)]),
                      ?clause([?var('Field'), ?var('_Model'), ?atom(false)], none,
                              [?error(?tuple([?var('Field'), ?atom(unknown)]))])],
    FieldsFun_ = fun(Name, Safe, KeyFun) ->
                         ?function(Name,
                                   [AccessField(F, Safe, KeyFun) || F <- Fields]
                                   ++ DefaultClause)
                 end,
    FunSafe_ = FieldsFun_(SafeFName, safe, fun int_key/1),
    FunSafeBinaryKey_ = FieldsFun_(SafeBinaryKeyFName, safe, fun ext_key/1),
    FunUnsafe_ = FieldsFun_(UnsafeFName, unsafe, fun int_key/1),
    FunUnsafeBinaryKey_ = FieldsFun_(UnsafeBinaryKeyFName, unsafe, fun ext_key/1),
    {[Fun2, Fun3], [FunSafe_, FunSafeBinaryKey_, FunUnsafe_, FunUnsafeBinaryKey_]}.

build_internal_functions(Model) ->
    Funs = [
            changed_fields_function(Model),
            field_from_ext(Model),
            field_to_ext(Model)
           ],
    Exports = ?export_funs(Funs),
    {Exports, Funs}.

changed_fields_function(#record_model{module=Module, fields=Fields}) ->
    AllowedFields = [F#record_field.name || F <- Fields,
                                            F#record_field.stores_in_record,
                                            F#record_field.setter,
                                            F#record_field.mode#access_mode.sw],
    ListAst = ?list([?tuple([?atom(F),
                             ?access(?var('Model'), Module, F),
                             ?access(?var('Model'), Module, ?changed_suffix(F))
                            ])
                     || F <- AllowedFields]),
    ?function(get_changed_fields,
              [?clause([?var('Model')], none,
                       [?list_comp(?tuple([?var('Name'), ?var('Val')]),
                                   [?generator(?tuple([?var('Name'), ?var('Val'), ?atom(true)]),
                                               ListAst)]
                                  )])]).

field_from_ext(#record_model{fields=Fields}) ->
    Valid = fun(F, Var) ->
                    case F#record_field.validators =:= [] of
                        true ->
                            ?ok(Var);
                        false ->
                            ?cases(?apply_(?apply(validator, [int_key(F)]), [Var]),
                                   [?clause([?atom('ok')], none,
                                            [?ok(Var)]),
                                    ?clause([?var('Err')], none,
                                            [?var('Err')])])
                    end
            end,
    IgnoreNullHook =
        fun (F, Var, Ast) ->
                ?cases(Var,
                       [?clause([?atom(null)], none, [Valid(F, Var)]),
                        ?clause([?underscore], none, [Ast])])
        end,
    ?function(field_from_ext,
              [?clause([int_key(F), ?var('Bin')], none,
                       [
                        IgnoreNullHook(
                          F,
                          ?var('Bin'),
                          case F#record_field.from_ext of
                              none ->
                                  Valid(F, ?var('Bin'));
                              FromExt ->
                                  ?cases(function_call(FromExt, [?var('Bin')]),
                                         [?clause([?ok(?var('Val'))], none,
                                                  [Valid(F, ?var('Val'))]),
                                          ?clause([?var('Err')], none,
                                                  [?var('Err')])])
                          end)
                       ]) || F <- Fields]).

field_to_ext(#record_model{fields=Fields}) ->
    ?function(field_to_ext,
              [?clause([int_key(F), ?var('Val')], none,
                       [to_ext_hook(F, ?var('Val'))]) || F <- Fields]).

build_get_field_name(#record_model{fields=Fields}) ->
    ErrorClause =
        [?clause([?var('FieldName'), ?var('_Mode')], none,
                 [?error(?tuple([?var('FieldName'), ?atom(unknown)]))])],
    FieldClauses =
        fun(Transform) ->
                [
                 begin
                     Name = Transform(F),
                     ?clause([Name, ?var('Mode')], none,
                             [?cases(?var('Mode'),
                                     [?clause([?atom(M)], none,
                                              [?ok(int_key(F))])
                                      || M <- ?MODES, tq_transform_utils:check_acl(
                                                        F#record_field.mode,
                                                        tq_transform_utils:mode_to_acl(M))
                                     ] ++ [?clause([?var('_')], none,
                                                   [?error(?tuple([Name, ?atom(unknown)]))])])]
                            )
                 end || F <- Fields
                 ] ++ ErrorClause
        end,
    AtomFun =
        ?function('$get_field_name', FieldClauses(fun int_key/1)),
    BinaryFun =
        ?function('$get_field_name_ext_key', FieldClauses(fun ext_key/1)),
    OptsFunc =
        ?func(
           [
            ?clause(
               [
                ?tuple([?atom(mode), ?var('Mode')]),
                ?tuple([?var('_OldMode'), ?var('BinKey')])
               ], none,
               [?ok(?tuple([?var('Mode'), ?var('BinKey')]))]),
            ?clause(
               [
                ?atom(ext_key),
                ?tuple([?var('Mode'), ?var('_BinKey')])
               ], none,
               [?ok(?tuple([?var('Mode'), ?atom(true)]))]),
            ?clause(
               [
                ?var('Opt'),
                ?var('_State')
               ], none,
               [?error(?tuple([?var('Opt'), ?atom(unknown_option)]))])
           ]),
    DefaultOpts = ?tuple([?atom(r), ?atom(false)]),

    MainFun =
        ?function(
           get_field_name,
           [?clause(
               [?var('FieldName'), ?var('Opts')], none,
               [
                ?cases(?apply(tq_transform_utils, error_writer_foldl,
                              [OptsFunc, DefaultOpts, ?var('Opts')]),
                       [?clause(
                           [?ok(?tuple([?var('AccessMode'), ?var('BinaryKey')]))], none,
                           [?cases(?var('BinaryKey'),
                                   [
                                    ?clause([?atom(true)], none,
                                            [?apply('$get_field_name_ext_key',
                                                    [?var('FieldName'), ?var('AccessMode')])]),
                                    ?clause([?atom(false)], none,
                                            [?apply('$get_field_name',
                                                    [?var('FieldName'), ?var('AccessMode')])])
                                   ])]),
                        ?clause([?var('Err')], none, [?var('Err')])
                       ])
               ])
           ]),
    Public = [MainFun],
    Private = [AtomFun, BinaryFun],
    Funs = Public ++ Private,

    Exports = ?export_funs(Public),
    {Exports, Funs}.

build_validators(#record_model{module=Module, fields=Fields, validators=Validators}) ->
    FieldsWithValidator =
        [F || F <- Fields,
              F#record_field.getter orelse F#record_field.stores_in_record],
    UnknownFieldClause =
        ?clause([?var('F')], none,
                [
                 ?apply(throw, [?tuple([?atom(unknown_field), ?var('F')])])
                ]),
    ValidatorFun = ?function(validator,
                             [?clause([int_key(F)], none,
                                      [validator(F#record_field.validators,
                                                 F#record_field.is_required)])
                              || F <- FieldsWithValidator
                             ] ++ [UnknownFieldClause]),
    AppyUtilsValid = ?apply(tq_transform_utils, valid, [?var('Data')]),
    ValidModelAst = case Validators of
                        [] ->
                            AppyUtilsValid;
                        _ ->
                            ?cases(AppyUtilsValid,
                                   [?clause([?atom(ok)], none,
                                            [fold_validators(Validators, ?var('Model'))]),
                                    ?clause([?error(?var('Reason'))], none,
                                            [?error(?var('Reason'))])])
                    end,
    ValidFun =
        ?function(valid,
                  [?clause([?var('Model')], none,
                           [?match(?var('Fields'),
                                   ?list([?tuple(
                                             [int_key(F),
                                              case F#record_field.stores_in_record of
                                                  true ->
                                                      ?access(?var('Model'), Module, F#record_field.name);
                                                  false ->
                                                      ?apply(F#record_field.name, [])
                                              end,
                                              ?atom(F#record_field.is_required),
                                              ?access(?var('Model'), Module, ?changed_suffix(F#record_field.name))
                                             ])
                                          || F <- FieldsWithValidator,
                                             F#record_field.setter
                                         ])),
                            ?match(?var('Data'),
                                   ?cases(?access(?var('Model'), Module, '$is_new$'),
                                          [?clause([?atom(true)], none,
                                                  [?list_comp(
                                                      ?tuple([
                                                              ?var('Name'),
                                                              ?apply(validator, [?var('Name')]),
                                                              ?var('Value')
                                                             ]),
                                                      [?generator(
                                                          ?tuple([
                                                                  ?var('Name'),
                                                                  ?var('Value'),
                                                                  ?var('IsRequired'),
                                                                  ?var('IsChanged')
                                                                 ]),
                                                          ?var('Fields')),
                                                       ?ORELSE(?var('IsRequired'), ?var('IsChanged'))
                                                      ])]),
                                           ?clause([?atom(false)], none,
                                                   [?list_comp(
                                                       ?tuple([
                                                               ?var('Name'),
                                                               ?apply(validator, [?var('Name')]),
                                                               ?var('Value')
                                                              ]),
                                                       [?generator(
                                                           ?tuple([
                                                                   ?var('Name'),
                                                                   ?var('Value'),
                                                                   ?underscore,
                                                                   ?atom(true)
                                                                  ]),
                                                           ?var('Fields'))])])
                                          ])),
                            ValidModelAst
                           ])]),
    Funs = [ValidatorFun, ValidFun],
    Exports = ?export_funs(Funs),
    {Exports, Funs}.

validator(Validators, IsRequired) ->
    NullReq_clause = ?clause([?atom(null)], none, [?error(?atom(required))]),
    IgnoreNull_clause = ?clause([?atom(null)], none, [?atom(ok)]),
    Main_clause =
        case Validators of
            [] ->
                ?clause([?underscore], none, [?atom(ok)]);
            _ ->
                Var = ?var('Val'),
                ?clause([Var], none, [fold_validators(Validators, Var)])
        end,
    ClausesOpts =
        [
         {IsRequired, NullReq_clause},
         {not IsRequired, IgnoreNull_clause},
         {true, Main_clause}
        ],
    Clauses = [Val || {true, Val} <- ClausesOpts],
    ?func(Clauses).

fold_validators([Fun], Var) ->
    function_call(Fun, [Var]);
fold_validators([Fun|Rest], Var) ->
    ?cases(function_call(Fun, [Var]),
           [?clause([?atom(ok)], none,
                    [fold_validators(Rest, Var)]),
            ?clause([?error(?var('Reason'))], none,
                    [?error(?var('Reason'))])]).

build_is_changed(#record_model{module=Module, fields=Fields}) ->
    UnknownFieldClause =
        ?clause([?var('F'), ?underscore], none,
                [
                 ?apply(throw, [?tuple([?atom(unknown_field), ?var('F')])])
                ]),
    Fun = ?function(is_changed,
                    [?clause([int_key(F), ?var('Model')], none,
                             [?access(?var('Model'), Module, ?changed_suffix(F#record_field.name))])
                     || F <- Fields, F#record_field.stores_in_record] ++ [UnknownFieldClause]),
    Export = ?export_fun(Fun),
    {[Export], [Fun]}.

%% Internal helpers.
int_key(F) -> ?atom(F#record_field.name).
ext_key(F) -> ?abstract(F#record_field.ext_name).

function_call({Mod, Fun, FunArgs}, Args) ->
    FunArgs2 = [erl_syntax:abstract(A) || A <- FunArgs],
    ?apply(Mod, Fun, FunArgs2++Args);
function_call({Fun, FunArgs}, Args) when is_list(FunArgs) ->
    FunArgs2 = [erl_syntax:abstract(A) || A <- FunArgs],
    ?apply(Fun, FunArgs2++Args);
function_call({Mod, Fun}, Args) ->
    ?apply(Mod, Fun, Args);
function_call(Fun, Args) ->
    ?apply(Fun, Args).

%% Ast diff bugfix
implicit_func(FunName, Arity) ->
    Args = [?var("A"++integer_to_list(I)) || I <- lists:seq(1, Arity)],
    ?func([?clause(Args, none, [?apply(FunName, Args)])]).
