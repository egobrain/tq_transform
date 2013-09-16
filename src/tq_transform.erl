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

-module(tq_transform).

-export([parse_transform/3,
         print_module/1,
         pretty_print/1]).

-include("include/ast_helpers.hrl").

-record(state, {
          module,
          plugins
         }).

-define(DBG(F, D), io:format("~p:~p "++F++"~n", [?FILE, ?LINE | D])).

parse_transform(Ast, _Options, [])->
    Ast;
parse_transform(Ast, _Options, Plugins) ->
    try
        State = #state{plugins=Plugins},
        %% ?DBG("~n~p~n=======~n", [Ast]),
        %% ?DBG("~n~s~n=======~n", [pretty_print(Ast)]),
        case error_map_foldl(fun transform_node/2, State, Ast) of
            {error, {Ast2, _State2}} ->
                lists:flatten(Ast2);
            {ok, {Ast2, State2}} ->
                {ModuleBlock, InfoBlock, FunctionsBlock} = split_ast(Ast2),
                Ast3 = case normalize_models(State2) of
                           {ok, State3} ->
                               {IBlock, FBlock} = build_models(State3),
                               revert(lists:flatten([ModuleBlock, IBlock, InfoBlock, FBlock, FunctionsBlock]));
                           {error, Reasons} ->
                               Ast2 ++ [global_error_ast(1, R) || R <- Reasons]
                       end,
                lists:flatten(lists:filter(fun(Node)-> Node =/= nil end, Ast3))
        end
        %% ?DBG("~n~p~n<<<<~n", [Ast3]),
        %% ?DBG("~n~s~n>>>>~n", [pretty_print(Ast3)]),
    catch T:E ->
            Reason = io_lib:format("~p:~p | ~p ~n", [T, E, erlang:get_stacktrace()]),
            [global_error_ast(1, Reason) | Ast]
    end.

error_map_foldl(Fun, State, List) ->
    FoldFun = fun(A, {St, IsError}) ->
                      {Res, {Data, St2}} = Fun(A, St),
                      IsError2 = case Res of
                                     ok -> IsError;
                                     error -> error
                                 end,
                      {Data, {St2, IsError2}}
              end,
    {List2, {State2, IsError}} = lists:mapfoldl(FoldFun, {State, ok}, List),
    {IsError, {List2, State2}}.


split_ast(Ast) ->
    {ok, {ModuleBlock, RestBlock}} = ast_split_with(fun({attribute, _, module, _}) -> true;
                                                       (_) -> false
                                                    end, Ast, 'after'),
    {ok, {InfoBlock, FunctionsBlock}} = ast_split_with(fun({function, _, _, _, _}) -> true;
                                                          (_) -> false
                                                       end, RestBlock, 'before'),
    {ModuleBlock, InfoBlock, FunctionsBlock}.

transform_node(Node={attribute, Line, model, Opts}, State) ->
    case is_list(Opts) of
        true ->
            case model_options(Opts, State) of
                {ok, State2} ->
                    {ok, {Node, State2}};
                {error, Reason} ->
                    {error, {global_error_ast(Line, Reason), State}}
            end;
        false ->
            Node2 = global_error_ast(Line, "Wrong model spec"),
            {ok, {Node2, State}}
    end;

transform_node(Node={attribute, _Line, module, Module}, #state{plugins=Plugins}=State) ->
    Plugins2 = [{P, P:create_model(Module)} || P <- Plugins],
    State2 = State#state{plugins = Plugins2},
    {ok, {Node, State2}};
transform_node(Node={attribute, Line, field, FieldOpts}, State) ->
    case FieldOpts of
        {Name, Opts} when is_atom(Name) andalso is_list(Opts) ->
            case create_fields(Name, Opts, State) of
                {ok, Fields} ->
                    Plugins = [{P, P:set_field(F, S)} || {P, F, S} <- Fields],
                    State2 = State#state{plugins=Plugins},
                    {ok, {Node, State2}};
                {error, Reasons} ->
                    Node2 = [begin
                                 io:format("ERROR: ~s~n", [R]),
                                 global_error_ast(Line, R)
                             end || R <- Reasons],
                    {error, {Node2, State}}
            end;
        _ ->
            {error, {error_ast(Line, "Wrong field spec"), State}}
    end;
transform_node(Node, State) ->
    {ok, {Node, State}}.

create_fields(Name, Opts, #state{plugins=Plugins}) ->
    Fields0 = [{P, P:create_field(Name), S} || {P, S} <- Plugins],
    OptionFun = fun(Val, Fields) ->
                        case normalize_option(Val) of
                            {ok, {OptionName, Data}} ->
                                try_field_option(OptionName, Data, Fields);
                            {error, _} = Err -> Err
                        end
                end,
    case tq_transform_utils:error_writer_foldl(OptionFun, Fields0, Opts) of
        {ok, Fields2} ->
            normalize_fields(Fields2);
        {error, _} = Err ->
            Err
    end.

model_options(Opts, #state{plugins=Plugins}=State) ->
    OptionFun = fun(Val, Models) ->
                        case normalize_option(Val) of
                            {ok, {OptionName, Data}} ->
                                try_model_option(OptionName, Data, Models);
                            {error, _} = Err -> Err
                        end
                end,
    case tq_transform_utils:error_writer_foldl(OptionFun, Plugins, Opts) of
        {ok, Plugins2} ->
            State2 = State#state{plugins=Plugins2},
            {ok, State2};
        {error, _} = Err -> Err
    end.

try_field_option(Option, Data, Fields) ->
    try_field_option(Option, Data, Fields, [], false).

try_field_option(Option, _Data, [], Acc, IsOptionKnown) ->
    case IsOptionKnown of
        true ->
            {ok, lists:reverse(Acc)};
        _ ->
            {error, "Unknown field option " ++ atom_to_list(Option)}
    end;
try_field_option(Option, Data, [{P, F, S} | Rest], Acc, IsOptionKnown) ->
    case P:field_option(Option, Data, F) of
        {ok, F2} ->
            try_field_option(Option, Data, Rest, [{P, F2, S} | Acc], true);
        false ->
            try_field_option(Option, Data, Rest, [{P, F, S} | Acc], IsOptionKnown);
        {error, _} = Err -> Err
    end.

try_model_option(Option, Data, Models) ->
    try_model_option(Option, Data, Models, [], false).

try_model_option(Option, _Data, [], Acc, IsOptionKnown) ->
    case IsOptionKnown of
        true ->
            {ok, lists:reverse(Acc)};
        false ->
            {error, "Unknown model option " ++ atom_to_list(Option)}
    end;
try_model_option(Option, Data, [{P, M} | Rest], Acc, IsOptionKnown) ->
    case P:model_option(Option, Data, M) of
        {ok, M2} ->
            try_model_option(Option, Data, Rest, [{P, M2} | Acc], true);
        false ->
            try_model_option(Option, Data, Rest, [{P, M} | Acc], IsOptionKnown);
        {error, _} = Err -> Err
    end.


normalize_option({Name, Value}) when is_atom(Name) ->
    {ok, {Name, Value}};
normalize_option(Name) when is_atom(Name) ->
    {ok, {Name, true}};
normalize_option(Val) ->
    {error, io_lib:format("Wrong option spec: ~p", [Val])}.

%% Field rules.

normalize_fields(Fields) ->
    NormalizeFun = fun({P, F, S}) ->
                           case P:normalize_field(F) of
                               {ok, F2} ->
                                   {ok, {P, F2, S}};
                               {error, _} = Err ->
                                   Err
                           end
                   end,
    tq_transform_utils:error_writer_map(NormalizeFun, Fields).

%% Validators.

normalize_models(#state{plugins=Plugins}=State) ->
    ValidFun = fun({P, M}) ->
                       case P:normalize_model(M) of
                           {ok, M2} -> {ok, {P, M2}};
                           {error, _} = Err -> Err
                       end
               end,
    case tq_transform_utils:error_writer_map(ValidFun, Plugins) of
        {ok, Plugins2} ->
            State2 = State#state{plugins=Plugins2},
            {ok, State2};
        {error, _} = Err -> Err
    end.

%% Builder.

build_models(#state{plugins=Plugins}=State) ->
    MetaBlock = meta_functions(State),
    lists:foldl(fun({P, M}, {Exports, Funs}) ->
                        {Es, Fs} = P:build_model(M),
                        {[Es | Exports], [Fs | Funs]}
                end, MetaBlock, Plugins).

meta_functions(#state{plugins=Plugins}) ->
    MetaClauses = lists:flatten([P:meta_clauses(M) || {P, M} <- Plugins]),
    case MetaClauses of
        [] ->
            {[], []};
        _ ->
            MetaFun1 = ?function('$meta', MetaClauses),
            MetaFun2 = ?function('$meta', [?clause([?var('Arg'), ?underscore], none,
                                                   [?apply('$meta', [?var('Arg')])])]),
            MetaFuns = [MetaFun1, MetaFun2],
            Exports = ?export_funs(MetaFuns),
            {[Exports], MetaFuns}
    end.

%% Internal helpers.

-spec ast_split_with(Fun, List, 'before') -> {List1, List2} when
      Fun :: fun((E) -> boolean()),
      List :: [E], List1 :: [E], List2 :: [E];
                    (Fun, List, 'after') ->   {List1, List2} | {error, not_found} when
      Fun :: fun((E) -> boolean()),
      List :: [E], List1 :: [E], List2 :: [E].
ast_split_with(Fun, List, Opt) ->
    ast_split_with(Fun, List, Opt, []).
ast_split_with(Fun, [E | Rest] = List, Opt, Acc) ->
    case Fun(E) of
        true ->
            case Opt of
                'before' ->
                    {ok, {lists:reverse(Acc), List}};
                'after' ->
                    {ok, {lists:reverse([E | Acc]), Rest}}
            end;
        false ->
            ast_split_with(Fun, Rest, Opt, [E | Acc])
    end;
ast_split_with(_Fun, [], Opt, Acc) ->
    case Opt of
        'before' ->
            {ok, {lists:reverse(Acc), []}};
        'after' ->
            {error, not_found}
    end.

error_ast(Line, Reason) ->
    {error, {Line, Reason}}.

global_error_ast(Line, Reason) ->
    {error, {Line, erl_parse, Reason}}.

revert(Tree) ->
    [erl_syntax:revert(T) || T <- lists:flatten(Tree)].

pretty_print(Forms0) ->
    Forms = epp:restore_typed_record_fields(revert(Forms0)),
    [io_lib:fwrite("~s~n",
                   [lists:flatten([erl_pp:form(Fm) ||
                                      Fm <- Forms])])].

print_module(Module) ->
    BeamFileName = code:which(Module),
    case beam_lib:chunks(BeamFileName, [abstract_code]) of
        {ok, {_, [{abstract_code, {raw_abstract_v1,Forms}}]}} ->
            Code = pretty_print(Forms),
            io:format("~s~n", [Code]);
        Error ->
            Error
    end.

%% Tests.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

ast_split_test_() ->
    Tests = [
             {[{1, 1}, {2, 2}, {3, 3}], {'before', 1}, {ok, {[], [{1, 1}, {2, 2}, {3, 3}]}}},
             {[{1, 1}, {2, 2}, {3, 3}], {'after',  1}, {ok, {[{1, 1}], [{2, 2}, {3, 3}]}}},
             {[{1, 1}, {2, 2}, {3, 3}], {'before', 2}, {ok, {[{1, 1}], [{2, 2}, {3, 3}]}}},
             {[{1, 1}, {2, 2}, {3, 3}], {'after',  2}, {ok, {[{1, 1}, {2, 2}], [{3, 3}]}}},
             {[{1, 1}, {2, 2}, {3, 3}], {'before', 3}, {ok, {[{1, 1}, {2, 2}], [{3, 3}]}}},
             {[{1, 1}, {2, 2}, {3, 3}], {'after',  3}, {ok, {[{1, 1}, {2, 2}, {3, 3}], []}}},
             {[{1, 1}, {2, 2}, {3, 3}], {'before', 4}, {ok, {[{1, 1}, {2, 2}, {3, 3}], []}}},
             {[{1, 1}, {2, 2}, {3, 3}], {'after',  4}, {error, not_found}}
            ],
    F = fun(D, {Opt, Key},  R) ->
                Fun = fun(E) -> element(1, E) =:= Key end,
                R = ast_split_with(Fun, D, Opt)
        end,
    [fun() -> F(List, Opts, Res) end || {List, Opts, Res} <- Tests].

-endif.
