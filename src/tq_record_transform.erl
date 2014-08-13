%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(tq_record_transform).

-behavior(tq_transform_plugin).

-include("include/access_mode.hrl").
-include("include/record_model.hrl").

-export([parse_transform/2]).

-export([
         create_model/2,
         model_option/3,
         normalize_model/1,
         set_globals/2,
         build_model/1,

         create_field/1,
         field_option/3,
         normalize_field/2,
         set_field/2,

         meta_clauses/1
        ]).

-export([g/3]).

-define(CONVERTER_RULES_FILE, "converter.map").

parse_transform(Ast, CompileOpts) ->
    try
        tq_transform:parse_transform(Ast, CompileOpts, [?MODULE], [])
    catch T:E ->
            Reason = io_lib:format("~p:~p | ~p ~n", [T, E, erlang:get_stacktrace()]),
            [{error, {1, erl_parse, Reason}} | Ast]
    end.

%% API.

g(field, Field, Model) ->
    case lists:keyfind(Field, #record_field.name, Model#record_model.fields) of
        false ->
            {error, undefined};
        PluginState ->
            {ok, PluginState}
    end.

%% Model.

create_model(Module, Opts) ->
    tq_transform_utils:error_map_funs(
      [
       fun(M) -> set_global_opts(M, Opts) end,
       fun set_default_converter_rules/1
      ],
      #record_model{module=Module}).

set_global_opts(Model, Opts) ->
    tq_transform_utils:error_writer_foldl(
      fun set_global_opt/2,
      Model,
      Opts).

set_global_opt({converter_rules, ConverterRules}, Model) ->
    {ok, Model#record_model{converter_rules=ConverterRules}};
set_global_opt(_, Model) ->
    {ok, Model}.

set_default_converter_rules(#record_model{converter_rules=undefined}=Model) ->
    case code:priv_dir(tq_transform) of
        {error, badname} = Err ->
            Err;
        PrivDir ->
            Path = filename:join(PrivDir, ?CONVERTER_RULES_FILE),
            case file:consult(Path) of
                {ok, Data} ->
                    Model2 = Model#record_model{converter_rules=Data},
                    {ok, Model2};
                {error, Reason} ->
                    {ok, Cwd} = file:get_cwd(),
                    Description =
                        lists:flatten(
                          io_lib:format(
                            "Config file: ~s/~s error: ~p",
                            [Cwd, Path, Reason])),
                    {error, Description}
            end
    end;
set_default_converter_rules(Model) ->
    {ok, Model}.

model_option(validators, NewValidators, #record_model{validators=Validators}=Model) ->
    Model2 = Model#record_model{validators = Validators ++ NewValidators},
    {ok, Model2};
model_option(_Option, _Val, _Model) ->
    false.

normalize_model(Model) ->
    {ok, Model}.

set_globals(_Globals, Model) ->
    {ok, Model}.

build_model(Model) ->
    {Exports, Funs} = tq_record_generator:build_model(Model),
    {lists:reverse(Exports), lists:reverse(Funs)}.

%% Fields.

create_field(Name) ->
    #record_field{name=Name}.

field_option(ext_name, ExtName, Field) ->
    Field2 = Field#record_field{ext_name = ExtName},
    {ok, Field2};
field_option(required, Value, Field) ->
    Field2 = Field#record_field{is_required = Value},
    {ok, Field2};
field_option(default, DefaultValue, Field) ->
    Field2 = Field#record_field{default={value, DefaultValue}},
    {ok, Field2};
field_option(default_func, DefaultFunc, Field) ->
    Field2 = Field#record_field{default={func, DefaultFunc}},
    {ok, Field2};
field_option(mode, Mode, Field) ->
    Field2 = Field#record_field{mode = tq_transform_utils:mode_to_acl(Mode)},
    {ok, Field2};
field_option(type, Type, Field) ->
    Field2 = Field#record_field{type = Type},
    {ok, Field2};
field_option(from_ext, FromExtFun, Field) ->
    Field2 = Field#record_field{from_ext = FromExtFun},
    {ok, Field2};
field_option(to_ext, ToExtFun, Field) ->
    Field2 = Field#record_field{to_ext = ToExtFun},
    {ok, Field2};
field_option(get, Getter, Field) ->
    Field2 = Field#record_field{getter = Getter},
    {ok, Field2};
field_option(set, Setter, Field) ->
    Field2 = Field#record_field{setter = Setter},
    {ok, Field2};
field_option(record, StoresInRecord, Field) ->
    Field2 = Field#record_field{stores_in_record = StoresInRecord},
    {ok, Field2};
field_option(validators, NewValidators, #record_field{validators=Validators}=Field) ->
    Field2 = Field#record_field{validators = Validators ++ NewValidators},
    {ok, Field2};
field_option(_Option, _Val, _Field) ->
    false.

normalize_field(Field, #record_model{converter_rules=ConverterRules}) ->
    Rules = [
             fun default_ext_name/1,
             fun access_mode_getter_rule/1,
             fun access_mode_setter_rule/1,
             fun get_set_record_rule/1,
             fun(F) -> set_convert_rules(F, ConverterRules) end
            ],
    tq_transform_utils:error_writer_foldl(fun(R, F) -> R(F) end, Field, Rules).

set_field(Field, #record_model{fields=Fields} = Model) ->
    Model#record_model{fields=[Field | Fields]}.

%% Meta.

meta_clauses(Model) ->
    tq_record_generator:meta_clauses(Model).

%% Rules.

set_convert_rules(#record_field{
                     type=Type,
                     from_ext=FromExt,
                     to_ext=ToExt,
                     validators=ValidatorHooks
                    }=F,
                  ConverterRules) ->
    case lists:keyfind(Type, 1, ConverterRules) of
        {_, From, To, Validators} ->
            F2 =
                F#record_field{
                  from_ext =
                      case FromExt of
                          undefined -> From;
                          _ -> FromExt
                      end,
                  to_ext =
                      case ToExt of
                          undefined -> To;
                          _ -> ToExt
                      end,
                  validators = Validators ++ ValidatorHooks
                 },
            {ok, F2};
        false ->
            Reason = io_lib:format("Unknown type: ~p. from_ext required", [Type]),
            {error, Reason}
    end.

get_set_record_rule(Field=#record_field{stores_in_record=false, getter=Getter, setter=Setter}) ->
    case {Getter, Setter} of
        {true, true} ->
            {error, "Storing in record required for default getter and setter; "};
        {true, _} ->
            {error, "Storing in record required for default getter; "};
        {_, true} ->
            {error, "Storing in record required for default setter; "};
        {_, _} ->
            {ok, Field}
    end;
get_set_record_rule(Field) ->
    {ok, Field#record_field{stores_in_record=true}}.

default_ext_name(Field=#record_field{ext_name=undefined, name=Name}) ->
    ExtName = atom_to_binary(Name),
    {ok, Field#record_field{ext_name=ExtName}};
default_ext_name(Field) ->
    {ok, Field}.

access_mode_getter_rule(Field=#record_field{mode=#access_mode{sr=false}}) ->
    {ok, Field#record_field{getter=false}};
access_mode_getter_rule(Field) ->
    {ok, Field}.

access_mode_setter_rule(Field=#record_field{mode=#access_mode{sw=false}}) ->
    {ok, Field#record_field{setter=false}};
access_mode_setter_rule(Field) ->
    {ok, Field}.

%% Internal helpers.

atom_to_binary(Atom) ->
    list_to_binary(atom_to_list(Atom)).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_set_record_rule_test_() ->
    C = fun({Stored, Getter, Setter}) -> #record_field{stores_in_record=Stored, getter=Getter, setter=Setter} end,
    Default = fun(undefined) -> true;
                 (V) -> V
              end,
    Values = [true, false, custom],
    Tests1 = [{C({St, G, S}), {ok, C({Default(St), Default(G), Default(S)})}} || St <- [undefined, true], G <- Values, S <- Values],

    %% Test case when stores_in_record manually set to false.
    Tests2 = [{C({false, G, S}), case {Default(G), Default(S)} of
                                     {true, true} ->
                                         {error, "Storing in record required for default getter and setter; "};
                                     {true, _} ->
                                         {error, "Storing in record required for default getter; "};
                                     {_, true} ->
                                         {error, "Storing in record required for default setter; "};
                                     {G1, S1} ->
                                         {ok, C({false, G1, S1})}
                                 end} || G <- Values, S <- Values],
    Tests = Tests1 ++ Tests2,
    F = fun(From, To) ->
                ?assertEqual(To, get_set_record_rule(From))
        end,
    [fun() -> F(From, To) end || {From, To} <- Tests].

-endif.
