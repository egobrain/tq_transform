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

-include("include/record_model.hrl").

-export([parse_transform/2]).

-export([create_model/1,
         model_option/3,
         normalize_model/1,
         set_globals/2,
         build_model/1,

         create_field/1,
         field_option/3,
         normalize_field/1,
         set_field/2,

         meta_clauses/1
        ]).

-export([g/3]).

parse_transform(Ast, Options) ->
    try
        tq_transform:parse_transform(Ast, Options, [?MODULE])
    catch T:E ->
            Reason = io_lib:format("~p:~p | ~p ~n", [T, E, erlang:get_stacktrace()]),
            [{error, {1, erl_parse, Reason}} | Ast]
    end.

%% API.

g(field, Field, Model) ->
    case lists:keyfind(Field, #record_field.name, Model#record_model.fields) of
        {_, PluginState} ->
            {ok, PluginState};
        false ->
            {error, undefined}
    end.


%% Model.

create_model(Module) ->
    #record_model{module=Module}.

model_option(init, InitFuns,  Model) ->
    Model2 = Model#record_model{init_funs = to_list(InitFuns)},
    {ok, Model2};
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

field_option(required, Value, Field) ->
    Field2 = Field#record_field{is_required = Value},
    {ok, Field2};
field_option(default, DefaultValue, Field) ->
    Field2 = Field#record_field{default_value=DefaultValue},
    {ok, Field2};
field_option(mode, Mode, Field) ->
    Field2 = Field#record_field{mode = mode_to_acl(Mode)},
    {ok, Field2};
field_option(type, Type, Field) ->
    Field2 = Field#record_field{type = Type},
    {ok, Field2};
field_option(type_constructor, TypeConstructor, Field) ->
    Field2 = Field#record_field{type_constructor = TypeConstructor},
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
field_option(init, InitFuns, Field) ->
    Field2 = Field#record_field{init_funs = to_list(InitFuns)},
    {ok, Field2};
field_option(_Option, _Val, _Field) ->
    false.

normalize_field(Field) ->
    Rules = [
             fun access_mode_getter_rule/1,
             fun access_mode_setter_rule/1,
             fun get_set_record_rule/1,
             fun type_constructor_rule/1,
             fun default_validators_rule/1
            ],
    tq_transform_utils:error_writer_foldl(fun(R, F) -> R(F) end, Field, Rules).

set_field(Field, #record_model{fields=Fields} = Model) ->
    Model#record_model{fields=[Field | Fields]}.

%% Meta.

meta_clauses(Model) ->
    tq_record_generator:meta_clauses(Model).

%% Rules.

get_set_record_rule(Field=#record_field{stores_in_record=false, getter=Getter, setter=Setter}) ->
    case {Getter, Setter} of
        {true, true} ->
            {error, "Storing in record required for default getter and setter"};
        {true, _} ->
            {error, "Storing in record required for default getter"};
        {_, true} ->
            {error, "Storing in record required for default setter"};
        {_, _} ->
            {ok, Field}
    end;
get_set_record_rule(Field) ->
    {ok, Field#record_field{stores_in_record=true}}.

type_constructor_rule(#record_field{type_constructor=undefined, type=Type}=Field) ->
    case type_constructor(Type) of
        {ok, TypeConstructor} ->
            Field2 = Field#record_field{type_constructor=TypeConstructor},
            {ok, Field2};
        {error, undefined} ->
            Reason = lists:flatten(io_lib:format("type_constructor required for type: ~p", [Type])),
            {error, Reason}
    end;
type_constructor_rule(Field) ->
    {ok, Field}.

default_validators_rule(#record_field{type=non_neg_integer, validators=Validators}=Field) ->
    NonNegValidator = {tq_transform_utils, more_or_eq, [0]},
    Field2 = Field#record_field{validators=[NonNegValidator|Validators]},
    {ok, Field2};
default_validators_rule(#record_field{type=non_empty_binary, validators=Validators}=Field) ->
    NonNegValidator = {tq_transform_utils, non_empty_binary},
    Field2 = Field#record_field{validators=[NonNegValidator|Validators]},
    {ok, Field2};
default_validators_rule(Field) ->
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

type_constructor(binary) -> {ok, none};
type_constructor(non_empty_binary) -> {ok, none};
type_constructor(non_neg_integer) -> {ok, {tq_transform_utils, to_integer}};
type_constructor(non_neg_float) -> {ok, {tq_transform_utils, to_float}};
type_constructor(integer) -> {ok, {tq_transform_utils, to_integer}};
type_constructor(float) -> {ok, {tq_transform_utils, to_float}};
type_constructor(boolean) -> {ok, {tq_transform_utils, to_boolean}};
type_constructor(date) -> {ok, {tq_transform_utils, to_date}};
type_constructor(time) -> {ok, {tq_transform_utils, to_time}};
type_constructor(datetime) -> {ok, {tq_transform_utils, to_datetime}};
type_constructor(_) -> {error, undefined}.

mode_to_acl(r)    -> #access_mode{r=true,  sr=true,  w=false, sw=false};
mode_to_acl(w)    -> #access_mode{r=false, sr=false, w=true,  sw=true};
mode_to_acl(rw)   -> #access_mode{r=true,  sr=true,  w=true,  sw=true};
mode_to_acl(sr)   -> #access_mode{r=false, sr=true,  w=false, sw=false};
mode_to_acl(sw)   -> #access_mode{r=false, sr=false, w=false, sw=true};
mode_to_acl(srsw) -> #access_mode{r=false, sr=true,  w=false, sw=true};
mode_to_acl(rsw)  -> #access_mode{r=true,  sr=true,  w=false, sw=true};
mode_to_acl(srw)  -> #access_mode{r=false, sr=true,  w=true,  sw=true}.

to_list(A) when is_list(A) ->
    A;
to_list(A) ->
    [A].

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
                                         {error, "Storing in record required for default getter and setter"};
                                     {true, _} ->
                                         {error, "Storing in record required for default getter"};
                                     {_, true} ->
                                         {error, "Storing in record required for default setter"};
                                     {G1, S1} ->
                                         {ok, C({false, G1, S1})}
                                 end} || G <- Values, S <- Values],
    Tests = Tests1 ++ Tests2,
    F = fun(From, To) ->
                ?assertEqual(To, get_set_record_rule(From))
        end,
    [fun() -> F(From, To) end || {From, To} <- Tests].

-endif.
