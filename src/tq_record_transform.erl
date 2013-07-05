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

-include("include/records.hrl").

-export([parse_transform/2]).

-export([create_model/1,
		 model_option/3,
		 normalize_model/1,
		 build_model/1,

		 create_field/1,
		 field_option/3,
		 normalize_field/1,
		 set_field/2,

		 meta_clauses/1
		]).

parse_transform(Ast, Options) ->
	tq_transform:parse_transform(Ast, Options, [?MODULE]).

%% Model.

create_model(Module) ->
	#model{module=Module}.

model_option(init, InitFun, Model) ->
	Model2 = Model#model{init_fun=InitFun},
	{ok, Model2};
model_option(_Option, _Val, _Model) ->
	false.

normalize_model(Model) ->
	{ok, Model}.

build_model(Model) ->
	{Exports, Funs} = tq_record_generator:build_model(Model),
	{lists:reverse(Exports), lists:reverse(Funs)}.

%% Fields.

create_field(Name) ->
	#field{name=Name}.

field_option(required, Value, Field) ->
	Field2 = Field#field{is_required = Value},
	{ok, Field2};
field_option(default, DefaultValue, Field) ->
	Field2 = Field#field{default_value=DefaultValue},
	{ok, Field2};
field_option(init, Init, Field) ->
	Field2 = Field#field{init = Init},
	{ok, Field2};
field_option(mode, Mode, Field) ->
	Field2 = Field#field{mode = mode_to_acl(Mode)},
	{ok, Field2};
field_option(type, Type, Field) ->
	Field2 = Field#field{type = Type},
	{ok, Field2};
field_option(type_constructor, TypeConstructor, Field) ->
	Field2 = Field#field{type_constructor = TypeConstructor},
	{ok, Field2};
field_option(get, Getter, Field) ->
	Field2 = Field#field{getter = Getter},
	{ok, Field2};
field_option(set, Setter, Field) ->
	Field2 = Field#field{setter = Setter},
	{ok, Field2};
field_option(record, StoresInRecord, Field) ->
	Field2 = Field#field{stores_in_record = StoresInRecord},
	{ok, Field2};
field_option(_Option, _Val, _Field) ->
	false.

normalize_field(Field) ->
	Rules = [
			 fun get_set_record_rule/1,
			 fun type_constructor_rule/1
			],
	tq_transform_utils:error_writer_foldl(fun(R, F) -> R(F) end, Field, Rules).

set_field(Field, #model{fields=Fields} = Model) ->
	Model#model{fields=[Field | Fields]}.

%% Meta.

meta_clauses(_Model) -> [].

%% Rules.

get_set_record_rule(Field=#field{stores_in_record=false, getter=Getter, setter=Setter}) ->
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
	{ok, Field#field{stores_in_record=true}}.

type_constructor_rule(#field{type_constructor=undefined, type=Type}=Field) ->
	case type_constructor(Type) of
		{ok, TypeConstructor} ->
			Field2 = Field#field{type_constructor=TypeConstructor},
			{ok, Field2};
		{error, undefined} ->
			Reason = lists:flatten(io_lib:format("type_constructor required for type: ~p", [Type])),
			{error, Reason}
	end;
type_constructor_rule(Field) ->
	{ok, Field}.

%% Internal helpers.

type_constructor(binary) -> {ok, none};
type_constructor(integer) -> {ok, {tq_transform_utils, binary_to_integer}};
type_constructor(float) -> {ok, {tq_transform_utils, binary_to_float}};
type_constructor(_) -> {error, undefined}.

mode_to_acl(r)    -> #access_mode{r=true,  sr=true,  w=false, sw=false};
mode_to_acl(w)    -> #access_mode{r=false, sr=false, w=true,  sw=true};
mode_to_acl(rw)   -> #access_mode{r=true,  sr=true,  w=true,  sw=true};
mode_to_acl(sr)   -> #access_mode{r=false, sr=true,  w=false, sw=false};
mode_to_acl(sw)   -> #access_mode{r=false, sr=false, w=false, sw=true};
mode_to_acl(srsw) -> #access_mode{r=false, sr=true,  w=false, sw=true};
mode_to_acl(rsw)  -> #access_mode{r=true,  sr=true,  w=false, sw=true};
mode_to_acl(srw)  -> #access_mode{r=false, sr=true,  w=true,  sw=true}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_set_record_rule_test_() ->
	C = fun({Stored, Getter, Setter}) -> #field{stores_in_record=Stored, getter=Getter, setter=Setter} end,
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
