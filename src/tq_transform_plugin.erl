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

-module(tq_transform_plugin).

-callback create_model(Name, Opts) -> {ok, Model} | {error, Reason} when
      Name :: module(),
      Opts :: [any()],
      Model :: any(),
      Reason :: any().
-callback model_option(Option, Value, Model) -> {ok, Model} | {error, Reason} | false when
      Option :: atom(),
      Value :: any(),
      Reason :: any().

-callback create_field(Name :: atom()) -> {ok, Field :: any()} | false.
-callback field_option(Option, Value, Field) -> {ok, Field} | {error, Reason} | false when
      Option :: atom(),
      Value :: any(),
      Reason :: any().
-callback normalize_field(Field, Model :: any()) -> {ok, Field} | {error, Reason :: any()}.
-callback set_field(Field :: any(), Model) -> {ok, Model}.

-callback normalize_model(Model) -> {ok, Model} | {error, Reasons :: any()}.
-callback build_model(Model :: any()) -> {Exports :: [erl_syntax:forms()], Funs :: [erl_syntax:forms()]}.

-callback meta_clauses(Model :: any()) -> [Clauses :: erl_syntax:forms()].

-callback set_globals(Globals :: tq_transform:globals() , Model) -> Model.
