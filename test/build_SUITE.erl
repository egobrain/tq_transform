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

-module(build_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%% Tests.

-export([simple_build/1]).

%% ct.

all() ->
	[
	 {group, build}
	].

groups() ->
	Tests = [
			 simple_build
			],
	[
	 {build, [parallel], Tests}
	].

init_per_suite(Config) ->
	Models = [db_simple],
	[{models, Models} | Config].

end_per_suite(Config) ->
	ok.


simple_build(Config) ->
	Models = ?config(models, Config),
	DataDir = ?config(data_dir, Config),
	lists:foreach(fun(Module) ->
						  Filename = filename:join(DataDir, atom_to_list(Module)++".erl"),
						  {ok, Module} = compile:file(Filename, [{parse_transform, tq_db_transform}])
				  end, Models).
