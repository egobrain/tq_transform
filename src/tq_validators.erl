-module(tq_validators).

-export([
         gt/2,
         lt/2,
         ge/2,
         le/2,
         non_empty/1
        ]).

gt(A, B) when B > A -> ok;
gt(A, _) -> {error, {less_than, A}}.

lt(A, B) when B < A -> ok;
lt(A, _) -> {error, {more_than, A}}.

ge(A, B) when B >= A -> ok;
ge(A, _) -> {error, {less_than, A}}.

le(A, B) when B =< A -> ok;
le(A, _) -> {error, {more_than, A}}.

non_empty(<<>>) -> {error, empty};
non_empty(_) -> ok.
