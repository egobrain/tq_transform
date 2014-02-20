-module(db_types).

-compile({parse_transform, tq_record_transform}).

-define(FIELD(NAME, TYPE), -field({NAME, [{type, TYPE}]})).
?FIELD(binary_field, binary).
?FIELD(non_empty_binary_field, non_empty_binary).
?FIELD(non_neg_integer_field, non_neg_integer).
?FIELD(non_neg_float_field, non_neg_float).
?FIELD(integer_field, integer).
?FIELD(float_field, float).
?FIELD(boolean_field, boolean).
?FIELD(date_field, date).
?FIELD(time_field, time).
?FIELD(datetime_field, datetime).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

converter_test_() ->
    Tests =
        [
         {binary_field, <<"binary">>, {ok, <<"binary">>}},
         {binary_field, <<"">>, {ok, <<"">>}},

         {non_empty_binary_field, <<"binary">>, {ok, <<"binary">>}},
         {non_empty_binary_field, <<"">>, {error, [{non_empty_binary_field, empty}]}},

         {integer_field, <<"123">>, {ok, 123}},
         {integer_field, <<"-123">>, {ok, -123}},
         {integer_field, <<"123a">>, {error, [{integer_field, wrong_format}]}},

         {non_neg_integer_field, <<"123">>, {ok, 123}},
         {non_neg_integer_field, <<"-123">>, {error, [{non_neg_integer_field, {less_than, 0}}]}},
         {non_neg_integer_field, <<"123a">>, {error, [{non_neg_integer_field, wrong_format}]}},

         {float_field, <<"12">>, {ok, 12.0}},
         {float_field, <<"12.3">>, {ok, 12.3}},
         {float_field, <<"-12">>, {ok, -12.0}},
         {float_field, <<"-12.3">>, {ok, -12.3}},
         {float_field, <<"12.3a">>, {error, [{float_field, wrong_format}]}},

         {non_neg_float_field, <<"12">>, {ok, 12.0}},
         {non_neg_float_field, <<"12.3">>, {ok, 12.3}},
         {non_neg_float_field, <<"-12">>, {error, [{non_neg_float_field, {less_than, 0}}]}},
         {non_neg_float_field, <<"-12.3">>, {error, [{non_neg_float_field, {less_than, 0}}]}},
         {non_neg_float_field, <<"12.3a">>, {error, [{non_neg_float_field, wrong_format}]}},

         {date_field, <<"2014-03-15">>, {ok, {2014, 03, 15}}},
         {date_field, <<"2014-A3-15">>, {error, [{date_field, wrong_format}]}},
         {date_field, <<"2014-13-32">>, {error, [{date_field, invalid_date}]}},

         {time_field, <<"12:01:02">>, {ok, {12, 01, 02}}},
         {time_field, <<"24:01:02">>, {error, [{time_field, invalid_time}]}},
         {time_field, <<"25:01:02">>, {error, [{time_field, invalid_time}]}},
         {time_field, <<"12:60:02">>, {error, [{time_field, invalid_time}]}},
         {time_field, <<"12:01:60">>, {error, [{time_field, invalid_time}]}},

         {datetime_field, <<"2014-03-15T00:00:00z">>, {ok, {{2014, 03, 15}, {0, 0, 0}}}},
         {datetime_field, <<"2014-03-15T18:50:00-04:00">>, {ok, {{2014, 03, 15}, {22, 50, 0}}}},
         {datetime_field, <<"2014-12-31T22:50:00-04:00">>, {ok, {{2015, 01, 01}, {2, 50, 0}}}},
         {datetime_field, <<"2014-03-15T22:50:00+04:00">>, {ok, {{2014, 03, 15}, {18, 50, 0}}}},
         {datetime_field, <<"2015-01-01T2:50:00+04:00">>, {ok, {{2014, 12, 31}, {22, 50, 0}}}},
         {datetime_field, <<"2999-12-31t00:00:00.001z">>, {ok, {{2999, 12, 31}, {0, 0, 0}}}},
         {datetime_field, <<"2999-12-31T01:02:03z">>, {ok, {{2999, 12, 31}, {1, 2, 3}}}},
         {datetime_field, <<"2014-13-15T00:00:00Z">>, {error, [{datetime_field, invalid_date}]}},
         {datetime_field, <<"2014-12-15T24:00:00Z">>, {error, [{datetime_field, invalid_time}]}},
         {datetime_field, <<"01-13-01 0:0:0">>, {error, [{datetime_field, wrong_format}]}},
         {datetime_field, <<"2014/12/01 0:0:0">>, {error, [{datetime_field, wrong_format}]}},
         {datetime_field, <<"2014-12-1  0:0:0">>, {error, [{datetime_field, wrong_format}]}}
        ],
    [fun() ->
             Result =
                 from_ext_proplist(
                   [
                    {list_to_binary(atom_to_list(F)), V}
                   ]),
             case Result of
                 {ok, M} ->
                     case M:valid() of
                         ok ->
                             ?assertEqual({ok, M:F()}, R);
                         {error, _Reason} = Err ->
                             ?assertEqual(Err, R)
                     end;
                 {error, _Reason} = Err ->
                     ?assertEqual(Err, R)
             end
     end || {F, V, R} <- Tests].

-endif.
