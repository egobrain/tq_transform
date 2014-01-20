-module(db_validators_is_new).

-compile({parse_transform, tq_record_transform}).

-field({req,
        [
         {type, integer},
         required
        ]}).

-field({req2,
        [
         {type, integer},
         {default, 2},
         required
        ]}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

new_test_() ->
    Model = new(),
    [
     {"Required fields",
      fun() ->
              ?assertEqual({error, [{req, required}]}, Model:valid())
      end}
    ].

old_test_() ->
    Model = new(),
    Model2 = Model#?MODULE{'$is_new$' = false},
    [
     {"Now fields required",
      fun() ->
              ok = Model2:valid()
      end},
     {"Changed field required",
      fun() ->
              Model3 = Model2:set_req2(undefined),
              ?assertEqual({error, [{req2, required}]}, Model3:valid())
      end},
     {"Changed field required 2",
      fun() ->
              Model3 = Model2:set_req(1),
              ?assertEqual(ok, Model3:valid()),
              Model4 = Model3:set_req(undefined),
              ?assertEqual({error, [{req, required}]}, Model4:valid())
      end}
    ].

-endif.
