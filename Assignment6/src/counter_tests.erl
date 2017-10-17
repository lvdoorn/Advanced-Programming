-module(counter_tests).
-include_lib("eunit/include/eunit.hrl").

% To run:
% $ c(counter_tests).
% $ eunit:test(counter).

inc_test() ->
  {ok, Flamingo} = flamingo:new("The Flamingo Server"),
  flamingo:route(Flamingo, ["/inc_with", "/dec_with"], counter, 0),
  Me = self(),
  Ref = make_ref(),
  flamingo:request(Flamingo, {"/inc_with", [{"x", 7}]}, Me, Ref),
  receive
    {Ref, Reply} -> ?assertEqual(Reply, {200, 7}) 
  end.

dec_test() ->
  {ok, Flamingo} = flamingo:new("The Flamingo Server"),
  flamingo:route(Flamingo, ["/inc_with", "/dec_with"], counter, 0),
  Me = self(),
  Ref = make_ref(),
  flamingo:request(Flamingo, {"/dec_with", [{"x", 7}]}, Me, Ref),
  receive
    {Ref, Reply} -> ?assertEqual(Reply, {200, -7}) 
  end.


inc_no_arg_test() ->
  {ok, Flamingo} = flamingo:new("The Flamingo Server"),
  flamingo:route(Flamingo, ["/inc_with", "/dec_with"], counter, 0),
  Me = self(),
  Ref = make_ref(),
  flamingo:request(Flamingo, {"/inc_with", []}, Me, Ref),
  receive
    {Ref, Reply} -> ?assertEqual(Reply, {200, 1}) 
  end.

inc_negative_arg_test() ->
  {ok, Flamingo} = flamingo:new("The Flamingo Server"),
  flamingo:route(Flamingo, ["/inc_with", "/dec_with"], counter, 0),
  Me = self(),
  Ref = make_ref(),
  flamingo:request(Flamingo, {"/inc_with", [{"x", -5}]}, Me, Ref),
  receive
    {Ref, Reply} -> ?assertEqual(Reply, {200, 1}) 
  end.

dec_no_arg_test() ->
  {ok, Flamingo} = flamingo:new("The Flamingo Server"),
  flamingo:route(Flamingo, ["/inc_with", "/dec_with"], counter, 0),
  Me = self(),
  Ref = make_ref(),
  flamingo:request(Flamingo, {"/dec_with", []}, Me, Ref),
  receive
    {Ref, Reply} -> ?assertEqual(Reply, {200, -1}) 
  end.

dec_negative_arg_test() ->
  {ok, Flamingo} = flamingo:new("The Flamingo Server"),
  flamingo:route(Flamingo, ["/inc_with", "/dec_with"], counter, 0),
  Me = self(),
  Ref = make_ref(),
  flamingo:request(Flamingo, {"/dec_with", [{"x", -5}]}, Me, Ref),
  receive
    {Ref, Reply} -> ?assertEqual(Reply, {200, -1}) 
  end.

inc_no_matching_route_test() ->
  {ok, Flamingo} = flamingo:new("The Flamingo Server"),
  flamingo:route(Flamingo, ["/inc_with", "/dec_with"], counter, 0),
  Me = self(),
  Ref = make_ref(),
  flamingo:request(Flamingo, {"/unknown", []}, Me, Ref),
  receive
    Msg -> ?assertEqual(Msg, {Ref, {404, no_matching_routes}}) 
  end.

invalid_args_test() ->
  {ok, Flamingo} = flamingo:new("The Flamingo Server"),
  flamingo:route(Flamingo, ["/inc_with", "/dec_with"], counter, 0),
  Me = self(),
  Ref = make_ref(),
  flamingo:request(Flamingo, {"/dec_with", [{"y", -5}]}, Me, Ref),
  receive
    {Ref, Reply} -> ?assertEqual(Reply, {500, error}) 
  end.

same_action_module_different_groups_test() ->
  {ok, Flamingo} = flamingo:new("The Flamingo Server"),
  flamingo:route(Flamingo, ["/inc_with"], counter, 0),
  flamingo:route(Flamingo, ["/dec_with"], counter, 0),
  Me = self(),
  Ref = make_ref(),
  flamingo:request(Flamingo, {"/inc_with", [{"x", 5}]}, Me, Ref),
  receive
    Msg -> ?assertEqual(Msg, {Ref, {200, 5}}) 
  end,
  flamingo:request(Flamingo, {"/dec_with", [{"x", 33}]}, Me, Ref),
  receive
    Msg1 -> ?assertEqual(Msg1, {Ref, {200, -33}}) 
  end,
  flamingo:request(Flamingo, {"/inc_with", [{"x", 5}]}, Me, Ref),
  receive
    Msg2 -> ?assertEqual(Msg2, {Ref, {200, 10}}) 
  end,
  flamingo:request(Flamingo, {"/dec_with", [{"x", 2}]}, Me, Ref),
  receive
    Msg3 -> ?assertEqual(Msg3, {Ref, {200, -35}}) 
  end.

end_to_end_test() ->
  {ok, Flamingo} = flamingo:new("The Flamingo Server"),
  flamingo:route(Flamingo, ["/inc_with", "/dec_with"], counter, 0),
  Me = self(),
  Ref = make_ref(),
  flamingo:request(Flamingo, {"/inc_with", []}, Me, Ref),
  receive
    {Ref, Reply1} -> ?assertEqual(Reply1, {200, 1}) 
  end,
  flamingo:request(Flamingo, {"/inc_with", []}, Me, Ref),
  receive
    {Ref, Reply2} -> ?assertEqual(Reply2, {200, 2}) 
  end,
  flamingo:request(Flamingo, {"/inc_with", []}, Me, Ref),
  receive
    {Ref, Reply3} -> ?assertEqual(Reply3, {200, 3}) 
  end,
  flamingo:request(Flamingo, {"/dec_with", [{"x", 3}]}, Me, Ref),
  receive
    {Ref, Reply4} -> ?assertEqual(Reply4, {200, 0}) 
  end.

end_to_end_2_test() ->
  {ok, Flamingo} = flamingo:new("The Flamingo Server"),
  flamingo:route(Flamingo, ["/inc_with", "/dec_with"], counter, 0),
  Me = self(),
  Ref = make_ref(),
  flamingo:request(Flamingo, {"/inc_with", [{"x", -2}]}, Me, Ref),
  receive
    {Ref, Reply1} -> ?assertEqual(Reply1, {200, 1}) 
  end,
  flamingo:request(Flamingo, {"/inc_with", [{"x", 9}]}, Me, Ref),
  receive
    {Ref, Reply2} -> ?assertEqual(Reply2, {200, 10}) 
  end,
  flamingo:request(Flamingo, {"/dec_with", [{"x", 5}]}, Me, Ref),
  receive
    {Ref, Reply3} -> ?assertEqual(Reply3, {200, 5}) 
  end,
  flamingo:request(Flamingo, {"/inc_with", []}, Me, Ref),
  receive
    {Ref, Reply4} -> ?assertEqual(Reply4, {200, 6}) 
  end.
