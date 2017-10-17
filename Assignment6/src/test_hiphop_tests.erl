-module(hiphop_tests).
-include_lib("eunit/include/eunit.hrl").

% To run:
% $ c(hiphop_tests).
% $ eunit:test(hiphop).

hip_request_test() ->
  {ok, Flamingo} = flamingo:new("The Flamingo Server"),
  flamingo:route(Flamingo, ["/hip", "/hop", "/hi"], hiphop, {0,0}),
  Me = self(),
  Ref = make_ref(),
  flamingo:request(Flamingo, {"/hip", []}, Me, Ref),
  receive
    {Ref, Reply} -> ?assertEqual(Reply, {200, "1"}) 
  end,
  flamingo:request(Flamingo, {"/hip", []}, Me, Ref),
  receive
    {Ref, Reply1} -> ?assertEqual(Reply1, {200, "2"}) 
  end.

hop_request_test() ->
  {ok, Flamingo} = flamingo:new("The Flamingo Server"),
  flamingo:route(Flamingo, ["/hip", "/hop", "/hi"], hiphop, {0,0}),
  Me = self(),
  Ref = make_ref(),
  flamingo:request(Flamingo, {"/hop", []}, Me, Ref),
  receive
    {Ref, Reply} -> ?assertEqual(Reply, {200, "1"}) 
  end,
  flamingo:request(Flamingo, {"/hop", []}, Me, Ref),
  receive
    {Ref, Reply1} -> ?assertEqual(Reply1, {200, "2"}) 
  end.

hi_request_test() ->
  {ok, Flamingo} = flamingo:new("The Flamingo Server"),
  flamingo:route(Flamingo, ["/hip", "/hop", "/hi"], hiphop, {0,0}),
  Me = self(),
  Ref = make_ref(),
  flamingo:request(Flamingo, {"/hi", []}, Me, Ref),
  receive
    {Ref, Reply} -> ?assertEqual(Reply, {200, "Wassup"}) 
  end.

end_to_end_test() ->
  {ok, Flamingo} = flamingo:new("The Flamingo Server"),
  flamingo:route(Flamingo, ["/hip", "/hop", "/hi"], hiphop, {0,0}),
  Me = self(),
  Ref = make_ref(),
  flamingo:request(Flamingo, {"/hop", []}, Me, Ref),
  receive
    {Ref, Reply} -> ?assertEqual(Reply, {200, "1"}) 
  end,
  flamingo:request(Flamingo, {"/hip", []}, Me, Ref),
  receive
    {Ref, Reply1} -> ?assertEqual(Reply1, {200, "1"}) 
  end,
  flamingo:request(Flamingo, {"/hi", []}, Me, Ref),
  receive
    {Ref, Reply2} -> ?assertEqual(Reply2, {200, "Wassup"}) 
  end,
  flamingo:request(Flamingo, {"/hop", []}, Me, Ref),
  receive
    {Ref, Reply3} -> ?assertEqual(Reply3, {200, "2"}) 
  end,
  flamingo:request(Flamingo, {"/hip", []}, Me, Ref),
  receive
    {Ref, Reply4} -> ?assertEqual(Reply4, {200, "2"}) 
  end.
