-module(flamingo_tests).
-include_lib("eunit/include/eunit.hrl").

% To run:
% $ c(flamingo_tests).
% $ eunit:test(flamingo).

new_server_test() ->
  ?assertEqual({ok, Flamingo}, flamingo:new(env)).

route_test() ->
  {ok, Flamingo} = flamingo:new(env),
  ?assertEqual({ok, Id}, flamingo:route(Flamingo, ["/test"], greeter, none)).

succeeding_request_exact_match_test() ->
  {ok, Flamingo} = flamingo:new(env),
  {ok, _Id} = flamingo:route(Flamingo, ["/test"], greeter, none),
  Ref = make_ref(),
  Me = self(),
  Name = "me",
  flamingo:request(Flamingo, {"/test", [{"name", Name}]}, Me, Ref),
  receive
    Msg -> ?assertEqual(Msg, {Ref, {200, lists:concat(["Greetings ", Name, "\n",
                   "You have reached ", Flamingo])}})
  end.

succeeding_request_not_exact_match_test() ->
  {ok, Flamingo} = flamingo:new(env),
  {ok, _Id} = flamingo:route(Flamingo, ["/test"], greeter, none),
  Ref = make_ref(),
  Me = self(),
  Name = "me",
  flamingo:request(Flamingo, {"/te", [{"name", Name}]}, Me, Ref),
  receive
    Msg -> ?assertEqual(Msg, {Ref, {200, lists:concat(["Greetings ", Name, "\n",
                   "You have reached ", Flamingo])}})
  end.

failing_request_test() ->
  {ok, Flamingo} = flamingo:new(env),
  Ref = make_ref(),
  Me = self(),
  Name = "me",
  flamingo:request(Flamingo, {"/test", [{"name", Name}]}, Me, Ref),
  receive
    Msg -> ?assertEqual(Msg, {Ref, {404, _}})
  end.

error_request_test() ->
  {ok, Flamingo} = flamingo:new(env),
  {ok, _Id} = flamingo:route(Flamingo, ["/fail"], failer, none),
  Ref = make_ref(),
  Me = self(),
  flamingo:request(Flamingo, {"/fail", []}, Me, Ref),  
  receive
    Msg -> ?assertEqual(Msg, {Ref, {500, _}})
  end.

request_multiple_parameters_test() ->
  {ok, Flamingo} = flamingo:new(env),
  {ok, _Id} = flamingo:route(Flamingo, ["/add"], add, none),
  Ref = make_ref(),
  Me = self(),
  flamingo:request(Flamingo, {"/add", [{"x", "5"}, {"y", "37"}]}, Me, Ref),
  receive
    Msg -> ?assertEqual(Msg, {Ref, {200, "42"}})
  end.

longest_prefix_test_exact_match() ->
  {ok, Flamingo} = flamingo:new(env),
  {ok, _Id1} = flamingo:route(Flamingo, ["/tes"], failer, none),
  {ok, _Id2} = flamingo:route(Flamingo, ["/test"], greeter, none),
  Ref = make_ref(),
  Me = self(),
  Name = "me",
  flamingo:request(Flamingo, {"/test", [{"name", Name}]}, Me, Ref),
  receive
    Msg -> ?assertEqual(Msg, {Ref, {200, lists:concat(["Greetings ", Name, "\n",
                   "You have reached ", Flamingo])}})
  end.

longest_prefix_test_not_exact_match() ->
  {ok, Flamingo} = flamingo:new(env),
  {ok, _Id1} = flamingo:route(Flamingo, ["/te"], failer, none),
  {ok, _Id2} = flamingo:route(Flamingo, ["/tes"], greeter, none),
  Ref = make_ref(),
  Me = self(),
  Name = "me",
  flamingo:request(Flamingo, {"/test", [{"name", Name}]}, Me, Ref),
  receive
    Msg -> ?assertEqual(Msg, {Ref, {200, lists:concat(["Greetings ", Name, "\n",
                   "You have reached ", Flamingo])}})
  end.

same_prefix_in_different_modules_test() ->
  {ok, Flamingo} = flamingo:new(env),
  {ok, _Id1} = flamingo:route(Flamingo, ["/test"], failer, none),
  {ok, _Id2} = flamingo:route(Flamingo, ["/test"], greeter, none),
  Ref = make_ref(),
  Me = self(),
  Name = "me",
  flamingo:request(Flamingo, {"/test", [{"name", Name}]}, Me, Ref),
  receive
    Msg -> ?assertEqual(Msg, {Ref, {200, lists:concat(["Greetings ", Name, "\n",
                   "You have reached ", Flamingo])}})
  end.

