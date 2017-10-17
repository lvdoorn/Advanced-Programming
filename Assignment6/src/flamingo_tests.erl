-module(flamingo_tests).
-include_lib("eunit/include/eunit.hrl").

% To run:
% $ c(flamingo_tests).
% $ eunit:test(flamingo).

new_server_test() ->
  {ok, _Flamingo} = flamingo:new(env).

route_test() ->
  {ok, Flamingo} = flamingo:new(env),
  {ok, _Id} = flamingo:route(Flamingo, ["/test"], greeter, none).

succeeding_request_exact_match_test() ->
  Env = "test",
  {ok, Flamingo} = flamingo:new(Env),
  {ok, _Id} = flamingo:route(Flamingo, ["/test"], greeter, none),
  Ref = make_ref(),
  Me = self(),
  Name = "me",
  flamingo:request(Flamingo, {"/test", [{"name", Name}]}, Me, Ref),
  receive
    Msg -> ?assertEqual({Ref, {200, lists:concat(["Greetings ", Name, "\n",
                   "You have reached ", Env])}}, Msg)
  end.

succeeding_request_not_exact_match_test() ->
  Env = "test",
  {ok, Flamingo} = flamingo:new(Env),
  {ok, _Id} = flamingo:route(Flamingo, ["/te"], greeter, none),
  Ref = make_ref(),
  Me = self(),
  Name = "me",
  flamingo:request(Flamingo, {"/test", [{"name", Name}]}, Me, Ref),
  receive
    Msg -> ?assertEqual({Ref, {200, lists:concat(["Greetings ", Name, "\n",
                   "You have reached ", Env])}}, Msg)
  end.

failing_request_test() ->
  {ok, Flamingo} = flamingo:new(env),
  Ref = make_ref(),
  Me = self(),
  Name = "me",
  flamingo:request(Flamingo, {"/test", [{"name", Name}]}, Me, Ref),
  receive
    Msg -> ?assertEqual(Msg, {Ref, {404, no_matching_routes}})
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

longest_prefix_exact_match_test() ->
  Env = "test",
  {ok, Flamingo} = flamingo:new(Env),
  {ok, _Id1} = flamingo:route(Flamingo, ["/tes"], failer, none),
  {ok, _Id2} = flamingo:route(Flamingo, ["/test"], greeter, none),
  Ref = make_ref(),
  Me = self(),
  Name = "me",
  flamingo:request(Flamingo, {"/test", [{"name", Name}]}, Me, Ref),
  receive
    Msg -> ?assertEqual(Msg, {Ref, {200, lists:concat(["Greetings ", Name, "\n",
                   "You have reached ", Env])}})
  end.

longest_prefix_not_exact_match_test() ->
  Env = "test",
  {ok, Flamingo} = flamingo:new(Env),
  {ok, _Id1} = flamingo:route(Flamingo, ["/te"], failer, none),
  {ok, _Id2} = flamingo:route(Flamingo, ["/tes"], greeter, none),
  Ref = make_ref(),
  Me = self(),
  Name = "me",
  flamingo:request(Flamingo, {"/test", [{"name", Name}]}, Me, Ref),
  receive
    Msg -> ?assertEqual(Msg, {Ref, {200, lists:concat(["Greetings ", Name, "\n",
                   "You have reached ", Env])}})
  end.

same_prefix_in_different_modules_test() ->
  Env = "test",
  {ok, Flamingo} = flamingo:new(Env),
  {ok, _Id1} = flamingo:route(Flamingo, ["/test"], failer, none),
  {ok, _Id2} = flamingo:route(Flamingo, ["/test"], greeter, none),
  Ref = make_ref(),
  Me = self(),
  Name = "me",
  flamingo:request(Flamingo, {"/test", [{"name", Name}]}, Me, Ref),
  receive
    Msg -> ?assertEqual(Msg, {Ref, {200, lists:concat(["Greetings ", Name, "\n",
                   "You have reached ", Env])}})
  end.

same_prefix_in_route_group_test() ->
  {ok, Flamingo} = flamingo:new(env),
  ?assertEqual({error, invalid_prefixes}, flamingo:route(Flamingo, ["/test", "/test"], greeter, none)).

no_prefix_in_route_test() ->
  {ok, Flamingo} = flamingo:new(env),
  ?assertEqual({error, invalid_prefixes}, flamingo:route(Flamingo, [], greeter, none)).

empty_prefix_in_route__test() ->
  {ok, Flamingo} = flamingo:new(env),
  ?assertEqual({error, invalid_prefixes}, flamingo:route(Flamingo, [""], greeter, none)).

% error_request_test() ->
%   {ok, Flamingo} = flamingo:new(env),
%   {ok, _Id} = flamingo:route(Flamingo, ["/fail"], failer, none),
%   Ref = make_ref(),
%   Me = self(),
%   flamingo:request(Flamingo, {"/fail", []}, Me, Ref),  
%   receive
%     Msg -> ?assertEqual(Msg, {Ref, {500, fail}})
%   end.
