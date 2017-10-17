-module(hiphop_tryout).
-export([server/0, try_it/1]).

server() ->
  {ok, Flamingo} = flamingo:new("The Flamingo Server"),
  flamingo:route(Flamingo, ["/hip", "/hop", "/hi"], hiphop, {0,0}),
  Flamingo.

try_it(Server) ->
  Me = self(),
  Ref = make_ref(),
  flamingo:request(Server, {"/hip", []}, Me, Ref),
  receive
    {Ref, Reply} -> Reply
  end.

% Flamingo = hiphop_tryout:server().
% hiphop_tryout:try_it(Flamingo).
