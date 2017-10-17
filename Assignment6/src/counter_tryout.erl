-module(counter_tryout).
-export([server/0, try_it/1]).

server() ->
  {ok, Flamingo} = flamingo:new("The Flamingo Server"),
  flamingo:route(Flamingo, ["/inc_with", "/dec_with"], counter, 0),
  Flamingo.

try_it(Server) ->
  Me = self(),
  Ref = make_ref(),
  flamingo:request(Server, {"/inc_with", [{"x", 7}]},
                   Me, Ref),
  receive
      {Ref, Reply} -> Reply
  end.

% Flamingo = counter_tryout:server().
% counter_tryout:try_it(Flamingo).
