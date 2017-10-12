-module(greeting_tryout).
-export([greetings/0, try_it/1]).

greetings() ->
    {ok, F} = flamingo:new("The Flamingo Server"),
    flamingo:route(F, ["/hello"], greeter, none),
    F.

try_it(Server) ->
    Me = self(),
    Ref = make_ref(),
    flamingo:request(Server, {"/hello", [{"name", "Student"}]},
                     Me, Ref),
    receive
        {Ref, Reply} -> Reply
    end.
