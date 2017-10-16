-module(greeter).
-export([initialise/1, action/3]).
-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.
initialise(_Arg) -> {ok, nothing}.

action({_Path, [{"name", Name} | _ ]}, Server, _) ->
  ?PRINT(Server),
  {no_change,
    lists:concat(["Greetings ", Name, "\n",
                  "You have reached ", Server])}.
