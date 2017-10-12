-module(greeter).
-export([initialise/1, action/3]).

initialise(_Arg) -> {ok, nothing}.

action({_Path, [{"name", Name} | _ ]}, Server, _) ->
    {no_change,
     lists:concat(["Greetings ", Name, "\n",
                   "You have reached ", Server])}.
