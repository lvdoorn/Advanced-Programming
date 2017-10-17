-module(exception).
-export([initialise/1, action/3]).

initialise(_Arg) -> {ok, nothing}.

action(_, _, _) ->
  throw(message).
