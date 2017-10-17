-module(failer).
-export([initialise/1, action/3]).

initialise(_Arg) -> {ok, nothing}.

action(_, _, _) ->
  exit(self(), ok).
