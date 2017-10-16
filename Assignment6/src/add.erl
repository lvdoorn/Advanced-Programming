-module(add).

-export([initialise/1, action/3]).

initialise(_Arg) -> {ok, nothing}.

action({_Path, [{"x", X}, {"y", Y}]}, _, _) ->
  {no_change, integer_to_list(list_to_integer(X) + list_to_integer(Y))}.