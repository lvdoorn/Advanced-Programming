-module(initialise_fail).
-export([initialise/1, action/3]).

initialise(_Arg) -> throw(initialise_exception).

action({"/test", _Arg}, _Env, State) ->
	{no_change, State}.