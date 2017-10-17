-module(counter).
-export([initialise/1, action/3]).

initialise(Arg) -> {ok, Arg}.

action({"/inc_with", Arg}, _Env, State) ->
	N = getN(Arg),
	NewState = State + N,
	{new_state, NewState, NewState};

action({"/dec_with", Arg}, _Env, State) ->
	N = getN(Arg),
	NewState = State - N,
	{new_state, NewState, NewState}.

% Private functions
getN(Arg) ->
  case Arg of
		[{"x", Number}] ->
    case Number > 0 of
			true -> Number;
			false -> 1
  	end;
	  [] -> 1
  end.
