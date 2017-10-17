-module(hiphop).
-export([initialise/1, action/3]).

initialise(Arg) -> {ok, Arg}.

action({"/hip", []}, _Env, {HipCounter, HopCounter}) ->
  NewHipCounter = HipCounter + 1,
  {new_state, integer_to_list(NewHipCounter), {NewHipCounter, HopCounter}};

action({"/hop", []}, _Env, {HipCounter, HopCounter}) ->
  NewHopCounter = HopCounter + 1,
  {new_state, integer_to_list(NewHopCounter), {HipCounter, NewHopCounter}};

action({"/hi", []}, _Env, _State) ->
  {no_change, "Wassup"}.
