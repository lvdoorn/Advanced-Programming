-module(counter).

-export([start/0, increment/1, increment/2, get/1]).

%%%% make a server that can keep track of a counter

%%% Internal implementation
request_reply(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    {Pid, Response} -> Response
  end.

%%% API

start() -> spawn(fun () -> loop(0) end).

% Increments a counter by one.
increment(Pid) ->
  request_reply(Pid, inc).

% Increments a counter by Amount.
increment(Pid, Amount) ->
 request_reply(Pid, {inc, Amount}).

% Returns the value of a counter.
get(Pid) ->
  request_reply(Pid, get).

loop(State) ->
  receive
    {From, inc} ->
    NewState = State + 1,
    From ! {self(), NewState},
    loop(NewState);

    {From, {inc, Amount}} ->
    NewState = State + Amount,
    From ! {self(), NewState},
    loop(NewState);

    {From, get} ->
    From ! {self(), State},
    loop(State)
  end.