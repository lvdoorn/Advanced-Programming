-module(kaboose).
-export([start/0
       , get_a_room/1
       , add_question/2
       , get_questions/1
       , play/1
       , next/1
       , timesup/1
       , join/2
       , leave/2
       , rejoin/2
       , guess/3]).
-import(maps, [new/0, find/2, get/2]).

start() -> {ok, spawn(fun loop/0)}.

get_a_room(Server) -> request_reply(Server, new_room).

add_question(Room, {Description, Answers}) -> request_reply(Room, {add_question, {Description, Answers}}).

get_questions(Room) -> request_reply(Room, get_questions).

play(Room) -> request_reply(Room, play).

next(ActiveRoom) -> request_reply(ActiveRoom, next).

timesup(ActiveRoom) -> request_reply(ActiveRoom, timesup).

join(ActiveRoom, Nick) -> request_reply(ActiveRoom, {join, Nick}).

leave(ActiveRoom, Ref) -> request_reply(ActiveRoom, {leave, Ref}).

rejoin(ActiveRoom, Ref) -> request_reply(ActiveRoom, {rejoin, Ref}).

guess(ActiveRoom, Ref, Index) -> request_reply(ActiveRoom, {guess, Ref, Index}).

% Copied from lecture slides
request_reply(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    {Pid, Response} -> Response
  end.

loop() ->
  receive
    {From, new_room} -> 
      From ! {self(), {ok, spawn(fun() -> roomLoop([]) end)}},
      loop()
  end.

roomLoop(Questions) ->
  receive
    {From, {add_question, {Description, Answers}}} ->
      From ! {self(), ok},
      roomLoop(Questions ++ [{Description, Answers}]); % TODO: check input and return error if necessary. 

    {From, get_questions} ->
      From ! {self(), Questions},
      roomLoop(Questions);

    {From, play} -> 
      From ! {self(), {spawn(fun() -> activeRoomLoop([dummy|Questions], [], From, false) end), From}},
      roomLoop(Questions)
  end.

activeRoomLoop(Questions, Players, CRef, Active) -> 
  receive

  % next
    {From, next} -> 
      From ! {self(), {ok, lists:nth(2, Questions)}},
      activeRoomLoop(lists:nthtail(1, Questions), Players, CRef, true); % TODO errors

  % timesup
    {_From, timesup} ->
      activeRoomLoop(Questions, Players, CRef, false);% TODO return

  % join
    {_From, {join, Nick}} ->
      activeRoomLoop(Questions, [Nick|Players], CRef, Active);% TODO return
    % TODO: send message to conductor
    % TODO: error if Nick is taken
    % TODO: keep list of Refs returned mapped to players



  % leave
    {_From, {leave, _Ref}} ->
      todo;
  
  % rejoin
    {_From, {rejoin, _Ref}} ->
      todo;

  % guess
    {_From, {guess, _Ref, _Index}} ->
      todo

  end.