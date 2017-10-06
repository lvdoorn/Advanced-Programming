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

% start() -> {ok, spawn(fun loop(new(), new()))}.
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
      io:fwrite("new room called\n"),
      From ! {self(), {ok, spawn(fun() -> roomLoop([]) end)}},
      io:fwrite("function spawned\n"),
      loop()
  end.

roomLoop(Questions) ->
  receive
    % add question
    {From, {add_question, {Description, Answers}}} ->
      From ! {self(), ok},
      roomLoop(Questions ++ [{Description, Answers}]); % TODO: check input and return error if necessary. 

    % get questions
    {From, get_questions} ->
      From ! {self(), Questions},
      roomLoop(Questions)
  end.
    % play -> spawns active room loop

% activeRoomLoop(Questions, Players, CRef) ->
  % next
  % timesup
  % join
  % leave
  % rejoin
  % guess