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

start() -> ok.

get_a_room(Server) -> ok.

add_question(Room, {Description, Answers}) -> ok.

get_questions(Room) -> ok.

play(Room) -> ok.

next(ActiveRoom) -> ok.

timesup(ActiveRoom) -> ok.

join(ActiveRoom, Nick) -> ok.

leave(ActiveRoom, Ref) -> ok.

rejoin(ActiveRoom, Ref) -> ok.

guess(ActiveRoom, Ref, Index) -> ok.