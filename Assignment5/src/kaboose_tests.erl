-module(kaboose_tests).
-include_lib("eunit/include/eunit.hrl").

% To run:
% $ c(kaboose_tests).
% $ eunit:test(kaboose).

demo_test() -> 
  {ok, Server} = kaboose:start(),
  {ok, Room} = kaboose:get_a_room(Server),
  _ = kaboose:add_question(Room, {"a?", [{correct, "a"}, "b", "c"]}),
  _ = kaboose:add_question(Room, {"b?", ["a", {correct, "b", "c"}]}),
  _ = kaboose:add_question(Room, {"c?", ["a", "b", {correct, "c"}]}),
  List = kaboose:get_questions(Room),
  ?assertEqual([{"a?", [{correct, "a"}, "b", "c"]}, {"b?", ["a", {correct, "b", "c"}]}, {"c?", ["a", "b", {correct, "c"}]}], List).

start_test() -> 
  {ok, _} = kaboose:start().

get_a_room_test() ->
  {ok, Server} = kaboose:start(),
  {ok, _} = kaboose:get_a_room(Server).

add_question_no_correct_answer_test() ->
  {ok, Server} = kaboose:start(),
  {ok, Room} = kaboose:get_a_room(Server),
  ?assertEqual({error, no_correct_answer}, kaboose:add_question(Room, {"a?", ["a", "b", "c"]})).

add_question_wrong_type_answer_test() ->
  {ok, Server} = kaboose:start(),
  {ok, Room} = kaboose:get_a_room(Server),
  ?assertEqual({error, wrong_type_answer}, kaboose:add_question(Room, {"a?"}, [{correct, "a"}, 1])).

join_test() ->
  {ok, Server} = kaboose:start(),
  {ok, Room} = kaboose:get_a_room(Server),
  {ActiveRoom, _} = kaboose:play(Room),
  {ok, _} = kaboose:join(ActiveRoom, "Nickname").

leave_test() ->
  {ok, Server} = kaboose:start(),
  {ok, Room} = kaboose:get_a_room(Server),
  {ActiveRoom, _} = kaboose:play(Room),
  {ok, Ref} = kaboose:join(ActiveRoom, "Nickname"),
  kaboose:leave(ActiveRoom, Ref).

leave_non_existent_player_test() ->
  {ok, Server} = kaboose:start(),
  {ok, Room} = kaboose:get_a_room(Server),
  {ActiveRoom, _} = kaboose:play(Room),
  kaboose:leave(ActiveRoom, self()).

timesup_test() ->
  {ok, Server} = kaboose:start(),
  {ok, Room} = kaboose:get_a_room(Server),
  kaboose:add_question(Room, {"c?", ["a", "b", {correct, "c"}]}),
  kaboose:add_question(Room, {"c?", ["a", "b", {correct, "c"}]}),
  kaboose:add_question(Room, {"c?", ["a", "b", {correct, "c"}]}),
  {ActiveRoom, _} = kaboose:play(Room),
  kaboose:next(ActiveRoom),
  ?assertEqual({ok,[0],#{},#{},false}, kaboose:timesup(ActiveRoom)).