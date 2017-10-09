-module(kaboose_tests).
-include_lib("eunit/include/eunit.hrl").

% To run:
% $ c(kaboose_tests).
% $ eunit:test(kaboose).

demo_test() -> 
  {ok, Server} = kaboose:start(),
  {ok, Room} = kaboose:get_a_room(Server),
  _ = kaboose:add_question(Room, {"a?", [{correct, "a"}, "b", "c"]}),
  _ = kaboose:add_question(Room, {"b?", ["a", {correct, "b"}, "c"]}),
  _ = kaboose:add_question(Room, {"c?", ["a", "b", {correct, "c"}]}),
  List = kaboose:get_questions(Room),
  ?assertEqual([{"a?", [{correct, "a"}, "b", "c"]}, {"b?", ["a", {correct, "b"}, "c"]}, {"c?", ["a", "b", {correct, "c"}]}], List).

start_test() -> 
  {ok, _} = kaboose:start().

get_a_room_test() ->
  {ok, Server} = kaboose:start(),
  {ok, _} = kaboose:get_a_room(Server).

validate_non_empty_string_test() ->
  ok = kaboose:validateNonEmptyString("valid string").

invalid_non_empty_string_test() ->
  ?assertEqual(not_a_non_empty_string, kaboose:validateNonEmptyString(invalidString)).

invalid_empty_string_test() ->
  ?assertEqual(not_a_non_empty_string, kaboose:validateNonEmptyString("")).

invalid_answer_test() ->
  ?assertEqual(answer_format_is_invalid, kaboose:validateAnswer(["a", 1, {correct, "c"}])).

invalid_answer_empty_string_test() ->
  ?assertEqual(answer_format_is_invalid, kaboose:validateAnswer(["a", "", {correct, "c"}])).

invalid_answer_no_correct_test() ->
  ?assertEqual(no_correct_options, kaboose:validateAnswer(["a", "b", "c"])).

invalid_answer_type_test() ->
  ?assertEqual(answer_format_is_invalid, kaboose:validateAnswer(["a", "d", {correct, "c", "b"}])).

invalid_answer_correct_type_test() ->
  ?assertEqual(answer_format_is_invalid, kaboose:validateAnswer(["a", "s", {incorrect, "c"}])).


add_question_no_correct_answer_test() ->
  {ok, Server} = kaboose:start(),
  {ok, Room} = kaboose:get_a_room(Server),
  ?assertEqual({error, no_correct_options}, kaboose:add_question(Room, {"a?", ["a", "b", "c"]})).

add_question_wrong_type_answer_test() ->
  {ok, Server} = kaboose:start(),
  {ok, Room} = kaboose:get_a_room(Server),
  ?assertEqual({error, answer_format_is_invalid}, kaboose:add_question(Room, {"a?", [{correct, "a"}, 1]})).

add_question_description_empty_test() ->
  {ok, Server} = kaboose:start(),
  {ok, Room} = kaboose:get_a_room(Server),
  ?assertEqual({error, not_a_non_empty_string}, kaboose:add_question(Room, {"", [{correct, "a"}, 1]})).

next_test() ->
  {ok, Server} = kaboose:start(),
  {ok, Room} = kaboose:get_a_room(Server),
  kaboose:add_question(Room, {"a?", [{correct, "a"}, "b", "c"]}),
  kaboose:add_question(Room, {"b?", ["a", {correct, "b"}, "c"]}),
  kaboose:add_question(Room, {"c?", ["a", "b", {correct, "c"}]}),
  {ActiveRoom, _} = kaboose:play(Room),
  ?assertEqual({ok, {"a?", [{correct, "a"}, "b", "c"]}}, kaboose:next(ActiveRoom)),
  kaboose:timesup(ActiveRoom),
  ?assertEqual({ok, {"b?", ["a", {correct, "b"}, "c"]}}, kaboose:next(ActiveRoom)),
  kaboose:timesup(ActiveRoom),
  ?assertEqual({ok, {"c?", ["a", "b", {correct, "c"}]}}, kaboose:next(ActiveRoom)).


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

% leave_non_existent_player_test() ->
%   {ok, Server} = kaboose:start(),
%   {ok, Room} = kaboose:get_a_room(Server),
%   {ActiveRoom, _} = kaboose:play(Room),
%   kaboose:leave(ActiveRoom, self()).

timesup_test() ->
  {ok, Server} = kaboose:start(),
  {ok, Room} = kaboose:get_a_room(Server),
  kaboose:add_question(Room, {"c?", ["a", "b", {correct, "c"}]}),
  kaboose:add_question(Room, {"c?", ["a", "b", {correct, "c"}]}),
  kaboose:add_question(Room, {"c?", ["a", "b", {correct, "c"}]}),
  {ActiveRoom, _} = kaboose:play(Room),
  kaboose:next(ActiveRoom),
  ?assertEqual({ok,[0, 0, 0],#{},#{},false}, kaboose:timesup(ActiveRoom)).

scenario1_test() ->
  {ok, Server} = kaboose:start(),
  {ok, Room} = kaboose:get_a_room(Server),
  kaboose:add_question(Room, {"q1?", ["a", "b", {correct, "c"}]}),
  kaboose:add_question(Room, {"q2?", ["a", "b", {correct, "c"}]}),
  kaboose:add_question(Room, {"q3?", ["a", "b", {correct, "c"}]}),
  {ActiveRoom, _} = kaboose:play(Room),
  {ok, Ref} = kaboose:join(ActiveRoom, "Nickname"),
  kaboose:next(ActiveRoom), % set q1
  kaboose:guess(ActiveRoom, Ref, 1),
  kaboose:timesup(ActiveRoom),
  kaboose:next(ActiveRoom), % set q2
  kaboose:guess(ActiveRoom, Ref, 2),
  kaboose:timesup(ActiveRoom),
  kaboose:next(ActiveRoom), % set q3
  kaboose:guess(ActiveRoom, Ref, 3),
  kaboose:timesup(ActiveRoom),
  kaboose:leave(ActiveRoom, Ref),
  kaboose:rejoin(ActiveRoom, Ref).