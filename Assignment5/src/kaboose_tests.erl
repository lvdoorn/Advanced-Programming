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
  kaboose:add_question(Room, {"z?", ["x", {correct, "z"}]}),
  {ActiveRoom, _} = kaboose:play(Room),
  kaboose:next(ActiveRoom),
  ?assertEqual({error, has_active_question}, kaboose:next(ActiveRoom)).

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