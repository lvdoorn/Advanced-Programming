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
  ?_assertException(error, _, kaboose:validateNonEmptyString(invalidString)).

invalid_empty_string_test() ->
  ?_assertException(error, _, kaboose:validateNonEmptyString("")).

invalid_answer_test() ->
  ?_assertException(error, _, kaboose:validateAnswer(["a", 1, {correct, "c"}])).

invalid_answer_empty_string_test() ->
  ?_assertException(error, _, kaboose:validateAnswer(["a", "", {correct, "c"}])).

invalid_answer_no_correct_test() ->
  ?_assertException(error, _, kaboose:validateAnswer(["a", "b", "c"])).

invalid_answer_type_test() ->
  ?_assertException(error, _, kaboose:validateAnswer(["a", "d", {correct, "c", "b"}])).

invalid_answer_correct_type_test() ->
  ?_assertException(error, _, kaboose:validateAnswer(["a", "s", {incorrect, "c"}])).


get_a_room_exception_test() ->
  {error,badarg} = kaboose:get_a_room(1).

add_question_no_correct_answer_test() ->
  {ok, Server} = kaboose:start(),
  {ok, Room} = kaboose:get_a_room(Server),
  ?assertEqual({error, "Answer does not have any correct options"}, kaboose:add_question(Room, {"a?", ["a", "b", "c"]})).

add_question_wrong_type_answer_test() ->
  {ok, Server} = kaboose:start(),
  {ok, Room} = kaboose:get_a_room(Server),
  ?assertEqual({error, "Answer format is invalid"}, kaboose:add_question(Room, {"a?", [{correct, "a"}, 1]})).

add_question_description_empty_test() ->
  {ok, Server} = kaboose:start(),
  {ok, Room} = kaboose:get_a_room(Server),
  ?assertEqual({error, "The input is not a non empty string"}, kaboose:add_question(Room, {"", [{correct, "a"}, 1]})).

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