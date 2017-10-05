-module(simple_test).
-export([test/0]).


test() -> 
  {ok, Server} = kaboose:start(),
  {ok, Room} = kaboose:get_a_room(Server),
  _ = kaboose:add_question(Room, {"abcd?", ["abc", {correct, "def"}]}),
  _ = kaboose:add_question(Room, {"abc?", ["abc", {correct, "def"}]}),
  _ = kaboose:add_question(Room, {"ab?", ["abc", {correct, "def"}]}),
  List = kaboose:get_questions(Room).
