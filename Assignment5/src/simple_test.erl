-module(simple_test).
-export([runall/0
       , test/0
       , testStart/0
       ]).


runall() -> 
  test(),
  testStart().

test() -> 
  {ok, Server} = kaboose:start(),
  {ok, Room} = kaboose:get_a_room(Server),
  _ = kaboose:add_question(Room, {"abcd?", ["abc", {correct, "def"}]}),
  _ = kaboose:add_question(Room, {"abc?", ["abc", {correct, "def"}]}),
  _ = kaboose:add_question(Room, {"ab?", ["abc", {correct, "def"}]}),
  List = kaboose:get_questions(Room),
  IOList = io_lib:format("~w", [List]),
  FlatList = lists:flatten(IOList),
  io:fwrite(FlatList).
testStart() -> 
  {ok, Server} = kaboose:start().