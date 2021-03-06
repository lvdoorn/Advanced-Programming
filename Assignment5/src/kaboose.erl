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
       , guess/3
       , validateNonEmptyString/1
       , validateAnswer/1
       , isConductor/3
       ]).

start() -> wrapInTry(fun() -> 
                        {ok, spawn(fun loop/0)}
                     end).

get_a_room(Server) -> request_reply(Server, new_room).

add_question(Room, Question) -> request_reply(Room, {add_question, Question}).

get_questions(Room) -> request_reply(Room, get_questions).

play(Room) -> request_reply(Room, play).

next(ActiveRoom) -> request_reply(ActiveRoom, next).

timesup(ActiveRoom) -> request_reply(ActiveRoom, timesup).

join(ActiveRoom, Nick) -> request_reply(ActiveRoom, {join, Nick}).

leave(ActiveRoom, Ref) -> request_reply_async(ActiveRoom, {leave, Ref}).

rejoin(ActiveRoom, Ref) -> request_reply_async(ActiveRoom, {rejoin, Ref}).

guess(ActiveRoom, Ref, Index) -> request_reply_async(ActiveRoom, {guess, Ref, Index}).

% Copied from lecture slides
request_reply(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    {Pid, Response} -> Response
  end.

request_reply_async(Pid, Request) ->
  Pid ! {self(), Request}.

loop() ->
  receive
    {From, new_room} -> 
      try
        From ! {self(), {ok, spawn(fun() -> roomLoop([]) end)}}
      catch
        _:Error -> request_reply_async(From, errorTuple(Error))
      after 
        loop()
      end
  end.

roomLoop(Questions) ->
  receive
    {From, {add_question, Question}} ->
      case validateQuestion(Question, From) of
        true -> From ! {self(), ok},
                roomLoop(Questions ++ [Question]);
        false -> roomLoop(Questions)
      end;

    {From, get_questions} ->
      From ! {self(), Questions},
      roomLoop(Questions);

    {From, play} -> 
      From ! {self(), {spawn(fun() -> activeRoomLoop(Questions, #{}, From, false, [], #{}, #{}, 0, []) end), From}},
      roomLoop(Questions)
  end.
 
% Players: Map(Ref -> {Nickname, Active[true/false]})
% Dist: [Counter]
% LastQ: Map(Ref -> Counter)
% Total: Map(Ref -> Counter)
% Time: Nanoseconds since the question was made active
% HaveGuessed: [Ref]
activeRoomLoop(Questions = [{Description, Answers}|T], Players, CRef, Active, Dist, LastQ, Total, Time, HaveGuessed) -> 
  receive
  % next, LastQ is reset here
    {From, next} -> 
      case validateNextQuestion(From, CRef, Active) of
        true -> From ! {self(), {ok, {Description, Answers}}},
                activeRoomLoop(Questions, Players, CRef, true, counters(length(Answers)), defaultMap(maps:keys(Players)), Total, erlang:system_time(), []);
        false -> activeRoomLoop(Questions, Players, CRef, Active, Dist, LastQ, Total, Time, HaveGuessed)
      end;

  % timesup
    {From, timesup} ->
      case validateTimesup(From, CRef, Active) of 
        false -> activeRoomLoop(Questions, Players, CRef, Active, Dist, LastQ, Total, Time, HaveGuessed);
        true ->
          Final = if
            T =:= [] -> true;
            true -> false
          end,
          maps:map(fun(K, V) -> counter:increment(maps:get(K, Total), counter:get(V)) end, LastQ),
          From ! {self(), {ok, lists:map(fun(E) -> counter:get(E) end, Dist), keyValueMap(LastQ, fun(K) -> getName(K, Players) end, fun(V) -> counter:get(V) end), keyValueMap(Total, fun(K) -> getName(K, Players) end, fun(V) -> counter:get(V) end), Final}},
          if
            Final -> end_process;
            true -> activeRoomLoop(T, Players, CRef, false, Dist, LastQ, Total, Time, HaveGuessed)
          end
      end;
      
  % join
    {From, {join, Nick}} ->
      GetNickFromPlayers = maps:filter(fun(_,{Nickname, _}) -> Nickname =:= Nick end, Players),
      IsNickAvailable = GetNickFromPlayers =:= #{},
      case IsNickAvailable of
        true -> From ! {self(), {ok, From}},
                CRef ! {CRef, {player_joined, Nick, activePlayers(Players) + 1}},
                activeRoomLoop(Questions, maps:put(From, {Nick, true}, Players), CRef, Active, Dist, maps:put(From, counter:start(), LastQ), maps:put(From, counter:start(), Total), Time, HaveGuessed);
        false -> request_reply_async(From, {error, Nick, is_taken}), 
                 activeRoomLoop(Questions, Players, CRef, Active, Dist, LastQ, Total, Time, HaveGuessed)
      end;

  % leave
    {_From, {leave, Ref}} ->
      Name = getName(Ref, Players),
      CRef ! {CRef, {player_left, Name, activePlayers(Players) - 1}},
      activeRoomLoop(Questions, maps:put(Ref, {Name, false}, Players), CRef, Active, Dist, LastQ, Total, Time, HaveGuessed);
  
  % rejoin
    {_From, {rejoin, Ref}} ->
      Name = getName(Ref, Players),
      CRef ! {CRef, {player_joined, Name, activePlayers(Players) + 1}},
      activeRoomLoop(Questions, maps:put(Ref, {Name, true}, Players), CRef, Active, Dist, LastQ, Total, Time, HaveGuessed);

  % guess, score is added to LastQ here
    {From, {guess, Ref, Index}} ->
    Timesup = (erlang:system_time() - Time < 500000000),
    Score = if
      Timesup -> 1000;
      true -> 500
    end,
    case isGuessValid(Answers, Index, Active, From, HaveGuessed) of
      true -> 
        NewHaveGuessed = [From|HaveGuessed],
        counter:increment(lists:nth(Index, Dist)),
        case lists:nth(Index, Answers) of
          {correct, _} -> counter:increment(maps:get(Ref, LastQ), Score);
          _ -> doNothing
        end;
      false -> NewHaveGuessed = HaveGuessed
    end,
    activeRoomLoop(Questions, Players, CRef, Active, Dist, LastQ, Total, Time, NewHaveGuessed)
  end.

% Returns a list of length Length filled with counters.
counters(0) -> [];
counters(Length) -> [counter:start()|counters(Length - 1)].

% Every element in the input list will map to a counter at zero.
defaultMap([]) -> #{};
defaultMap([H|T]) -> maps:put(H, counter:start(), defaultMap(T)).

% activePlayers(Map) -> length(lists:filter(fun(V) -> case V of {_, true} -> true; (_) -> false end, maps:values(Map))).
activePlayers(Map) -> length(lists:filter(fun(V) -> matchV(V) end, maps:values(Map))).
matchV(V) -> case V of 
  {_, true} -> true;
  (_) -> false
end.

getName(Ref, Players) ->
  {Name, _} = maps:get(Ref, Players),
  Name.

% For each (K, V) in Map inserts (Fun1(K), Fun2(V)) into the resulting map.
keyValueMap(Map, Fun1, Fun2) -> 
  List = maps:to_list(Map),
  Mapped = lists:map(fun({A, B}) -> {Fun1(A), Fun2(B)} end, List),
  maps:from_list(Mapped).

isGuessValid(Answers, Index, Active, From, HaveGuessed) ->
    IsQuestionActive = Active =:= true,
    IsIndexInRange = Index >= 1 andalso Index =< length(Answers),
    GuessedBefore = lists:member(From, HaveGuessed),
    IsQuestionActive andalso IsIndexInRange andalso not(GuessedBefore).

wrapInTry(F) -> try F()
                catch
                    _:{From, Error} -> From ! {self(), errorTuple(Error)};
                    _:Error -> errorTuple(Error)
                end.

errorTuple(Msg) -> {error, Msg}.

validateQuestion(Question, From) ->
  IsQuestionValid = case Question of
                      {_, _} -> Question;
                      _ -> request_reply_async(From, errorTuple(format_is_invalid)), false
                    end,

  IsDescValid = case IsQuestionValid of
                  {Description, _} ->
                  case validateNonEmptyString(Description) of
                    ok -> ok;
                    Msg -> request_reply_async(From, errorTuple(Msg)), false
                  end;
                  false -> false
                end,

  IsAnswerValid = case IsQuestionValid of
                    {_, Answer} ->
                      case validateAnswer(Answer) of
                        ok -> ok;
                        Msg1 -> request_reply_async(From, errorTuple(Msg1)), false
                      end;
                    false -> false
                  end,
  IsDescValid =:= ok andalso IsAnswerValid =:= ok.

isConductor(From, CRef, Msg) -> case From =:= CRef of
                                  true -> true;
                                  false -> request_reply_async(From, errorTuple(Msg)), false
                                end.

validateTimesup(From, CRef, Active) ->
  IsConductor = isConductor(From, CRef, nice_try),

  HasActiveQuestion = case Active of 
                          true -> true;
                          false -> request_reply_async(From, errorTuple(no_question_asked)), false
                      end,
  IsConductor andalso HasActiveQuestion.

validateNextQuestion(From, CRef, Active) ->
  IsConductor = isConductor(From, CRef, who_are_you),
  HasNoActiveQuestion = case Active of 
                          true -> request_reply_async(From, errorTuple(has_active_question)), false;
                          false -> true
                        end,
  IsConductor andalso HasNoActiveQuestion.


% Input validation section
isNonEmptyString(Str) -> io_lib:printable_list(Str) andalso Str =/= "".
                                
validateNonEmptyString(Str) -> case isNonEmptyString(Str) of
                                true -> ok;
                                false -> not_a_non_empty_string
                              end.

% Answer is list, not empty
validateAnswer(List) -> case is_list(List) andalso List =/= [] of
                          true -> validateAnswerHelper(List, false);
                          false -> not_a_non_empty_list
                        end.

% Answer list: at least one {correct, _}, non empty strings, {correct, "non empty string"} 
validateAnswerHelper([], HasCorrectOption) -> case HasCorrectOption of
                                                true -> ok;
                                                false -> no_correct_options
                                              end;
validateAnswerHelper([{correct, Text} | T], _) -> throwOrContinue(Text, T, true);
validateAnswerHelper([Text | T], HasCorrectOption) -> throwOrContinue(Text, T, HasCorrectOption);
validateAnswerHelper(_, _) -> format_is_invalid.

throwOrContinue(Text, T, HasCorrectOption) -> case isNonEmptyString(Text) of 
                                                true -> validateAnswerHelper(T, HasCorrectOption); 
                                                false -> answer_format_is_invalid
                                              end.
