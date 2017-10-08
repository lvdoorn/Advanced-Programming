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
       , isNonEmptyString/1]).
-import(maps, [new/0, find/2, get/2]).

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

leave(ActiveRoom, Ref) -> request_reply(ActiveRoom, {leave, Ref}).

rejoin(ActiveRoom, Ref) -> request_reply(ActiveRoom, {rejoin, Ref}).

guess(ActiveRoom, Ref, Index) -> request_reply(ActiveRoom, {guess, Ref, Index}).

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
            _:Error -> request_reply_async(From, {error, Error})
        after 
            loop()
        end
  end.


roomLoop(Questions) ->
  receive
    {From, {add_question, Question}} ->
      {V1, V2} = validateQuestion(Question, From),

      case V1 =:= ok andalso V2 =:= ok of
        true -> From ! {self(), ok},
                roomLoop(Questions ++ [Question]);
        false -> roomLoop(Questions)
      end;

    {From, get_questions} ->
      From ! {self(), Questions},
      roomLoop(Questions);

    {From, play} -> 
      From ! {self(), {spawn(fun() -> activeRoomLoop([{"dummy", ["dummy"]}|Questions], #{}, From, false, [], #{}, #{}) end), From}},
      roomLoop(Questions)
  end.
 


% activeRoomLoop works like this: we start with a dummy question which is inactive
% when next/1 is called, we return the second element in the questions list and make it active.
% when timesup/1 is called, the current question is simply made inactive.

% Players: Map(Ref -> {Nickname, Active[true/false]})

activeRoomLoop([{Description, Answers}|T], Players, CRef, Active, Dist, LastQ, Total) -> 
  Questions = [{Description, Answers}|T],
  receive

  % next
    {From, next} -> 
      From ! {self(), {ok, lists:nth(2, Questions)}},
      activeRoomLoop(lists:nthtail(1, Questions), Players, CRef, true, [], #{}, #{}); % TODO errors

  % timesup
    {From, timesup} ->
      % TODO: calculate final
      From ! {ok, Dist, LastQ, Total, false},
      % TODO: calculate score.
      activeRoomLoop(Questions, Players, CRef, false, Dist, #{}, #{}); % TODO return

  % join
    {From, {join, Nick}} ->
      From ! {self(), {ok, From}},
      activeRoomLoop(Questions, maps:put(From, {Nick, true}, Players), CRef, Active, Dist, LastQ, Total);
      % TODO: send message to conductor
      % TODO: error if Nick is taken

  %leave
    {From, {leave, Ref}} ->
      % TODO: inform conductor.
      From ! {self() , ok},
      {Name, _} = maps:get(Ref, Players),
      activeRoomLoop(Questions, maps:put(From, {Name, false}), CRef, Active, Dist, LastQ, Total);
  
  % rejoin
    {From, {rejoin, Ref}} ->
      From ! {self() , ok},
      {Name, _} = maps:get(Ref, Players),
      activeRoomLoop(Questions, maps:put(Ref, {Name, true}), CRef, Active, Dist, LastQ, Total);

  % guess
    {From, {guess, Ref, Index}} ->
      From ! {self(), ok}

  end.

wrapInTry(F) -> try F()
                catch
                    _:{From, Error} -> From ! {self(), {error, Error}};
                    _:Error -> {error, Error}
                end.


validateQuestion(Question, From) ->
   Q = case Question of
         {_, _} -> Question;
           _ -> request_reply_async(From, {error, format_is_invalid}), false
        end,

   V1 = case Q of
         {Description, _} ->
          case validateNonEmptyString(Description) of
            ok -> ok;
            Msg -> request_reply_async(From, {error, Msg}), false
          end;
        false -> false
        end,

   V2 = case Q of
        {_, Answer} ->
          case validateAnswer(Answer) of
            ok -> ok;
            Msg1 -> request_reply_async(From, {error, Msg1}), false
          end;
        false -> false
        end,
{V1, V2}.


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