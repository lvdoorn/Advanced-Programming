-module(flamingo).
-behaviour(gen_server).

%% API
-export([ new/1
        , route/4
        , request/4
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

%% API
new(Global) -> 
  gen_server:start(?MODULE, Global, []).

route(Flamingo, Prefixes, Action, Arg) -> 
  gen_server:call(Flamingo, {route, Prefixes, Action, Arg}).

request(Flamingo, Request, From, Ref) -> 
  gen_server:cast(Flamingo, {request, Request, From, Ref}).

%% Internal

init(Env) ->
  {ok, {Env, #{}}}.

handle_call({route, Prefixes, Action, Arg}, _From, OldEnv = {Env, RoutingGroups}) ->
  case checkPrefixes(Prefixes) of
    true -> {ok, State} = Action:initialise(Arg),
      Id = spawn(fun() -> routing_group({Action, State}) end),
      NewRoutingGroups = add_route_to_map(Prefixes, Id, RoutingGroups),
      {reply, {ok, Id}, {Env, NewRoutingGroups}};
    invalid_prefixes -> {reply, {error, invalid_prefixes}, OldEnv}
  end.




handle_cast({request, Request = {Path, _}, From, Ref}, Global = {Env, RoutingGroups}) ->
  case getActionModule(Path, RoutingGroups) of
    error -> From ! {Ref, {404, no_matching_routes}};
    Id -> Id ! {Request, Env, From, Ref}
  end,
  {noreply, Global}.

% TODO
handle_info(_Info, State) ->
  {noreply, State}.

% TODO
terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% Private functions
routing_group(LocalState = {Action, State}) ->
  receive
    {Request, Env, From, Ref} -> 
      case Action:action(Request, Env, State) of 
        {new_state, Content, NewState} ->
          From ! {Ref, {200, Content}},
          routing_group({Action, NewState});
        {no_change, Content} -> 
          From ! {Ref, {200, Content}},
          routing_group(LocalState);
        _ -> From ! {Ref, {500, error}},
         routing_group(LocalState)
      end;
    {From, _} -> From ! {self(), {500, error}},
      routing_group(LocalState)
  end.

% Maps each prefix to the corresponding routing group
add_route_to_map([], _, Map) -> Map;
add_route_to_map([H|Prefixes], Id, Map) ->
  add_route_to_map(Prefixes, Id, maps:put(H, Id, Map)).

getActionModule(Path, RoutingGroups) ->
  case longestPrefix(Path, maps:keys(RoutingGroups)) of
    [] -> error;
    Str -> maps:get(Str, RoutingGroups)
  end.

longestPrefix(Path, Prefixes) ->
  longestPrefixHelper(Path, Prefixes, "").

longestPrefixHelper(_, [], Max) -> Max;
longestPrefixHelper(Path, [H|Prefixes], Max) ->
  case string:prefix(Path, H) of
    nomatch -> longestPrefixHelper(Path, Prefixes, Max);
    _       -> if length(H) > length(Max) -> longestPrefixHelper(Path, Prefixes, H);
                  true                    -> longestPrefixHelper(Path, Prefixes, Max)
               end
  end.

checkPrefixes(Prefixes) ->
  SetLength = sets:size(sets:from_list(Prefixes)),
  ListLength = length(Prefixes),
  ContainsEmpty = lists:any(fun(E) -> E =:= "" end, Prefixes),
  if  SetLength =/= ListLength -> invalid_prefixes;
      ContainsEmpty -> invalid_prefixes;
      ListLength =:= 0 -> invalid_prefixes;
      true -> true
  end.
