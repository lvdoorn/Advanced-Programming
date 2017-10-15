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
%% API
new(Global) -> 
    gen_server:start(?MODULE, [Global], []).

route(Flamingo, Prefixes, Action, Arg) -> 
	gen_server:call(Flamingo, {route, Prefixes, Action, Arg}).

request(Flamingo, Request, From, Ref) -> 
	gen_server:cast(Flamingo, {request, Request, From, Ref}).

%% Internal

init(Env) ->
    {ok, {Env, #{}}}.

handle_call({route, Prefixes, Action, Arg}, _From, {Env, RoutingGroups}) ->
	% Validated Prefixes
	{ok, State} = Action:initialise(Arg),
	Id = spawn(fun() -> routing_group({Action, State}) end),
	NewRoutingGroups = maps:put(Id, Prefixes, RoutingGroups),
    {reply, {ok, Id}, {Env, NewRoutingGroups}}.

handle_cast({request, Request = {Path, _}, From, Ref}, Global = {Env, RoutingGroups}) ->
	Id = getPidForPath(Path, RoutingGroups),
	case Id =:= [] of
		true -> From ! {Ref, {404, no_matching_routes}};
		false -> Id ! {Request, Env, From, Ref}
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

% TODO - get prefixes, this is dumb implementation
getPidForPath(Path, RoutingGroups) ->
	GetFilteredMapKeys = maps:keys(maps:filter(fun(_,List) -> lists:member(Path, List) end, RoutingGroups)),
	case GetFilteredMapKeys =:= [] of 
		true -> GetFilteredMapKeys;
		false -> hd(GetFilteredMapKeys)
	end.
