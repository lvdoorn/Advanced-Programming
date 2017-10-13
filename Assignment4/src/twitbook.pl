likes(G, X, Y) :- getFriendList(G, X, FriendList),
                  elem(Y, FriendList).

doesNotLike(G, X, Y) :- getFriendList(G, X, Xfriends),
	                isNotElemInFriendList(G, Y, Xfriends).

dislikes(G, X, Y) :- likes(G, Y, X), doesNotLike(G, X, Y).

popular(G, X) :- getFriendList(G, X, FriendList),
                 isPopular(G, X, FriendList).

isPopular(_, _, []).
isPopular(G, X, [Friend|RestFriends]) :- likes(G, Friend, X),
                                         isPopular(G, X, RestFriends).

outcast(G, X) :- getFriendList(G, X, FriendList),
                 isOutcast(G, X, FriendList).

isOutcast(_, _, []).
isOutcast(G, X, [Friend|RestFriends]) :- dislikes(G, Friend, X),
                                         isOutcast(G, X, RestFriends).

friendly(G, X) :- isPerson(G, X), isFriendly(G, X, G).

isFriendly([], _, _).
isFriendly([person(Name, _)|T], Name, Original) :- isFriendly(T, Name, Original).
isFriendly([person(Name, _)|T], X, Original) :- likes(Original, Name, X),
	                                        likes(Original, X, Name),
						isFriendly(T, X, Original).
isFriendly([person(_, Friendlist)|T], X, Original) :- isNotElemInFriendList(Original, X, Friendlist),
	                                              isFriendly(T, X, Original).

hostile(G, X) :- isPerson(G, X), isHostile(G, X, G).

isHostile([], _, _).
isHostile([person(Name, _)|T], Name, Original) :- isHostile(T, Name, Original).
isHostile([person(Name, _)|T], X, Original) :- likes(Original, Name, X),
	                                       dislikes(Original, X, Name),
					       isHostile(T, X, Original).
isHostile([person(_, Friendlist)|T], X, Original) :- isNotElemInFriendList(Original, X, Friendlist),
	                                             isHostile(T, X, Original).

admires(G, X, Y) :- different(G, X, Y), admiresHelper(G, X, Y, [X, Y]).
admiresHelper(G, X, Y, _) :- likes(G, X, Y).
admiresHelper(G, X, Y, List) :- doesNotLike(G, X, Y), likes(G, X, Z), isNotElemInFriendList(G, Z, List), admiresHelper(G, Z, Y, [Z|List]).


%% Get all people the X admires. Y should not be in that list.
indifferent(Original, X, Y) :- getAllAdmires(Original, [], [X], XAdmires), 
							   noElem(Original, Y, XAdmires).

%% Get everybody that X admires.
%% This is done with 2 accumulator helper lists, 
%% 		AdmiresList - current list of unique people that X admires
%% 		ToBeChecked - list of people to be added to AdmiresList; 
%% 					  foreach person in this list, add its friends to ToBeChecked

getAllAdmires(_, AdmiresList, [], AdmiresList).
getAllAdmires(Original, AdmiresList, [ToCheck|ToBeChecked], L) :-
				getFriendList(Original, ToCheck, ToCheckFriends),
				appendlist(AdmiresList, ToBeChecked, CurrentAdmires),
				removeSublistFromList(Original, CurrentAdmires, ToCheckFriends, NewFriendsToBeChecked),

				appendlist(ToBeChecked, NewFriendsToBeChecked, NewToBeChecked),
				appendlist(AdmiresList, [ToCheck], NewCheckedFriends),
				getAllAdmires(Original, NewCheckedFriends, NewToBeChecked, L).


appendlist([], X, X).
appendlist([T|H], X, [T|L]) :- appendlist(H, X, L).

same_world(_, _, _).

noElem(_, _, []).
noElem(Original, X, [Y|Z]) :- different(Original, X, Y), 
							  noElem(Original, X, Z).

removeSublistFromList(_, _, [], []).
removeSublistFromList(Original, Y, [X|W], Z):- elem(X, Y), 
								removeSublistFromList(Original, Y, W, Z).
removeSublistFromList(Original, Y, [X|W], [X|Z]) :- noElem(Original, X, Y), 
									 removeSublistFromList(Original, Y, W, Z).

% Helper functions
isNotElemInFriendList(_, _, []).
isNotElemInFriendList(G, X, [Friend|RestFriends]) :- different(G, X, Friend),
                                                     isNotElemInFriendList(G, X, RestFriends).
elem(X, [X|_]).
elem(X, [_|T]) :- elem(X, T).

getFriendList([person(X, FriendList)|_], X, FriendList).
getFriendList([_|T], X, FriendList) :- getFriendList(T, X, FriendList).

removeElem(X, [X|T], T).
removeElem(X, [H|T], [H|T1]) :- removeElem(X, T, T1).

%% different = Should be able to remove twice from G <=> X, Y distinct
different(G, X, Y) :- removeElem(person(X, _), G, NewG),
                      removeElem(person(Y, _), NewG, _).

isPerson([person(Name, _)|_], Name).
isPerson([_|T], Name) :- isPerson(T, Name).
