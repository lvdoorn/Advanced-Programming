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

friendly(G, X) :- isFriendly(G, X, G).

isFriendly([], _, _).
isFriendly([person(Name, _)|T], Name, Original) :- isFriendly(T, Name, Original).
isFriendly([person(Name, _)|T], X, Original) :- likes(Original, Name, X),
	                                        likes(Original, X, Name),
						isFriendly(T, X, Original).
isFriendly([person(_, Friendlist)|T], X, Original) :- isNotElemInFriendList(Original, X, Friendlist),
	                                              isFriendly(T, X, Original).

hostile(G, X) :- isHostile(G, X, G).

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

indifferent(G, X, Y) :- different(G, X, Y), doesNotLike(G, X, Y), indifferentHelper(G, X, Y, G).
indifferentHelper([person(Name, _)|T], Name, Y, Original) :- indifferentHelper(T, Name, Y, Original). % Everyone is indifferent to themselves for this predicate
indifferentHelper([person(Name, _)|T], X, Y, Original) :- doesNotLike(Original, Name, Y), indifferentHelper(T, X, Y, Original).
indifferentHelper([person(Name, _)|T], X, Y, Original) :- likes(Original, X, Name), indifferentHelper(Original, Name, Y, Original), indifferentHelper(T, X, Y, Original).

%
%	indifferent(G, X, Y) :- getFriendList(G, X, Friendlist),
% indifferentHelper(G, X, Y, Friendlist).
% indifferentHelper(G, X, Y, Friendlist) :- isNotElemInFriendList(G, Y,
% Friendlist).
%
% For every member in the graph we check if:
%     - either X does not like him and we are done
%     - or X does like him and we need to check that he does not like Y,
%	nor likes anyone that likes Y

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
