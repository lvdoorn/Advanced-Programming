g1([person(kara, [barry, clark]),
    person(bruce, [clark, oliver]),
    person(barry, [kara, oliver]),
    person(clark, [oliver, kara]),
    person(oliver, [kara])]).


likes(G, X, Y) :- getFriendList(G, X, FriendList),
                  elem(Y, FriendList).

dislikes(G, X, Y) :- likes(G, Y, X),
                     getFriendList(G, X, XFriendList),
                     isNotElemInFriendList(G, Y, XFriendList).

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
isFriendly([person(Name, _)|T], X, Original) :- likes(Original, Name, X), likes(Original, X, Name), isFriendly(T, X, Original).
isFriendly([person(_, Friendlist)|T], X, Original) :- isNotElemInFriendList(Original, X, Friendlist), isFriendly(T, X, Original).

hostile(G, X) :- isHostile(G, X, G).

isHostile([], _, _).
isHostile([person(Name, _)|T], Name, Original) :- isHostile(T, Name, Original).
isHostile([person(Name, _)|T], X, Original) :- likes(Original, Name, X), dislikes(Original, X, Name), isHostile(T, X, Original).
isHostile([person(_, Friendlist)|T], X, Original) :- isNotElemInFriendList(Original, X, Friendlist), isHostile(T, X, Original).

isHight(_, []).
isHight(X, [H|T]) :- X>H, isHight(X,T).

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
