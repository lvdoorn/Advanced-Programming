%% swipl -f tests.pl -t run_tests

:- consult(twitbook).

g1([person(kara, [barry, clark]),
    person(bruce, [clark, oliver]),
    person(barry, [kara, oliver]),
    person(clark, [oliver, kara]),
    person(oliver, [kara])]).

:- begin_tests(likes).

test(likes_succede, [true]) :- g1(G), likes(G, bruce, clark), !.
test(likes_fail, [fail]) :- g1(G), likes(G, clark, bruce).
test(likes_all, all(X == [clark, oliver])) :- g1(G), likes(G, bruce, X).

:- end_tests(likes).

:- begin_tests(dislikes).

test(different_succede, [true]) :- g1(G), different(G, clark, bruce), !.
test(different_fail, [fail]) :- g1(G), different(G, clark, clark).
test(different_all, all(X == [kara,bruce,barry,oliver])) :- g1(G), different(G, clark, X).


test(dislikes_succede, [true]) :- g1(G), dislikes(G, clark, bruce), !.
test(dislikes_fail, [fail]) :- g1(G), dislikes(G, bruce, clark).
test(dislikes_all, all(X == [bruce,barry,clark])) :- g1(G), dislikes(G, oliver, X).

:- end_tests(dislikes).

:- begin_tests(popular).

test(popular_succede, [true]) :- g1(G), popular(G, kara), !.
test(popular_fail, [fail]) :- g1(G), popular(G, clark).
test(popular_all, all(X == [kara])) :- g1(G), popular(G, X).

:- end_tests(popular).

:- begin_tests(outcast).

test(outcast_succede, [true]) :- g1(G), outcast(G, bruce), !.
test(outcast_fail, [fail]) :- g1(G), outcast(G, kara).
test(outcast_all, all(X == [bruce,oliver])) :- g1(G), outcast(G, X).

:- end_tests(outcast).

:- begin_tests(friendly).

test(friendly_succede, [true]) :- g1(G), friendly(G, barry), !.
test(friendly_succede, [true]) :- g1(G), friendly(G, bruce), !.

:- end_tests(friendly).

:- begin_tests(hostile).

test(hostile_succede, [true]) :- g1(G), hostile(G, oliver), !.
test(hostile_succede, [true]) :- g1(G), hostile(G, bruce), !.
test(hostile_fail, [fail]) :- g1(G), hostile(G, kara).

:- end_tests(hostile).

:- begin_tests(admires).

test(admires_succede, [true]) :- g1(G), admires(G, bruce, kara), !.
test(admires_fail, [fail]) :- g1(G), admires(G, kara, bruce).

:- end_tests(admires).
