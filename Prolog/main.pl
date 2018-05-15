#!/usr/bin/env swipl

:- initialization(go, main).

go :-
        concat_atom([3, '+', 4], ' ', SingleArg),
        term_to_atom(Term, SingleArg),
        Val is Term,
        format('Result is ~w~n', [Val]).

expert(suzuko, scratch).
expert(yoshiki, volleyball).

learner(yoshiki, scratch).
learner(ken, scratch).
learner(ken, volleyball).

cooks(haruko, cuisine(japanese)).

teaches(X, cuisine(Y)) :- cooks(X, cuisine(Y)).
teaches(X, Y) :- expert(X, S), learner(Y, S).
