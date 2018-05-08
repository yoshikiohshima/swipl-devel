#!/usr/bin/env swipl

:- initialization(go, main).

go :-
        concat_atom([3, '+', 4], ' ', SingleArg),
        term_to_atom(Term, SingleArg),
        Val is Term,
        format('Result is ~w~n', [Val]).

