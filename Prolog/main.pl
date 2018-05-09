#!/usr/bin/env swipl

:- initialization(go, main).

go :-
        current_prolog_flag(ios, IOS),
        format('ios is ~w~n', IOS),
        concat_atom([3, '+', 4], ' ', SingleArg),
        term_to_atom(Term, SingleArg),
        Val is Term,
        format('Result is ~w~n', [Val]).

