:- module(hprolog,
	  [ prolog_flag/3,		% +Flag, -Old, +New
	    term_variables/2,		% +Term, -Vars
	    append_lists/2,		% +ListOfLists, -List
	    nth/3,			% ?Index, ?List, ?Element
	    substitute/4,		% +OldVal, +OldList, +NewVal, -NewList
	    memberchk_eq/2,		% +Val, +List
	    intersect_eq/3,		% +List1, +List2, -Intersection
	    list_difference_eq/3,	% +List, -Subtract, -Rest
	    take/3,			% +N, +List, -FirstElements
	    max_go_list/2,		% +List, -Max
	    or_list/2			% +ListOfInts, -BitwiseOr
	  ]).
:- use_module(library(lists)).

%	prolog_flag(+Flag, -Old, +New)
%	
%	Combine ISO prolog flag reading and writing

prolog_flag(Flag, Old, New) :-
	current_prolog_flag(Flag, Old),
	(   Old == New
	->  true
	;   set_prolog_flag(Flag, New)
	).

		 /*******************************
		 *	    TERM HACKING	*
		 *******************************/

%	term_variables(+Term, -Vars)
%	
%	Unify Vars with a list containing all variables in Term

term_variables(Term, Vars) :-
	free_variables(Term, Vars).


		 /*******************************
		 *      MORE LIST OPERATIONS	*
		 *******************************/

%	append_lists(+ListOfLists, -List)
%	
%	Convert a one-level nested list into a flat one.  E.g.
%	append_lists([[a,b], [c]], X) --> X = [a,b,c].  See also
%	flatten/3.

append_lists([],[]).
append_lists([X|Xs],L) :-
	append(X,T,L),
	append_lists(Xs,T).


%	nth(?Index, ?List, ?Element)
%	
%	Same as nth1/3

nth(Index, List, Element) :-
	nth1(Index, List, Element).


%	substitute(+OldVal, +OldList, +NewVal, -NewList)
%	
%	Substitute OldVal by NewVal in OldList and unify the result
%	with NewList.  JW: Shouldn't this be called substitute_eq/4?

substitute(_, [], _, []) :- ! .
substitute(X, [U|Us], Y, [V|Vs]) :-
        (   X == U
	->  V = Y,
            substitute(X, Us, Y, Vs)
        ;   V = U,
            substitute(X, Us, Y, Vs)
        ).

%	memberchk_eq(+Val, +List)
%	
%	Deterministic check of membership using == rather than
%	unification.

memberchk_eq(X, [Y|Ys]) :-
   (   X == Y
   ->  true
   ;   memberchk_eq(X, Ys)
   ).


%	list_difference_eq(+List, -Subtract, -Rest)
%	
%	Delete all elements of Subtract from List and unify the result
%	with Rest.  Element comparision is done using ==/2.

list_difference_eq([],_,[]).
list_difference_eq([X|Xs],Ys,L) :-
	(   memberchk_eq(X,Ys)
	->  list_difference_eq(Xs,Ys,L)
	;   L = [X|T],
	    list_difference_eq(Xs,Ys,T)
	).

%	intersect_eq(+List1, +List2, -Intersection)
%	
%	Determine the intersection of two lists without unifying values.

intersect_eq([], _, []).
intersect_eq([X|Xs], Ys, L) :-
	(   memberchk_eq(X, Ys)
	->  L = [X|T],
	    intersect_eq(Xs, Ys, T)
	;   intersect_eq(Xs, Ys, L)
	).


%	take(+N, +List, -FirstElements)
%	
%	Take the first  N  elements  from   List  and  unify  this  with
%	FirstElements. The definition is based   on the GNU-Prolog lists
%	library. Implementation by Jan Wielemaker.

take(0, _, []) :- !.
take(N, [H|TA], [H|TB]) :-
	N > 0,
	N2 is N - 1,
	take(N2, TA, TB).


%	max_go_list(+List, -Max)
%	
%	Return the maximum of List in the standard order of terms.

max_go_list([H|T], Max) :-
	max_go_list(T, H, Max).

max_go_list([], Max, Max).
max_go_list([H|T], X, Max) :-
        (   H @=< X
	->  max_go_list(T, X, Max)
        ;   max_go_list(T, H, Max)
        ).

%	or_list(+ListOfInts, -BitwiseOr)
%	
%	Do a bitwise disjuction over all integer members of ListOfInts.

or_list(L, Or) :-
	or_list(L, 0, Or).

or_list([], Or, Or).
or_list([H|T], Or0, Or) :-
	Or1 is H \/ Or0,
	or_list(T, Or1, Or).

