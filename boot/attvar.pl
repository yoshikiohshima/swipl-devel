/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module($attvar,
	  [ '$wakeup'/1,		% +Wakeup list
	    freeze/2			% +Var, :Goal
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Attributed variable and corouting support based on attributed variables.
This  module  is  complemented  with  C-defined  predicates  defined  in
pl-attvar.c
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%	'$wakeup'(+List)
%	
%	Called from the kernel if assignments have been made to
%	attributed variables.

'$wakeup'([]).
'$wakeup'(wakeup(Attribute, Value, Rest)) :-
	call_all_attr_uhooks(Attribute, Value),
	'$wakeup'(Rest).

call_all_attr_uhooks([], _).
call_all_attr_uhooks(att(Module, AttVal, Rest), Value) :-
	uhook(Module, AttVal, Value),
	call_all_attr_uhooks(Rest, Value).


%	uhook(+AttributeName, +AttributeValue, +Value)
%	
%	Run the unify hook for attributed named AttributeName after
%	assigning an attvar with attribute AttributeValue the value
%	Value.
%	
%	This predicate deals with reserved attribute names to avoid
%	the meta-call overhead.

uhook(freeze, Goal, Y) :- !,
	(   attvar(Y)
	->  (   get_attr(Y, freeze, G2)
	    ->	put_attr(Y, freeze, (G2, Goal))
	    ;	put_attr(Y, freeze, Goal)
	    )
	;   Goal
	).
uhook(Module, AttVal, Value) :-
	Module:attr_unify_hook(AttVal, Value).


%	freeze(+Var, :Goal)
%	
%	Suspend execution of Goal until Var is unbound.

:- module_transparent freeze/2.

freeze(Var, Goal) :-
	'$freeze'(Var, Goal), !.	% Succeeds if delayed
freeze(_, Goal) :-
	Goal.
