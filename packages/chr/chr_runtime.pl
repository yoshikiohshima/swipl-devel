/*  $Id$

    Part of CHR (Constraint Handling Rules)

    Author:        Christian Holzbaur and Tom Schrijvers
    E-mail:        christian@ai.univie.ac.at
		   Tom.Schrijvers@cs.kuleuven.ac.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2003-2004, K.U. Leuven

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

    Distributed with SWI-Prolog under the above conditions with
    permission from the authors.
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%       _                             _   _                
%%   ___| |__  _ __   _ __ _   _ _ __ | |_(_)_ __ ___   ___ 
%%  / __| '_ \| '__| | '__| | | | '_ \| __| | '_ ` _ \ / _ \
%% | (__| | | | |    | |  | |_| | | | | |_| | | | | | |  __/
%%  \___|_| |_|_|    |_|   \__,_|_| |_|\__|_|_| |_| |_|\___|
%%
%% hProlog CHR runtime:
%%
%% 	* based on the SICStus CHR runtime by Christian Holzbaur
%% 
%%          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%          %  Constraint Handling Rules		      version 2.2 %
%%          %								  %
%%          %  (c) Copyright 1996-98					  %
%%          %  LMU, Muenchen						  %
%% 	    %								  %
%%          %  File:   chr.pl						  %
%%          %  Author: Christian Holzbaur	christian@ai.univie.ac.at %
%%          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%%	
%%	* modified by Tom Schrijvers, K.U.Leuven, Tom.Schrijvers@cs.kuleuven.ac.be
%%		- ported to hProlog
%%		- modified for eager suspension removal
%%
%%      * First working version: 6 June 2003
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SWI-Prolog changes
%% 
%% 	* Added initialization directives for saved-states
%%	* Renamed merge/3 --> sbag_merge/3 (name conflict)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(chr_runtime,
	  [ 'chr sbag_del_element'/3,
	    'chr sbag_member'/2,
	    'chr merge_attributes'/3,

	    'chr run_suspensions'/1,
	    'chr run_suspensions_loop'/1,

	    'chr insert_constraint_internal'/5,
	    'chr remove_constraint_internal'/2,
	    'chr allocate_constraint'/4,
	    'chr activate_constraint'/3,

	    'chr global_term_ref_1'/1,

	    'chr via_1'/2,
	    'chr via_2'/3,

	    'chr lock'/1,
	    'chr unlock'/1,
	    'chr not_locked'/1,

	    'chr update_mutable'/2,
	    'chr get_mutable'/2,

	    'chr novel_production'/2,
	    'chr extend_history'/2
	  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                                       
:- use_module(library(assoc)).
:- use_module(hprolog).
:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%   I N I T I A L I S A T I O N

?- initialization			% SWI
   nb_setval(id,0).

?- initialization			% SWI
   nb_setval(chr_global,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'chr merge_attributes'( As, Bs, Cs) :-
	sbag_union(As,Bs,Cs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'chr run_suspensions'( Slots) :-
	    run_suspensions( Slots).

'chr run_suspensions_loop'([]).
'chr run_suspensions_loop'([L|Ls]) :-
	run_suspensions(L),
	'chr run_suspensions_loop'(Ls).

run_suspensions([]).
run_suspensions([S|Next] ) :-
	%iter_next( State, S, Next),
	arg( 2, S, Mref),
	Mref = mutable(Status), % get_mutable( Status, Mref), % XXX Inlined
	( Status==active ->
	    update_mutable( triggered, Mref),
	    arg( 4, S, Gref),
	    Gref = mutable(Gen), % get_mutable( Gen, Gref), % XXX Inlined
	    Generation is Gen+1,
	    update_mutable( Generation, Gref),
	    arg( 3, S, Goal),
	    call( Goal),
	    					% get_mutable( Post, Mref), % XXX Inlined
	    ( Mref == mutable(triggered) ->	% Post==triggered ->
		update_mutable( removed, Mref)
	    ;
		true
	    )
	;
	    true
	),
	run_suspensions( Next).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
locked:attr_unify_hook(_,_) :- fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'chr lock'(T) :- 
	lock(T).

'chr unlock'(T) :-
	unlock(T).

'chr not_locked'(T) :-
	not_locked(T).

lock(T) :-
	( var(T) -> 
		put_attr( T, locked, x)
	;
		functor(T,_,N),
		lock_arg(N,T)
	).
		
lock_arg( 0, _) :- ! .
lock_arg( 1, T) :- ! , arg( 1, T, A), lock( A).
lock_arg( 2, T) :- ! , arg( 1, T, A), lock( A), arg( 2, T, B), lock( B).
lock_arg( N, T) :-
	arg( N, T, A),
	lock( A),
	M is N-1,
	lock_arg( M, T).

unlock(T) :-
	( var(T) ->
		del_attr( T, locked)
	;
		functor( T, _, N),
		unlock_arg( N, T)
	).

unlock_arg( 0, _) :- ! .
unlock_arg( 1, T) :- ! , arg( 1, T, A), unlock( A).
unlock_arg( 2, T) :- ! , arg( 1, T, A), unlock( A), arg( 2, T, B), unlock( B).
unlock_arg( N, T) :-
	arg( N, T, A),
	unlock( A),
	M is N-1,
	unlock_arg( M, T).

none_locked( []).
none_locked( [V|Vs]) :-
	not_locked( V),
	none_locked( Vs).

not_locked( V) :- 
	( var( V) ->
  		( get_attr( V, locked, _) ->
			fail
		;
			true
		)
	;
		true
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Eager removal from all chains.
%
'chr remove_constraint_internal'( Susp, Agenda) :-
	arg( 2, Susp, Mref),
	Mref = mutable(State), % get_mutable( State, Mref), % XXX Inlined
	update_mutable( removed, Mref),		% mark in any case
	( compound(State) ->			% passive/1
	    Agenda = []
	; State==removed ->
	    Agenda = []
	%; State==triggered ->
	%     Agenda = []
	;
            Susp =.. [_,_,_,_,_,_,_|Args],
	    term_variables( Args, Vars),
	    global_term_ref_1( Global),
	    Agenda = [Global|Vars]
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'chr via_1'( X, V) :- var(X), !, X=V.
'chr via_1'( T, V) :- compound(T), nonground( T, V), ! .
'chr via_1'( _, V) :- global_term_ref_1( V).

'chr via_2'( X, _, V) :- var(X), !, X=V.
'chr via_2'( _, Y, V) :- var(Y), !, Y=V.
'chr via_2'( T, _, V) :- compound(T), nonground( T, V), ! .
'chr via_2'( _, T, V) :- compound(T), nonground( T, V), ! .
'chr via_2'( _, _, V) :- global_term_ref_1( V).

%
% The second arg is a witness.
% The formulation with term_variables/2 is
% cycle safe, but it finds a list of all vars.
% We need only one, and no list in particular.
%
nonground( Term, V) :-
	term_variables( Term, Vs),
	Vs = [V|_].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'chr novel_production'( Self, Tuple) :-
	arg( 5, Self, Ref),
	Ref = mutable(History), % get_mutable( History, Ref), % XXX Inlined
	( get_assoc( Tuple, History, _) ->
	    fail
	;
	    true
	).

%
% Not folded with novel_production/2 because guard checking
% goes in between the two calls.
%
'chr extend_history'( Self, Tuple) :-
	arg( 5, Self, Ref),
	Ref = mutable(History), % get_mutable( History, Ref), % XXX Inlined
	put_assoc( Tuple, History, x, NewHistory),
	update_mutable( NewHistory, Ref).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
constraint_generation( Susp, State, Generation) :-
	arg( 2, Susp, Mref),
	Mref = mutable(State), % get_mutable( State, Mref), % XXX Inlined
	arg( 4, Susp, Gref),
	Gref = mutable(Generation). % get_mutable( Generation, Gref). 	% not incremented meanwhile % XXX Inlined

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'chr allocate_constraint'( Closure, Self, F, Args) :-
	empty_history( History),
	create_mutable( passive(Args), Mref),
	create_mutable( 0, Gref),
	create_mutable( History, Href),
	gen_id( Id),
	Self =.. [suspension,Id,Mref,Closure,Gref,Href,F|Args].

%
% 'chr activate_constraint'( -, +, -).
%
% The transition gc->active should be rare
%
'chr activate_constraint'( Vars, Susp, Generation) :-
	arg( 2, Susp, Mref),
	Mref = mutable(State), % get_mutable( State, Mref),  % XXX Inlined
	update_mutable( active, Mref),
	( nonvar(Generation) ->			% aih
	    true
	;
	    arg( 4, Susp, Gref),
	    Gref = mutable(Gen), % get_mutable( Gen, Gref), % XXX Inlined
	    Generation is Gen+1,
	    update_mutable( Generation, Gref)
	),
	( compound(State) ->			% passive/1
	    term_variables( State, Vs),
	    none_locked( Vs),
	    global_term_ref_1( Global),
	    Vars = [Global|Vs]
	; State==removed ->			% the price for eager removal ...
	    Susp =.. [_,_,_,_,_,_,_|Args],
	    term_variables( Args, Vs),
	    global_term_ref_1( Global),
	    Vars = [Global|Vs]
	;
	    Vars = []
	).

'chr insert_constraint_internal'( [Global|Vars], Self, Closure, F, Args) :-
	term_variables( Args, Vars),
	none_locked( Vars),
	global_term_ref_1( Global),
	empty_history( History),
	create_mutable( active, Mref),
	create_mutable( 0, Gref),
	create_mutable( History, Href),
	gen_id( Id),
	Self =.. [suspension,Id,Mref,Closure,Gref,Href,F|Args].

insert_constraint_internal( [Global|Vars], Self, Term, Closure, F, Args) :-
	term_variables( Term, Vars),
	none_locked( Vars),
	global_term_ref_1( Global),
	empty_history( History),
	create_mutable( active, Mref),
	create_mutable( 0, Gref),
	create_mutable( History, Href),
	gen_id( Id),
	Self =.. [suspension,Id,Mref,Closure,Gref,Href,F|Args].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
change_state( Susp, State) :-
	arg( 2, Susp, Mref),
	update_mutable( State, Mref).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
empty_history( E) :- empty_assoc( E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gen_id( Id) :-
	incval( id, Id).

incval(id,Id) :-
	nb_getval(id,Id),
	NextId is Id + 1,
	nb_setval(id,NextId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_mutable(V,mutable(V)).
 
'chr get_mutable'(V, mutable(V)).  
 
'chr update_mutable'(V,M) :-
	setarg(1,M,V).

get_mutable(V, mutable(V)).  

update_mutable(V,M) :-
	setarg(1,M,V).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'chr global_term_ref_1'(X) :-
	global_term_ref_1(X).

global_term_ref_1(X) :-
	nb_getval(chr_global,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'chr sbag_member'( Element, [Head|Tail]) :-
	sbag_member( Element, Tail, Head).

% auxiliary to avoid choicepoint for last element

sbag_member( E, _,	     E).
sbag_member( E, [Head|Tail], _) :-
	sbag_member( E, Tail, Head).

'chr sbag_del_element'( [],	  _,	[]).
'chr sbag_del_element'( [X|Xs], Elem, Set2) :-
	( X==Elem ->
	    Set2 = Xs
	;
	    Set2 = [X|Xss],
	    'chr sbag_del_element'( Xs, Elem, Xss)
	).

sbag_union( A, B, C) :-
	sbag_merge( A, B, C).

sbag_merge([],Ys,Ys).
sbag_merge([X | Xs],YL,R) :-
  ( YL = [Y | Ys] ->
      arg(1,X,XId),
      arg(1,Y,YId),	
       ( XId < YId ->
           R = [X | T],
           sbag_merge(Xs,YL,T)
       ; XId > YId ->
           R = [Y | T],
           sbag_merge([X|Xs],Ys,T)
       ;
           R = [X | T],
           sbag_merge(Xs,Ys,T)
       )    
  ;
       R = [X | Xs]
  ).


