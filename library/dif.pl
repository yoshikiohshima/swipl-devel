/*  $Id$

    Part of SWI-Prolog

    Author:        Tom Schrijvers, K.U.Leuven
    E-mail:        Tom.Schrijvers@cs.kuleuven.ac.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2003-2004, K.U.Leuven

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This module implements the dif/2 constraint. It constraint two terms to be
% not identical.
%
%	Author: 	Tom Schrijvers, K.U.Leuven
% 	E-mail: 	Tom.Schrijvers@cs.kuleuven.ac.be
%	Copyright:	2003-2004, K.U.Leuven
%
% Update 7/3/2004:
%   Now uses unifyable/3. It enables dif/2 to work with infinite terms.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(dif,[dif/2]).

%:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dif(X,Y) :-
	( compound(X), compound(Y) ->
		dif_c_c(X,Y,-) 
	% not terms, so comparing should be relatively cheap
	; X == Y ->
		fail
	; nonvar(X), nonvar(Y) ->
		true
	;
		% one or both are variables
		( var(X) ->
			( var(Y) ->
				dif_var_var(X,Y)
			;
				dif_var_nonvar(X,Y)
			)
		; var(Y) ->
			dif_var_nonvar(Y,X)
		)
	).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% types of attributes?
% 	vardif: X is a variable
%	node(Parent,Children,Variables,Counter)

dif_c_c(X,Y,Parent) :-
	( unifyable(X,Y,Unifier) ->
		( Unifier == [] ->
			or_one_fail(Parent)
		;
			dif_c_c_l(Unifier,Parent)
		)
	;
		or_succeed(Parent)
	).

dif_c_c_l(Unifier,Parent) :-
	length(Unifier,N),
	put_attr(OrNode,dif,node(Parent,[],[],N)),
	register_child(Parent,OrNode),
	dif_c_c_l_aux(Unifier,OrNode).	

register_child(Parent,Child) :-
	( var(Parent) ->
		get_attr(Parent,dif,Attr),
		Attr = node(P,Children,Vs,C),
		put_attr(Parent,dif,node(P,[Child|Children],Vs,C))
	;
		true
	).


dif_c_c_l_aux([],_).
dif_c_c_l_aux([X=Y|Unifier],OrNode) :-
	dif2(X,Y,OrNode),
	( var(OrNode) ->
		dif_c_c_l_aux(Unifier,OrNode)
	;
		true
	).


dif_var_nonvar(X,Y) :-
	( get_attr(X,dif,Attr) ->
		Attr = vardif(VL,NVL,OVL,ONVL),
		put_attr(X,dif,vardif(VL,[Y|NVL],OVL,ONVL))
	;
		put_attr(X,dif,vardif([],[Y],[],[]))
	).

dif_var_var(X,Y) :-
	dif_var_var_(X,Y),
	dif_var_var_(Y,X).

dif_var_var_(X,Y) :-
	( get_attr(X,dif,Attr) ->
		Attr = vardif(VL,NVL,OVL,ONVL),
		put_attr(X,dif,vardif([Y|VL],NVL,OVL,ONVL))
	;
		put_attr(X,dif,vardif([Y],[],[],[]))
	).

attr_unify_hook(vardif(VL,NVL,OVL,ONVL),Other) :-
	( var(Other) ->
		\+ memberchk_eq(Other,VL),
		( reverse_lookup(OVL,Other,OrNode) ->
 		  	or_one_fail(OrNode)
		;
			true
		),
		get_attr(Other,dif,OAttr),
		OAttr = vardif(VL2,NVL2,OVL2,ONVL2),
		append(VL,VL2,N_VL),
		append(NVL,NVL2,N_NVL),
		append(OVL,OVL2,N_OVL),
		append(ONVL,ONVL2,N_ONVL),
		put_attr(Other,dif,vardif(N_VL,N_NVL,N_OVL,N_ONVL))
	;
		var_bound(Other,VL,NVL,OVL,ONVL)
	).

% from hProlog lists.pl
memberchk_eq(X,[Y|Ys]) :-
   (   X == Y ->
       true
   ;   memberchk_eq(X,Ys)
   ).

reverse_lookup([X-Y|XYs],Value,Key) :-
	( Y == Value ->
		Key = X
	;
		reverse_lookup(XYs,Value,Key)
	).

var_bound(Other,VL,NVL,OVL,ONVL) :-
	var_bound_nvl(NVL,Other),
	var_bound_onvl(ONVL,Other),
	var_bound_vl(VL,Other),
	var_bound_ovl(OVL,Other).

var_bound_nvl([],_).
var_bound_nvl([Y|Ys],X) :-
	dif(X,Y),
	var_bound_nvl(Ys,X).

var_bound_onvl([],_).
var_bound_onvl([OrNode-Y|Ys],X) :-
	( var(OrNode) ->
		dif2(X,Y,OrNode)
	;
		true	
	),
	var_bound_onvl(Ys,X).

var_bound_vl([],_).
var_bound_vl([Y|Ys],X) :-
	( var(Y) ->
		get_attr(Y,dif,Attr),
		Attr = vardif(VL,NVL,OVL,ONVL),
		delete(VL,X,N_VL),
		put_attr(Y,dif,vardif(N_VL,[X|NVL],OVL,ONVL))
	;
		dif(X,Y)
	),
	var_bound_vl(Ys,X).

var_bound_ovl([],_).
var_bound_ovl([OrNode-Y|Ys],X) :-
	( var(OrNode) ->
		( var(Y) ->
			get_attr(Y,dif,Attr),
			Attr = vardif(VL,NVL,OVL,ONVL),
			delete(OVL,OrNode-X,N_OVL),
			put_attr(Y,dif,vardif(VL,NVL,N_OVL,[OrNode-X|ONVL]))
		;
			dif2(X,Y,OrNode)
		)
	;
		true
	),
	var_bound_ovl(Ys,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dif2(X,Y,OrNode) :-
	( compound(X), compound(Y) ->
		dif_c_c(X,Y,OrNode)
	% not terms, so comparing should be relatively cheap
	; X == Y ->
		or_one_fail(OrNode)
	; nonvar(X), nonvar(Y) ->
		or_succeed(OrNode)	
	;
		% one or both are variables
		( var(X) ->
			( var(Y) ->
				dif_var_var2(X,Y,OrNode)
			;
				dif_var_nonvar2(X,Y,OrNode)
			)
		; var(Y) ->
			dif_var_nonvar2(Y,X,OrNode)
		)
	).

or_succeed(OrNode) :-
	( var(OrNode) ->
		get_attr(OrNode,dif,Attr),
		Attr = node(Parent,Children,Variables,_Counter),
		del_attr(OrNode,dif),
		OrNode = (-),
		or_succeed(Parent),
		or_succeed_list(Children),
		del_or_dif(Variables)
	; OrNode == (-) ->
		true
	;
		writeln('ERROR: or_succeed/1: unexpectedly instantiated OrnOde')
	).

or_succeed_list([]).
or_succeed_list([Child|Children]) :-
	or_succeed(Child),
	or_succeed_list(Children).

or_one_fail(OrNode) :-
	( var(OrNode) ->
		get_attr(OrNode,dif,Attr),
		Attr = node(Parent,Children,Variables,Counter),
		NCounter is Counter - 1,
		( NCounter == 0 ->
			del_attr(OrNode,dif),
			OrNode = (-),
			or_one_fail(Parent)
		;
			put_attr(OrNode,dif,node(Parent,Children,Variables,NCounter))
		)
	;
		fail	
	).

del_or_dif([]).
del_or_dif([X|Xs]) :-
	( var(X) ->
		get_attr(X,dif,Attr),
		Attr = vardif(VL,NVL,OVL,ONVL),
		filter_dead_ors(OVL,N_OVL),
		filter_dead_ors(ONVL,N_ONVL),
		put_attr(X,dif,vardif(VL,NVL,N_OVL,N_ONVL))	
	;
		true
	),
	del_or_dif(Xs).


filter_dead_ors([],[]).
filter_dead_ors([Or-Y|Rest],List) :-
	( var(Or) ->
		List = [Or-Y|NRest]
	;
		List = NRest
	),
	filter_dead_ors(Rest,NRest).

dif_var_nonvar2(X,Y,OrNode) :-
	register_variable(X,OrNode),
	( get_attr(X,dif,Attr) ->
		Attr = vardif(VL,NVL,OVL,ONVL),
		put_attr(X,dif,vardif(VL,NVL,OVL,[OrNode-Y|ONVL]))
	;
		put_attr(X,dif,vardif([],[],[],[OrNode-Y]))
	).

register_variable(V,OrNode) :-
	get_attr(OrNode,dif,Attr),
	Attr = node(P,Cs,Vs,C),
	put_attr(OrNode,dif,node(P,Cs,[V|Vs],C)).

dif_var_var2(X,Y,OrNode) :-
	dif_var_var2_(X,Y,OrNode),
	dif_var_var2_(Y,X,OrNode).

dif_var_var2_(X,Y,OrNode) :-
	register_variable(X,OrNode),
	( get_attr(X,dif,Attr) ->
		Attr = vardif(VL,NVL,OVL,ONVL),
		
		put_attr(X,dif,vardif(VL,NVL,[OrNode-Y|OVL],ONVL))
	;
		put_attr(X,dif,vardif([],[],[OrNode-Y],[]))
	).
