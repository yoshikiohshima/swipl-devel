/*  $Id$

    Part of SWI-Prolog

    Author:        Tom Schrijvers
    E-mail:        tom.schrijvers@cs.kuleuven.ac.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2004, K.U.Leuven

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
%
% Simple integer solver that keeps track of upper and lower bounds
%
% Author: 	Tom Schrijvers
% E-mail: 	tom.schrijvers@cs.kuleuven.ac.be
% Copyright:	2004, K.U.Leuven
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Todo:
%	- reduce redundant propagation work
%	- other labelling functions
%	- abs, mod, ...
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(bounds,
	[
		(#>)/2,
		(#<)/2,
		(#>=)/2,
		(#=<)/2,
		(#=)/2,
		(#\=)/2,
		(#<=>)/2,
		(#=>)/2,
		(#<=)/2,
		(#/\)/2,
		(in)/2,
		label/1,
		all_different/1
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% operator declarations

:- op(760, yfx,user:(#<=>)).
:- op(750, xfy,user:(#=>)).
:- op(750, yfx,user:(#<=)).
:- op(720, yfx, user:(#/\)).
:- op(700,xfx,user:(#>)).
:- op(700,xfx,user:(#<)).
:- op(700,xfx,user:(#>=)).
:- op(700,xfx,user:(#=<)).
:- op(700,xfx,user:(#=)).
:- op(700,xfx,user:(#\=)).
:- op(700,xfx,user:(in)).
:- op(550,xfx,user:(..)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% exported predicates
X #>= Y :-
	parse_expression(X,RX),
	parse_expression(Y,RY), 
	geq(RX,RY,yes).
X #=< Y :- 
	parse_expression(X,RX),
	parse_expression(Y,RY),
	leq(RX,RY,yes).
X #= Y :-
	parse_expression(X,RX),
	parse_expression(Y,RX). 
X #\= Y :-
	parse_expression(X,RX),
	parse_expression(Y,RY), 
	neq(RX,RY,yes).
X #> Y :-
	Z #= Y + 1,
	X #>= Z.
X #< Y :-
	Y #> X.
X in L .. U :- 
	( is_list(X) ->
		domains(X,L,U)
	;
		domain(X,L,U)
	).

L #<=> R :-
	reify(L,B),
	reify(R,B).
L #=> R :-
	reify(L,BL),
	reify(R,BR),
	myimpl(BL,BR).
R #<= L :-
	reify(L,BL),
	reify(R,BR),
	myimpl(BL,BR).
L #/\ R :-
	call(L),
	call(R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reify(B,R) :-
	var(B), !,
	R = B.
reify(B,R) :-
	number(B), !,
	R = B.
reify(X #>= Y,B) :-
	parse_expression(X,XR),
	parse_expression(Y,YR),
	reified_geq(XR,YR,B).
reify(X #> Y,B) :-
	parse_expression(X,XR),
	Z #= Y + 1,
	reified_geq(XR,Z,B).
reify(X #=< Y,B) :-
	parse_expression(X,XR),
	parse_expression(Y,YR),
	reified_geq(YR,XR,B).
reify(X #< Y,B) :-
	reify(Y #> X,B).
reify(X #= Y,B) :-
	parse_expression(X,XR),
	parse_expression(Y,YR),
	reified_eq(XR,YR,B).
reify(X #\= Y,B) :-
	parse_expression(X,XR),
	parse_expression(Y,YR),
	reified_neq(XR,YR,B).
reify((X #/\ Y),B) :-
	reify(X,BX),
	reify(Y,BY),
	myand(BX,BY,B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_expression(Expr,Result) :-
	( var(Expr) ->
		Result = Expr
	; number(Expr) ->
		Result = Expr
	; Expr = (L + R) ->
		parse_expression(L,RL),
		parse_expression(R,RR),
		myplus(RL,RR,Result,yes)
	; Expr = (L * R) ->
		parse_expression(L,RL),
		parse_expression(R,RR),
		mytimes(RL,RR,Result,yes)
	; Expr = (L - R) ->
		parse_expression(L,RL),
		parse_expression(R,RR),
		mytimes(-1,RR,RRR,yes),
		myplus(RL,RRR,Result,yes)
	; Expr = (- E) ->
		parse_expression(E,RE),
		mytimes(-1,RE,Result,yes)
	; Expr = max(L,R) ->
		parse_expression(L,RL),
		parse_expression(R,RR),
		mymax(L,R,Result,yes)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
label([]).
label([V|Vs]) :-
	( get(V,L,U,_) -> 
		between(L,U,W),
		%format('\tlabelling ~w with ~w\n',[V,W]),
		V = W
	;
		true
	),
	label(Vs).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all_different([]).
all_different([X|Xs]) :-
	different(Xs,X),
	all_different(Xs).

different([],_).
different([Y|Ys],X) :-
	neq(X,Y,yes),
	different(Ys,X).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
domain(X,L,U) :-
	( var(X) ->
		get(X,XL,XU,Exp),
		NL is max(L,XL),
		NU is min(U,XU),
		put(X,NL,NU,Exp)
	; % nonvar(X) ->
		X >= L,
		X =< U
	).

domains([],_,_).
domains([V|Vs],L,U) :-
	domain(V,L,U),
	domains(Vs,L,U).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
leq(X,Y,New) :-
	geq(Y,X,New).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
geq(X,Y,New) :-
	( X == Y ->
		true
	; nonvar(X) ->
		( nonvar(Y) ->
			X >= Y
		; 
			get(Y,YL,YU,Exp) ->
			NYU is min(X,YU),
			put(Y,YL,NYU,Exp)
		)
	; nonvar(Y) ->
		get(X,XL,XU,Exp) ->
		NXL is max(Y,XL),
		put(X,NXL,XU,Exp)
	;
		get(X,XL,XU,ExpX),
		get(Y,YL,YU,ExpY),
		XU >= YL,
		( XL > YU ->
			true
		; XU == YL ->
			X = Y
		; member(leq(Z,State),ExpX),
	          Y == Z -> 
			set_passive(State),
			X = Y	
		; 
			( New == yes ->
				active_state(State),
				put(Y,YL,YU,[leq(X,State)|ExpY]),
				ExpX1 = [geq(Y,State)|ExpX]
			;
				ExpX1 = ExpX
			),
			NXL is max(XL,YL),
			put(X,NXL,XU,ExpX1),
			( get(Y,YL2,YU2,ExpY2) ->
				NYU is min(YU2,XU),
				put(Y,YL2,NYU,ExpY2)
			;
				true
			)
		)
	).
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
neq(X,Y,New) :-
	X \== Y,
	( nonvar(X) ->
		( nonvar(Y) ->
			true
		;
			get(Y,L,U,Exp),
			( L == X ->
				NL is L + 1,
				put(Y,NL,U,Exp)
			; U == X ->
				NU is U - 1,
				put(Y,L,NU,Exp)
			;
				( New == yes ->
					active_state(State),
					put(Y,L,U,[neq(X,State)|Exp])
				;
					true
				)	
			)
		)
	; nonvar(Y) ->
		neq(Y,X,New)
	;
		get(X,XL,XU,XExp),
		get(Y,YL,YU,YExp),
		( XL > YU ->
			true
		; YL > XU ->
			true
		;
			( New == yes ->
				active_state(State),
				put(X,XL,XU,[neq(Y,State)|XExp]),
				put(Y,YL,YU,[neq(X,State)|YExp])
			;
				true
			)
		)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
myplus(X,Y,Z,New) :-
	( nonvar(X) ->
		( nonvar(Y) ->
			Z is X + Y
		; nonvar(Z) ->
			Y is Z - X
		;
			get(Z,ZL,ZU,ZExp),
			get(Y,YL,YU,YExp),
			( New == yes ->
				ZExp1 = [myplus2(Y,X)|ZExp],
				YExp1 = [myplus(X,Z)|YExp],
				put(Y,YL,YU,YExp1)
			;
				ZExp1 = ZExp
			),
			NZL is max(ZL,X+YL),
			NZU is min(ZU,X+YU),
			put(Z,NZL,NZU,ZExp1),
			( get(Y,YL2,YU2,YExp2) ->
				NYL is max(YL,NZL-X),
				NYU is min(YU,NZU-X),	
				put(Y,NYL,NYU,YExp2)
			;
				Z is X + Y
			)
		)
	; nonvar(Y) ->
		myplus(Y,X,Z,New)
	; nonvar(Z) ->
		get(X,XL,XU,XExp),
		get(Y,YL,YU,YExp),
		( New == yes ->
			XExp1 = [myplus(Y,Z)|XExp],
			YExp1 = [myplus(X,Z)|YExp],
			put(Y,YL,YU,YExp1)
		;
			XExp1 = XExp
		),
		NXL is max(XL,Z-YU),
		NXU is min(XU,Z-YL),
		put(X,NXL,NXU,XExp1),
		( get(Y,YL2,YU2,YExp2) ->
			NYL is max(YL2,Z-NXU),
			NYU is min(YU2,Z-NXL),
			put(Y,NYL,NYU,YExp2)
		;
			X is Z - Y
		)
	;
		get(X,XL,XU,XExp),
		get(Y,YL,YU,YExp),
		get(Z,ZL,ZU,ZExp),
		( New == yes ->
			XExp1 = [myplus(Y,Z)|XExp],
			YExp1 = [myplus(X,Z)|YExp],
			ZExp1 = [myplus2(X,Y)|ZExp],
			put(Y,YL,YU,YExp1),
			put(Z,ZL,ZU,ZExp1)
		;
			XExp1 = XExp
		),
		NXL is max(XL,ZL-YU),
		NXU is min(XU,ZU-YL),
		put(X,NXL,NXU,XExp1),
		( get(Y,YL2,YU2,YExp2) ->
			NYL is max(YL2,ZL-NXU),
			NYU is min(YU2,ZU-NXL),
			put(Y,NYL,NYU,YExp2)
		;
			NYL = Y,
			NYU = Y
		),
		( get(Z,ZL2,ZU2,ZExp2) ->
			NZL is max(ZL2,NXL+NYL),
			NZU is min(ZU2,NXU+NYU),
			put(Z,NZL,NZU,ZExp2)
		;
			true
		)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% X * Y = Z

:- arithmetic_function(div/2).
div(X,Y,Z) :-
	( max_inf(X) ->
		( Y >= 0 ->
			Z = X
		;
			min_inf(Z)
		)
	; min_inf(X) ->
		( Y >= 0 ->
			Z = X
		;
			max_inf(Z)
		)
	; Y \== 0 ->
		Z is X / Y
	; X >= 0 ->
		max_inf(Z)
	; X < 0 ->
		min_inf(Z)
	).	

mytimes(X,Y,Z,New) :-
	( nonvar(X) ->
		( nonvar(Y) ->
			Z is X * Y
		; nonvar(Z) ->
			( X \== 0 -> 
				0 is Z mod X,
				Y is Z / X
			;
				true
			)		
		;
			get(Y,YL,YU,YExp),
			get(Z,ZL,ZU,ZExp),
			NZL is max(ZL,min(X * YL,X*YU)),
			NZU is min(ZU,max(X * YU,X*YL)),
			( New == yes ->
				YExp1 = [mytimes(X,Z)|YExp],
				put(Y,YL,YU,YExp1),
				put(Z,NZL,NZU,[mytimes2(X,Y)|ZExp])
			;
				put(Z,NZL,NZU,ZExp)
			),
			( get(Y,YL2,YU2,YExp2) ->
				min_divide(ZL,ZU,X,X,NYLT),
				NYL is max(YL2,ceiling(NYLT)),
				NYU is min(YU2,floor(max(div(ZU,X),div(ZL,X)))),
				put(Y,NYL,NYU,YExp2)
			;
				Z is X * Y
			)		
		)
	; nonvar(Y) ->
		mytimes(Y,X,Z,New)
	; nonvar(Z) ->
		get(X,XL,XU,XExp),
		get(Y,YL,YU,YExp),
		min_divide(Z,Z,YL,YU,TNXL),
		max_divide(Z,Z,YL,YU,TNXU),
		NXL is max(XL,ceiling(TNXL)),
		NXU is min(XU,floor(TNXU)),		
		( New == yes ->
			YExp1 = [mytimes(X,Z)|YExp],
			put(Y,YL,YU,YExp1),
			put(X,NXL,NXU,[mytimes(Y,Z)|XExp])
		;
			put(X,NXL,NXU,XExp)
		),
		( get(Y,YL2,YU2,YExp2) ->
			min_divide(Z,Z,NXL,NXU,NYLT),
			NYL is max(YL2,ceiling(NYLT)),
			NYU is min(YU2,floor(max(div(Z,NXL),div(Z,NXU)))),
			put(Y,NYL,NYU,YExp2)
		;
			( Y \== 0 ->
				0 is Z mod Y,
				X is Z / Y
			;
				true
			)
		)
	;
		get(X,XL,XU,XExp),
		get(Y,YL,YU,YExp),
		get(Z,ZL,ZU,ZExp),
		min_divide(ZL,ZU,YL,YU,TXL),
		NXL is max(XL,ceiling(TXL)),
		max_divide(ZL,ZU,YL,YU,TXU),
		NXU is min(XU,floor(TXU)),
		( New == yes ->
			put(Y,YL,YU,[mytimes(X,Z)|YExp]),
			put(Z,ZL,ZU,[mytimes2(X,Y)|ZExp]),
			put(X,NXL,NXU,[mytimes(Y,Z)|XExp])
		;
			put(X,NXL,NXU,XExp)
		),
		( get(Y,YL2,YU2,YExp2) ->
			min_divide(ZL,ZU,XL,XU,TYL),
			NYL is max(YL2,ceiling(TYL)),
			max_divide(ZL,ZU,XL,XU,TYU),
			NYU is min(YU2,floor(TYU)),
			put(Y,NYL,NYU,YExp2)	
		;
			NYL = Y,
			NYU = Y
		),
		( get(Z,ZL2,ZU2,ZExp2) ->
			min_times(NXL,NXU,NYL,NYU,TZL),	
			NZL is max(ZL2,TZL),
			max_times(NXL,NXU,NYL,NYU,TZU),	
			NZU is min(ZU2,TZU),
			put(Z,NZL,NZU,ZExp2)
		;
			true
		)	
	).

max_times(L1,U1,L2,U2,Max) :-
	Max is max(max(L1*L2,L1*U2),max(U1*L2,U1*U2)).	
min_times(L1,U1,L2,U2,Min) :-
	Min is min(min(L1*L2,L1*U2),min(U1*L2,U1*U2)).	
max_divide(L1,U1,L2,U2,Max) :-
	( L2 =< 0 , U2 >= 0 ->
		max_inf(Max)
	;
		Max is max(max(div(L1,L2),div(L1,U2)),max(div(U1,L2),div(U1,U2)))
	).	
min_divide(L1,U1,L2,U2,Min) :-
	( L2 =< 0 , U2 >= 0 ->
		min_inf(Min)
	;	
		Min is min(min(div(L1,L2),div(L1,U2)),min(div(U1,L2),div(U1,U2)))
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mymax(X,Y,Z,New) :-
	( nonvar(X) ->
		( nonvar(Y) ->
			Z is max(X,Y)
		; nonvar(Z) ->
			( Z == X ->		
				geq(X,Y,yes)
			; Z > X ->
				Y = Z
			%; Z < X
			%	fail
			)
		;
			get(Y,YL,YU,YExp),
			get(Z,ZL,ZU,ZExp),
			( YL >= X ->
				Z = Y
			; X >= YU ->
				Z = X
			; X < ZL ->
				Y = Z
			; YU < ZL ->
				X = Z
			;
				( New == yes -> 
					put(Y,YL,YU,[mymax(X,Z)|YExp]) 
				; 
					true
				),
				NZL is max(ZL,X),
				NZU is min(ZU,YU),
				( New == yes ->
					put(Z,NZL,NZU,[mymax2(X,Y)|ZExp])
				;
					put(Z,NZL,NZU,ZExp)
				),
				( get(Y,YL2,YU2,YExp2) ->
					NYU is min(YU2,NZU),
					put(Y,YL2,NYU,YExp2)
				;
					true
				)
			)
		)
	; nonvar(Y) ->
		mymax(Y,X,Z,New)
	; nonvar(Z) ->
		get(X,XL,XU,XExp),
		get(Y,YL,YU,YExp),
		( XU < Z ->
			Y = Z
		; % XU >= Z ->
		  YU < Z ->
		  	X = Z
		; % YU >= Z
		  XL > YU ->
			Z = X
		; YL > XU ->
			Z = Y
		;
			( New == yes ->
				put(Y,YL,YU,[mymax(X,Z)|YExp]),
				put(X,XL,Z,[mymax(Y,Z)|XExp])
			;
				put(X,XL,Z,XExp)
			),
			( get(Y,YL2,YU2,YExp2) ->
				NYU is min(YU2,Z),
				put(Y,YL2,NYU,YExp2)
			;
				true
			)
		)
	; X == Y ->
		Z = X
	; Z == X ->
		geq(X,Y,yes)
	; Z == Y ->
		geq(Y,X,yes)
	;
		get(X,XL,XU,XExp),
		get(Y,YL,YU,YExp),
		get(Z,ZL,ZU,ZExp),
		NZL is max(ZL,max(XL,YL)),
		NZU is min(ZU,max(XU,YU)),
		( New == yes ->
			put(X,XL,XU,[mymax(Y,Z)|XExp]),
			put(Y,YL,YU,[mymax(X,Z)|YExp]),
			put(Z,NZL,NZU,[mymax2(X,Y)|ZExp])
		;
			put(Z,NZL,NZU,ZExp)
		),
		( get(X,XL2,XU2,XExp2) ->
			( XU2 < NZL ->
				Y = Z
			; YU  < NZL ->
				X = Z
			; YU < XL2 ->
				X = Z
			; XU2 < YL ->
				Y = Z
			;
				NXU is min(XU2,NZU),
				put(X,XL2,NXU,XExp2),
				( get(Y,YL2,YU2,YExp2) ->
					NYU is min(YU2,NZU),
					put(Y,YL2,NYU,YExp2)
				;
					mymax(Y,X,Z,no)
				)
			)
		; 
			mymax(X,Y,Z,no)
		)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO
%	trigger reified constraints when geq is added
reified_geq(X,Y,B) :-
	( var(B) ->
		( nonvar(X) ->
			( nonvar(Y) ->
				( X >= Y ->
					B = 1
				;
					B = 0
				)
			;
				get(Y,L,U,Expr),
				( X >= U ->
					B = 1
				; X < L ->
					B = 0
				;
					put(Y,L,U,[reified_leq(X,B)|Expr]),
					get(B,BL,BU,BExpr),
					put(B,BL,BU,[reified_geq2(X,Y)|BExpr]),
					B in 0..1
				)
			)
		; nonvar(Y) ->
			get(X,L,U,Expr),
			( L >= Y ->
				B = 1
			; U < Y ->
				B = 0
			;
				put(X,L,U,[reified_geq(Y,B)|Expr]),
				get(B,BL,BU,BExpr),
				put(B,BL,BU,[reified_geq2(X,Y)|BExpr]),
				B in 0..1
			)
		;
			get(X,XL,XU,XExpr),
			get(Y,YL,YU,YExpr),
			( XL >= YU ->
				B = 1
			; XU < YL ->
				B = 0
			; member(geq(Z,_State),XExpr),
			  Z == Y ->
			  	B = 1
			;
				put(X,XL,XU,[reified_geq(Y,B)|XExpr]),
				put(Y,YL,YU,[reified_leq(X,B)|YExpr]),
				get(B,BL,BU,BExpr),
				put(B,BL,BU,[reified_geq2(X,Y)|BExpr]),
				B in 0..1
			)
		)
	; B == 1 ->
		X #>= Y	
	; B == 0 ->
		X #< Y
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reified_eq(X,Y,B) :-
	( var(B) ->
		( nonvar(X) ->
			( nonvar(Y) ->
				( X == Y ->
					B = 1
				;
					B = 0
				)
			;
				get(Y,L,U,Expr),
				( L > X ->
					B = 0
				; U < X ->
					B = 0
				;
					put(Y,L,U,[reified_eq(X,B)|Expr]),
					get(B,BL,BU,BExpr),
					put(B,BL,BU,[reified_eq2(X,Y)|BExpr]),
					B in 0..1
				)
			)
		; nonvar(Y) ->
			reified_eq(Y,X,B)
		; X == Y ->
			B = 1
		;
			get(X,XL,XU,XExpr),
			get(Y,YL,YU,YExpr),
			( XL > YU ->
				B = 0
			; YL > XU ->
				B = 0
			; member(neq(Z,_),XExpr),
			  Z == Y ->
			  	B = 0
			;
				put(X,XL,XU,[reified_eq(Y,B)|XExpr]),
				put(Y,YL,YU,[reified_eq(X,B)|YExpr]),
				get(B,BL,BU,BExpr),
				put(B,BL,BU,[reified_eq2(X,Y)|BExpr]),
				B in 0..1
			)
		)
	; B == 1 ->
		X #= Y
	; B == 0 ->
		X #\= Y
	).
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reified_neq(X,Y,B) :-
	mynot(B,B1),
	reified_eq(X,Y,B1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mynot(B1,B2) :-
	( nonvar(B1) ->
		( B1 == 1 ->
			B2 = 0
		; B1 == 0 ->
			B2 = 1
		)
	; nonvar(B2) ->
		mynot(B2,B1)
	;
		get(B1,L1,U1,Expr1),
		get(B2,L2,U2,Expr2),
		put(B2,L2,U2,[mynot(B1)|Expr2]),
		put(B1,L1,U1,[mynot(B2)|Expr1]),
		B1 in 0..1,
		B2 in 0..1
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
myimpl(B1,B2) :-
	( nonvar(B1) ->
		( B1 == 1 ->
			B2 = 1
		; B1 == 0 ->
			B2 in 0..1
		)
	; nonvar(B2) ->
		( B2 == 0 ->
			B1 = 0
		; B2 == 1 ->
			B1 in 0..1
		)
	;
		get(B1,L1,U1,Expr1),
		get(B2,L2,U2,Expr2),
		put(B1,L1,U1,[myimpl(B2)|Expr1]),
		put(B2,L2,U2,[myimpl2(B1)|Expr2]),
		B1 in 0..1,
		B2 in 0..1
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
myand(X,Y,Z) :-
	( nonvar(X) ->
		( X == 1 ->
			Y in 0..1,
			Y = Z
		; X == 0 ->
			Y in 0..1,
			Z = 0
		)
	; nonvar(Y) ->
		myand(Y,X,Z)
	; nonvar(Z) ->
		( Z == 1 ->
			X = 1,
			Y = 1
		; Z == 0 ->
			X in 0..1,
			Y in 0..1,
			X + Y #=< 1
		)
	;
		get(X,LX,UX,ExpX),
		get(Y,LY,UY,ExpY),
		get(Z,LZ,UZ,ExpZ),
		put(X,LX,UX,[myand(Y,Z)|ExpX]),
		put(Y,LY,UY,[myand(X,Z)|ExpY]),
		put(Z,LZ,UZ,[myand2(X,Y)|ExpZ]),
		[X,Y,Z] in 0..1
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get(X,L,U,Exp) :-
	( get_attr(X,bounds,Attr) ->
		Attr = bounds(L,U,Exp)
	; var(X) ->
		min_inf(L),
		max_inf(U),
		Exp = []
	).

put(X,L,U,Exp) :-
	L =< U,
	( L == U ->
		X = L,
		trigger_exps(Exp,X) 
	;
		( get_attr(X,bounds,Attr) ->
			put_attr(X,bounds,bounds(L,U,Exp)),
			Attr = bounds(OldL,OldU,_),
			( OldL == L, OldU == U ->
				true
			;
				%format('\t~w in ~w .. ~w\n',[X,L,U]),
				trigger_exps(Exp,X)
			)
		; 
			%format('\t~w in ~w .. ~w\n',[X,L,U]),
			put_attr(X,bounds,bounds(L,U,Exp))
		)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
min_inf(Inf) :-
	current_prolog_flag(min_integer,MInf),
	Inf is MInf + 1.

max_inf(Inf) :-
	current_prolog_flag(max_integer,Inf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
attr_unify_hook(bounds(L,U,Exp),Other) :-
	( get(Other,OL,OU,OExp) ->
		NL is max(L,OL),
		NU is min(U,OU),
		append(Exp,OExp,NExp),
		check_neqs(NExp,Other),
		put(Other,NL,NU,NExp)	
	; % nonvar(Other) ->
		Other >= L,
		Other =< U,
		trigger_exps(Exp,Other)
	).

check_neqs([],_).
check_neqs([E|Es],X) :-
	( E = neq(Y,_),
	  X == Y ->
		fail
	;
		check_neqs(Es,X)
	).
		

trigger_exps([],_).
trigger_exps([E|Es],X) :-
	trigger_exp(E,X),
	trigger_exps(Es,X).

trigger_exp(geq(Y,State),X) :-
	( is_active(State) ->
		geq(X,Y,no)
	;
		true
	).
trigger_exp(leq(Y,State),X) :-
	( is_active(State) ->
		leq(X,Y,no)
	;
		true
	).
trigger_exp(neq(Y,State),X) :-
	( is_active(State) ->
		neq(X,Y,no)
	;
		true
	).
trigger_exp(myplus(Y,Z),X) :-
	myplus(X,Y,Z,no).
trigger_exp(myplus2(A,B),X) :-
	myplus(A,B,X,no).

trigger_exp(mytimes(Y,Z),X) :-
	mytimes(X,Y,Z,no).
trigger_exp(mytimes2(A,B),X) :-
	mytimes(A,B,X,no).

trigger_exp(mymax(Y,Z),X) :-
	mymax(X,Y,Z,no).
trigger_exp(mymax2(X,Y),Z) :-
	mymax(X,Y,Z,no).

trigger_exp(reified_leq(X,B),Y) :-
	reified_geq(X,Y,B).
trigger_exp(reified_geq(Y,B),X) :-
	reified_geq(X,Y,B).
trigger_exp(reified_geq2(X,Y),B) :-
	reified_geq(X,Y,B).

trigger_exp(reified_eq(Y,B),X) :-
	reified_eq(X,Y,B).
trigger_exp(reified_eq2(X,Y),B) :-
	reified_eq(X,Y,B).

trigger_exp(mynot(Y),X) :-
	mynot(X,Y).

trigger_exp(myimpl(Y),X) :-
	myimpl(X,Y).
trigger_exp(myimpl2(X),Y) :-
	myimpl(X,Y).

trigger_exp(myand(Y,Z),X) :-
	myand(X,Y,Z).
trigger_exp(myand2(X,Y),Z) :-
	myand(X,Y,Z).

memberchk_eq(X,[Y|Ys],Z) :-
   (   X == Y ->
       Z = Y
   ;   memberchk_eq(X,Ys,Z)
   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
active_state(state(active)).
is_active(state(active)).
set_passive(State) :-
	setarg(1,State,passive).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
attr_portray_hook(bounds(L,U,_),_) :-
	write(L..U).

