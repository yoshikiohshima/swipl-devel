/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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

:- module(emacs_chr_mode, []).
:- use_module(library(pce)).
:- use_module(prolog_mode).
:- use_module(pce_boot(pce_operator)).	% push/pop operators


		 /*******************************
		 *	      CHR MODE		*
		 *******************************/

:- emacs_begin_mode(chr, prolog,
		    "Mode for editing Constraint Handling Rules (CHR) documents",
		    % BINDINGS
		    [
		    ],
		    % SYNTAX TABLE
		    [
		    ]).

:- emacs_end_mode.


		 /*******************************
		 *	   SYNTAX HOOKS		*
		 *******************************/

:- multifile
	emacs_prolog_mode:alternate_syntax/3.


emacs_prolog_mode:alternate_syntax(chr,
				   emacs_chr_mode:push_chr_operators,
				   emacs_chr_mode:pop_chr_operators).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note that we could generalise this to deal with all included files.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
	chr_operators/1.

push_chr_operators :-
	(   chr_operators(Ops)
	->  true
	;   init_chr_operators(Ops),
	    assert(chr_operators(Ops))
	),
	push_operators(Ops).

pop_chr_operators :-
	pop_operators.

init_chr_operators(Ops) :-
	absolute_file_name(library('chr/chr_op'),
			   [ file_type(prolog),
			     access(read),
			     file_errors(fail)
			   ],
			   OpFile),
	open(OpFile, read, In),
	read(In, Term),
	read_ops(Term, In, Ops),
	close(In).

read_ops(end_of_file, _, []) :- !.
read_ops((:- op(Pre, Ass, Ops)), In, [ op(Pre, Ass, Ops) |T]) :-
	read(In, T2),
	read_ops(T2, In, T).
read_ops(_, In, Ops) :-
	read(In, T2),
	read_ops(T2, In, Ops).
