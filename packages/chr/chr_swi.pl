/*  $Id$

    Part of CHR (Constraint Handling Rules)

    Author:        Tom Schrijvers and Jan Wielemaker
    E-mail:        Tom.Schrijvers@cs.kuleuven.ac.be
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
*/

:- module(chr,
	  [ op( 700, xfx, ::),
	    op(1180, xfx, ==>),
	    op(1180, xfx, <=>),
	    op(1150, fx, constraints),
	    op(1150, fx, handler),
	    op(1150, fx, rules),
	    op(1100, xfx, \),
	    op(1200, xfx, @),
	    op(1190, xfx, pragma),
	    op( 500, yfx, #),

	    chr_show_store/1,		% +Module
	    chr_trace/0,
	    chr_notrace/0
	  ]).

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

user:file_search_path(chr, library(chr)).

:- use_module(chr(chr_translate)).
:- use_module(chr(chr_runtime)).
:- use_module(chr(chr_debug)).
:- use_module(library(gensym)).

:- dynamic
	chr_term/2.			% File, Term

%	chr_expandable(+Term)
%	
%	Succeeds if Term is a  rule  that   must  be  handled by the CHR
%	compiler. Ideally CHR definitions should be between
%
%		:- constraints ...
%		...
%		:- end_constraints.
%
%	As they are not we have to   use  some heuristics. We assume any
%	file is a CHR after we've seen :- constraints ... or if the file
%	is named *.chr

chr_expandable((:- constraints _)).
chr_expandable((handler _)) :-
	is_chr_file.
chr_expandable((rules _)) :-
	is_chr_file.
chr_expandable((_ <=> _)) :-
	is_chr_file.
chr_expandable((_ @ _)) :-
	is_chr_file.
chr_expandable((_ ==> _)) :-
	is_chr_file.
chr_expandable(option(_, _)) :-
	is_chr_file.

is_chr_file :-
	source_location(File, _Line),
	(   chr_term(File, _)
	->  true
	;   file_name_extension(_, chr, File)
	).

%	chr_expand(+Term, -Expansion)
%	
%	Extract CHR declarations and rules from the file and run the
%	CHR compiler when reaching end-of-file.

chr_expand(Term, []) :-
	chr_expandable(Term), !,
	source_location(File, _Line),
	assert(chr_term(File, Term)).
chr_expand(end_of_file,
	   [ (:- use_module(chr(chr_runtime))),
	     (:- style_check(-discontiguous)) % no need to restore; file ends
	   | Program
	   ]) :-
	is_chr_file,
	source_location(File, _Line),
	findall(T, retract(chr_term(File, T)), CHR0),
	CHR0 \== [],
%	length(CHR0, NDecls),
%	format('Translating ~w declarations~n', [NDecls]),
	prolog_load_context(module, Module),
	(   Module == user
	->  (	memberchk(handler(Handler), CHR0)
	    ->	true
	    ;	gensym(chr_handler, Handler)
	    )
	;   Handler = Module
	),
	add_debug_decl(CHR0, CHR1),
	add_optimise_decl(CHR1, CHR),
	call_chr_translate(File,
			   [ (:- module(Handler, []))
			   | CHR
			   ],
			   Program0),
	delete_header(Program0, Program).


delete_header([(:- module(_,_))|T0], T) :- !,
	delete_header(T0, T).
delete_header(L, L).

add_debug_decl(CHR, CHR) :-
	memberchk(option(debug, _), CHR), !.
add_debug_decl(CHR, [option(debug, Debug)|CHR]) :-
	(   current_prolog_flag(generate_debug_info, true)
	->  Debug = on
	;   Debug = off
	).

add_optimise_decl(CHR, CHR) :-
	memberchk(option(optimize, _), CHR), !.
add_optimise_decl(CHR, [option(optimize, full)|CHR]) :-
	current_prolog_flag(optimize, true), !.
add_optimise_decl(CHR, CHR).


%	call_chr_translate(+File, +In, -Out)
%	
%	There  are  two  reasons  for  this   predicate.  First  of  all
%	chr_translate/2 leaves the CHR constraint store initialised, and
%	we  need  to  backtrack  to  restore  it  with  the  unfortunate
%	consequence that we have to copy the  result once more. Can this
%	be fixed in chr_translate/2? Should we use nb_setval/2?
%
%	Second the entire translation may  fail,   in  which we'd better
%	issue  a  warning  rather   than    simply   ignoring   the  CHR
%	declarations.

call_chr_translate(File, In, Out) :-
	chr_translate(In, Out),
	assert(chr_term(File, Out)),
	fail.
call_chr_translate(File, _, Out) :-
	retract(chr_term(File, Out)), !.
call_chr_translate(File, _, []) :-
	print_message(error, chr(compilation_failed(File))).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(chr(compilation_failed(From))) -->
	[ 'CHR: Failed to compile ~w'-[From] ].


		 /*******************************
		 *	   MUST BE LAST!	*
		 *******************************/

:- multifile user:term_expansion/2.
:- dynamic   user:term_expansion/2.

user:term_expansion(In, Out) :-
	chr_expand(In, Out).
	

