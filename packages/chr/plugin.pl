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

:- module(chr_plugin,
	  [
	  ]).

:- multifile
	user:prolog_load_file/2,
	user:prolog_file_type/2.
:- dynamic
	user:prolog_load_file/2,
	user:prolog_file_type/2.

%	user:prolog_file_type(Extension, FileType).

user:prolog_file_type(chr, chr).


user:prolog_load_file(Spec, Options) :-
	'$strip_module'(Spec, Module, File),
	absolute_file_name(File,
			   [ access(read),
			     file_errors(fail),
			     file_type(chr)
			   ],
			   CHRFile),
	file_name_extension(Base, chr, CHRFile), !,
	file_name_extension(Base, pl, PlFile),
	ensure_chr_compiled(CHRFile, PlFile),
	load_files(Module:PlFile, Options).

ensure_chr_compiled(CHRFile, PlFile) :-
	time_file(CHRFile, CHRTime),
	time_file(PlFile, PlTime),
	PlTime > CHRTime, !.
ensure_chr_compiled(CHRFile, PlFile) :-
	load_files(library('chr/chr_pp'), [silent(true)]),
	call(chr_compile(CHRFile, PlFile)). % Fool XREF
