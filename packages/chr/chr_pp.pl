%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   ____ _   _ ____     ____                      _ _
%%  / ___| | | |  _ \   / ___|___  _ __ ___  _ __ (_) | ___ _ __
%% | |   | |_| | |_) | | |   / _ \| '_ ` _ \| '_ \| | |/ _ \ '__|
%% | |___|  _  |  _ <  | |__| (_) | | | | | | |_) | | |  __/ |
%%  \____|_| |_|_| \_\  \____\___/|_| |_| |_| .__/|_|_|\___|_|
%%                                          |_|
%%
%% hProlog CHR compiler:
%%
%%	* by Tom Schrijvers, K.U. Leuven, Tom.Schrijvers@cs.kuleuven.ac.be
%%
%%	* based on the SICStus CHR compilation by Christian Holzbaur
%%
%% First working version: 6 June 2003
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(chr_pp,
	  [ chr_compile/2		% +CHRFile, -PlFile
	  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(chr_translate).
:- include(chr_op).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Main

chr_compile(From, To) :-
	prolog_flag(verbose,X),
	( X == yes ->
		writeln('start of CHR compilation')
	;
		true
	),
	readfile(From, Declarations),
	( X == yes ->
		write(' * CHR compiling '),
		write(From),
		writeln('...')
	;
		true
	),
	chr_translate(Declarations,NewDeclarations),
	( X == yes ->
		write('   finished CHR compiling '),
		writeln(From)
	;
		true
	),
	writefile(To,NewDeclarations),
	( X == yes ->
		writeln('end of CHR compilation')
	;
		true
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% File Reading

readfile(File,Declarations) :-
	see(File),
	prolog_flag(verbose,X),
	( X == yes ->
		write(' * loading file '),
		write(File),
		writeln('...')
	;
		true
	),
	( readcontent(Declarations) ->
		seen,
		( X == yes ->
			write('   finished loading file '),
			writeln(File)
		;
			true
		)
	;
		seen,
		( X == yes ->
			write('   ERROR loading file '),
			writeln(File)
		;
			true
		),
		fail
	).

readcontent(C) :-
	read_term(X,
		  [ module(chr_pp)	% SWI:get operators from this module
		  ]),
	( X = (:- op(Prec,Fix,Op)) ->
		op(Prec,Fix,chr_pp:Op)	% SWI: define the operators there
	;
		true
	),
	( X == end_of_file ->
		C = []
	;
		C = [X | Xs],
		readcontent(Xs)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% File Writing

writefile(File, Declarations) :-
%	prolog_flag(print_depth,Old,134217727),		% SWI
	prolog_flag(verbose,X),
	( X == yes ->
		write(' * writing file '),
		write(File),
		writeln('...')
	;
		true
	),
	tell(File),
	( writecontent(Declarations) ->
		told,
%		prolog_flag(print_depth,_,Old),		% SWI
		( X == yes ->
			write('   finished writing file '),
			writeln(File)
		;
			true
		)
	;
		told,
%		prolog_flag(print_depth,_,Old),		% SWI
		( X == yes ->
			write('   ERROR writing file '),
			writeln(File)
		;
			true
		),
		fail
	).

writecontent([]).
writecontent([D|Ds]) :-
	portray_clause(D),		% SWI-Prolog
	writecontent(Ds).

