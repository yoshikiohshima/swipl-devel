/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2004, University of Amsterdam

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

:- asserta(user:file_search_path(foreign, '.')).

:- use_module(ssl).
:- use_module(library(debug)).
:- use_module(library(readutil)).

%:- debug(connection).
%:- debug(_).

test :-
	thread_create(server, Id, []),
	thread_get_message(Started),
	Started == started,
	client,
	thread_join(Id, Status),
	Status == true.

		 /*******************************
		 *	       SERVER		*
		 *******************************/

:- dynamic
	stop_server/0.

server :-
	ssl_init(SSL, server,
		 [ host('localhost'),
                   port(1111),
                   cert(1),
                   peer_cert(1),
		   cacert_file('etc/demoCA/cacert.pem'),
		   certificate_file('etc/server/server-cert.pem'),
		   key_file('etc/server/server-key.pem'),
               	   cert_verify_hook(get_cert_verify),
%		   password('apenoot1'),
		   pem_password_hook(get_server_pwd)
		 ]),
	ssl_socket(SSL, Socket),
	thread_send_message(main, started),
	server_loop(SSL, Socket),
	ssl_exit(SSL).

server_loop(SSL, Socket) :-
	ssl_accept(SSL, Socket, SocketInst, Peer),
	debug(connection, 'Connection from ~p', [Peer]),
	ssl_open(SSL, SocketInst, In, Out),
	copy_client(In, Out),
	close(In),
	close(Out),
	(   retract(stop_server)
	->  true
	;   server_loop(SSL, Socket)
	).

copy_client(In, Out) :-
	read_line_to_codes(In, Line),
	(   Line == end_of_file
	->  true
	;   debug(data, 'Got ~s~n', [Line]),
	    format(Out, '~s~n', [Line]),
	    flush_output(Out),
	    (	Line = "bye"
	    ->	assert(stop_server)
	    ;	true
	    ),
	    copy_client(In, Out)
	).

get_server_pwd(_SSL, apenoot1) :-
	debug(passwd, 'Returning password from server passwd hook', []).

get_cert_verify(_SSL, Certificate, Error) :-
	debug(certificate,
	      'Certificate: ~w, error: ~w', [Certificate, Error]).


		 /*******************************
		 *	       CLIENT		*
		 *******************************/

client :-
	ssl_init(SSL, client,
		 [ host('localhost'),
                   port(1111),
                   cert(1),
                   peer_cert(1),
		   cacert_file('etc/demoCA/cacert.pem'),
		   certificate_file('etc/client/client-sign.pem'),
		   key_file('etc/client/client-sign-key.pem'),
%		   password('apenoot2'),
		   pem_password_hook(get_client_pwd)
		 ]),
	ssl_socket(SSL, Socket),
	client_loop(SSL, Socket),
        ssl_exit(SSL).

client_loop(SSL, Socket) :-
	ssl_connect(SSL, Socket, SocketInst),
	ssl_open(SSL, SocketInst, In, Out),
	Message = 'Hello world',
	write_server(Message, In, Out),
	write_server(Message, In, Out),
	write_server(Message, In, Out),
	write_server(bye, In, Out),
	close(In),
	close(Out).

write_server(Message, In, Out) :-
	write(Out, Message), nl(Out),
        flush_output(Out),
	read_line_to_codes(In, Line),
	(   Line == end_of_file
	->  true
	;   atom_codes(Reply, Line),
	    debug(data, 'Got ~q~n', [Reply]),
	    (	Reply == Message
	    ->	true
	    ;	format(user_error, 'ERROR: Sent ~q, Got ~q~n',
		       [Message, Reply])
	    )
	).

user:get_client_pwd(_SSL, apenoot2) :-
	debug(passwd, 'Returning password from client passwd hook', []).
