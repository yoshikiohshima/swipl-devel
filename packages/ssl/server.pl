:- module(server,
	  [ server/0
	  ]).

user:file_search_path(foreign, '.').

:- use_module(ssl).

:- debug(connection).

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
	thread_create(server_loop(SSL, Socket), _, []).

server_loop(SSL, Socket) :-
	ssl_accept(SSL, Socket, SocketInst, Peer),
	debug(connection, 'Connection from ~p', [Peer]),
	ssl_open(SSL, SocketInst, In, Out),
	copy_client(In, Out),
	close(In),
	close(Out),
	server_loop(SSL, Socket).

copy_client(In, Out) :-
	read_line_to_codes(In, Line),
	(   Line == end_of_file
	->  true
	;   format('Got ~s~n', [Line]),
	    format(Out, '~s~n', [Line]),
	    flush_output(Out),
	    copy_client(In, Out)
	).

get_server_pwd(_SSL, apenoot1) :-
	format('Returning password from server passwd hook~n').

get_cert_verify(_SSL, Certificate, Error) :-
	format('Handling detailed certificate verification~n'),
	format('Certificate: ~w, error: ~w~n', [Certificate, Error]),
	format('Server accepts the client certificate~n').

