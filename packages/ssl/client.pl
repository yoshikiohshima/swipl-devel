:- module(client,
	  [ client/0
	  ]).

user:file_search_path(foreign, '.').

:- use_module(ssl).

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
	write_server(In, Out),
	write_server(In, Out),
	write_server(In, Out),
	close(In),
	close(Out).

write_server(In, Out) :-
        format(Out, 'Hello~n', ''),
        flush_output(Out),
	read_line_to_codes(In, Line),
	(   Line == end_of_file
	->  true
	;   format('Got ~s~n', [Line])
	).

user:get_client_pwd(_SSL, apenoot2) :-
	format('Returning password from client passwd hook~n').

