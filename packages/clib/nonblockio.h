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

#ifndef H_NONBLOCKIO_INCLUDED
#define H_NONBLOCKIO_INCLUDED


		 /*******************************
		 *     GET REQUIRED HEADERS	*
		 *******************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <SWI-Stream.h>
#include <SWI-Prolog.h>

#ifdef WIN32

#include <io.h>
#include <winsock2.h>

#else /*WIN32*/

#include <sys/types.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#ifdef HAVE_H_ERRNO
extern int h_errno;
#else
#define h_errno errno
#endif

#endif /*WIN32*/


typedef enum
{ TCP_ERRNO,
  TCP_HERRNO
} tcp_error_map;

typedef enum				/* tcp_setopt() commands */
{ TCP_NONBLOCK,
  TCP_REUSEADDR,
  TCP_DISPATCH,
  TCP_INSTREAM,
  TCP_OUTSTREAM
} tcp_option;

					/* tcp_get_flags() mask */
#define SOCK_INSTREAM	0x01
#define SOCK_OUTSTREAM	0x02
#define SOCK_BIND	0x04		/* What have we done? */
#define SOCK_LISTEN	0x08
#define SOCK_CONNECT	0x10
#define SOCK_ACCEPT	0x20		/* Set on accepted sockets */
#define SOCK_NONBLOCK	0x40		/* Set to non-blocking mode */
#define SOCK_DISPATCH   0x80		/* do not dispatch events */
#define SOCK_CLOSE_SEEN	0x100		/* FD_CLOSE seen */
#define SOCK_EOF_SEEN   0x200		/* Seen end-of-file */


		 /*******************************
		 *	 BASIC FUNCTIONS	*
		 *******************************/

extern int	tcp_init(void);
extern int	tcp_cleanup(void);
extern int	tcp_debug(int level);

extern int	tcp_socket(void);
extern int	tcp_closesocket(int socket);
extern int	tcp_connect(int socket,
			    const struct sockaddr *serv_addr,
			    size_t addrlen);
extern int	tcp_bind(int socket,
			 struct sockaddr *my_addr,
			 size_t addrlen);
extern int	tcp_listen(int socket, int backlog);
extern int	tcp_accept(int master,
			   struct sockaddr *addr,
			   size_t *addrlen);

extern int	tcp_read(int socket, char *buf, int bufSize);
extern int 	tcp_write(int socket, char *buf, int bufSize);
extern int 	tcp_close_input(int socket);
extern int 	tcp_close_output(int socket);

extern int	tcp_unify_ip4(term_t ip4, unsigned long hip);
extern int	tcp_get_ip(term_t ip4, struct in_addr *ip);

extern int	tcp_error(int code, tcp_error_map map);
extern int	tcp_setopt(int socket, tcp_option opt, ...);
extern int	tcp_get_flags(int socket);


		 /*******************************
		 *	    CONVERSION		*
		 *******************************/

extern int	tcp_get_sockaddr(term_t Address, struct sockaddr_in *addr);
extern int	tcp_get_ip4(term_t ip4, struct in_addr *ip);

#endif /*H_NONBLOCKIO_INCLUDED*/
