/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2012, University of Amsterdam
                              VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#include <Foundation/Foundation.h>

#include <stdio.h>

#define PL_ARITY_AS_SIZE
#include "SWI-Prolog.h"

#include "ios-main.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This is SWI-Prolog's main(),  creating   swipl  or  swipl.exe (Windows).
SWI-Prolog itself is in  the   library  libswipl.{a,so,dll,...}, this is
merely a main() routine that sets up I/O and uses SWI-Prolog's embedding
interface to get the system going.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *		MAIN		*
		 *******************************/


int
ios_main(void)
{
  NSString *path;

  path = [[NSBundle mainBundle] executablePath];
  const char *execPath = [path cStringUsingEncoding:NSUTF8StringEncoding];

  path = [[NSBundle mainBundle] resourcePath];
  const char *rscPath = [path cStringUsingEncoding:NSUTF8StringEncoding];

  NSArray *paths = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES);
  path = [paths objectAtIndex:0];
  const char *documentsPath = [path cStringUsingEncoding:NSUTF8StringEncoding];

  path = [[NSBundle mainBundle] bundlePath];
  const char *mainDir = [path cStringUsingEncoding:NSUTF8StringEncoding];

  char mainPath[MAXPATHLEN] = {0};
  strcpy(mainPath, mainDir);
  strcat(mainPath, "/main.pl");

  const char *arg[] = {"swipl", mainPath, NULL};  
  const char *dirs[] = {execPath, rscPath, documentsPath, NULL};

  //  const char *arg[] = {mainPath, NULL};
  //if ( !PL_initialise(1, arg, dirs) )
  if ( !ios_PL_initialise(2, arg, 3, dirs) )
    PL_halt(1);

  return 0;
}
  
