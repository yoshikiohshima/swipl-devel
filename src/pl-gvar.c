/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/*#define O_DEBUG 1*/
#include "pl-incl.h"
#ifdef O_GVAR

#undef LD
#define LD LOCAL_LD

#ifndef GVAR_HASHSIZE
#define GVAR_HASHSIZE 256
#endif

static int
find_var(Word l, atom_t name, Word *vp ARG_LD)
{ for(;;)
  { deRef(l);
    
    if ( isNil(*l) )
    { *vp = l;
      fail;
    } else if ( isTerm(*l) )
    { Functor f = valueTerm(*l);

      if ( f->definition == FUNCTOR_att3 )
      { Word n;

	deRef2(&f->arguments[0], n);
	if ( *n == name )
	{ *vp = &f->arguments[1];

	  succeed;
	} else
	{ l = &f->arguments[2];
	}
      } else
      { *vp = NULL;			/* bad attribute list */
	fail;
      }
    } else
    { *vp = NULL;			/* bad attribute list */
      fail;
    }
  }
}


static
PRED_IMPL("b_setval", 2, b_setval, 0)
{ PRED_LD
  atom_t name;
  Word t;

  if ( !PL_get_atom_ex(A1, &name) )
    fail;

  t = valTermRef(LD->gvar.table);
  deRef(t);
  
  if ( isVar(*t) )			/* first global variable */
  { Word table = allocGlobal(GVAR_HASHSIZE+1);
    int i;

    table[0] = PL_new_functor(ATOM_gvar, GVAR_HASHSIZE);
    for(i=1; i<=GVAR_HASHSIZE; i++)
      table[i] = ATOM_nil;
	
    t = valTermRef(LD->gvar.table);
    deRef(t);
    *t = consPtr(table, TAG_COMPOUND|STG_GLOBAL);
    Trail(t);
  }

  requireStack(global, 4*sizeof(word));

  if ( isTerm(*t) )
  { Functor f = valueTerm(*t);
    int arity = arityFunctor(f->definition);
    int key = atomValue(name)->hash_value % arity;
    Word l = &f->arguments[key];
    Word vp;

    if ( find_var(l, name, &vp PASS_LD) )
    { TrailAssignment(vp);
      *vp = linkVal(valTermRef(A2));

      succeed;
    } else if ( vp )
    { Word at = allocGlobal(4);

      at[0] = FUNCTOR_att3;
      at[1] = name;
      at[2] = linkVal(valTermRef(A2));
      at[3] = ATOM_nil;

      TrailAssignment(vp);
      *vp = consPtr(at, TAG_COMPOUND|STG_GLOBAL);

      succeed;
    }
  }

  assert(0);
  fail;
}


static
PRED_IMPL("b_getval", 2, b_getval, 0)
{ PRED_LD
  atom_t name;
  Word t;

  if ( !PL_get_atom_ex(A1, &name) )
    fail;
  
  t = valTermRef(LD->gvar.table);
  deRef(t);

  if ( isTerm(*t) )
  { Functor f = valueTerm(*t);
    int arity = arityFunctor(f->definition);
    int key = atomValue(name)->hash_value % arity;
    Word l = &f->arguments[key];
    Word vp;

    if ( find_var(l, name, &vp PASS_LD) )
      return unify_ptrs(valTermRef(A2), vp PASS_LD);
  }

  fail;
}


		 /*******************************
		 *	    REGISTRATION	*
		 *******************************/

BeginPredDefs(gvar)
  PRED_DEF("b_setval", 2, b_setval, 0)
  PRED_DEF("b_getval", 2, b_getval, 0)
EndPredDefs

#endif /*O_ATTVAR*/
