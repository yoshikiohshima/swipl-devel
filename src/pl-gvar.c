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


		 /*******************************
		 *  BACKTRACKABLE GLOBAL VARS	*
		 *******************************/

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

  t = valTermRef(LD->gvar.b_vars);
  deRef(t);
  
  if ( isVar(*t) )			/* first global variable */
  { Word table = allocGlobal(GVAR_HASHSIZE+1);
    int i;

    table[0] = PL_new_functor(ATOM_gvar, GVAR_HASHSIZE);
    for(i=1; i<=GVAR_HASHSIZE; i++)
      table[i] = ATOM_nil;
	
    t = valTermRef(LD->gvar.b_vars);
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
  
  t = valTermRef(LD->gvar.b_vars);
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
		 * NON-BACKTRACKABLE GLOBAL VARS*
		 *******************************/

static void
freezeGlobal(ARG1_LD)
{ LD->frozen_bar = LD->mark_bar = gTop;  
}


void
destroyGlobalVars()
{ GET_LD

  if ( LD->gvar.nb_vars )
  { destroyHTable(LD->gvar.nb_vars);
    LD->gvar.nb_vars = NULL;
  }

  LD->frozen_bar = NULL;		/* unless used otherwise! */
}


static
PRED_IMPL("nb_setval", 2, nb_setval, 0)
{ PRED_LD
  atom_t name;
  Word p;
  Symbol s;

  if ( !PL_get_atom_ex(A1, &name) )
    fail;

  if ( !LD->gvar.nb_vars )
  { LD->gvar.nb_vars = newHTable(32|TABLE_UNLOCKED);
  }

  p = valTermRef(A2);
  deRef(p);

  if ( (s=lookupHTable(LD->gvar.nb_vars, (void*)name)) )
  { s->value = (void*)*p;
  } else
  { addHTable(LD->gvar.nb_vars, (void*)name, (void*)*p);
    PL_register_atom(name);
  }

  if ( isTerm(*p) ||
       isReal(*p) ||
       isString(*p) ||
       isAttVar(*p) ||
       isBignum(*p) )
    freezeGlobal(PASS_LD1);

  succeed;
}


static
PRED_IMPL("nb_getval", 2, nb_getval, 0)
{ PRED_LD
  atom_t name;

  if ( !PL_get_atom_ex(A1, &name) )
    fail;

  if ( LD->gvar.nb_vars )
  { Symbol s = lookupHTable(LD->gvar.nb_vars, (void*)name);
    
    if ( s )
    { word w = (word)s->value;

      if ( !isVar(w) )
	return unify_ptrs(valTermRef(A2), &w PASS_LD);

      succeed;
    }
  }

  fail;
}


		 /*******************************
		 *	    REGISTRATION	*
		 *******************************/

BeginPredDefs(gvar)
  PRED_DEF("b_setval", 2, b_setval, 0)
  PRED_DEF("b_getval", 2, b_getval, 0)
  PRED_DEF("nb_setval", 2, nb_setval, 0)
  PRED_DEF("nb_getval", 2, nb_getval, 0)
EndPredDefs

#endif /*O_ATTVAR*/
