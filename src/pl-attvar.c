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

#include "pl-incl.h"
#ifdef O_ATTVAR

#undef LD
#define LD LOCAL_LD

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines basic attributed variable   support  as described in
"Dynamic  attributes,  their  hProlog  implementation,    and   a  first
evaluation" by Bart  Demoen,  Report   CW350,  October  2002, Katholieke
Universiteit Leuven.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	     ASSIGNMENT		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
assignAttVar(Word var, Word value)		(var := value)
	
Assign  value  to  the  given  attributed    variable,   adding  a  term
wake(Attribute, Value, Tail) to the global variable resembling the goals
that should be awoken.

Before calling, av *must* point to   a  dereferenced attributed variable
and value to a legal value.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
assignAttVar(Word av, Word value ARG_LD)
{ Word wake = allocGlobal(4);		/* may NOT shift the stacks!!! */
  Word a;				/*  */
  Word tail = valTermRef(LD->attvar.tail);

  assert(isAttVar(*av));
  assert(!isRef(*value));

  if ( isAttVar(*value) )
  { if ( value > av )
    { Word tmp = av;
      av = value;
      value = tmp;
    }
  }

  a = valPAttVar(*av);
  wake[0] = FUNCTOR_wakeup3;
  wake[1] = needsRef(*a) ? makeRef(a) : *a;
  wake[2] = needsRef(*value) ? makeRef(value) : *value;
  wake[3] = ATOM_nil;

  if ( *tail )
  { Word t;				/* Non-empty list */

    deRef2(tail, t);
    TrailAssignment(t);
    *t = consPtr(wake, TAG_COMPOUND|STG_GLOBAL);
  } else				/* empty list */
  { Word head = valTermRef(LD->attvar.head);
    
    assert(isVar(*head));
    *head = consPtr(wake, TAG_COMPOUND|STG_GLOBAL);
    Trail(head);
  }

  TrailAssignment(tail);
  *tail = makeRef(wake+3);

  TrailAssignment(av);
  if ( isAttVar(*value) )
  { DEBUG(5, Sdprintf("Unifying two attvars\n"));
    *av = makeRef(value);
  } else
    *av = *value;

  succeed;
}


static int
PL_make_new_attvar(term_t var)
{ GET_LD
  Word p = valTermRef(var);

  deRef(p);

  if ( isVar(*p) )
  { Word gp;
    int is_local;

    if ( onStackArea(local, p) )
    { gp = allocGlobal(2);
      is_local = TRUE;
    } else
    { gp = allocGlobal(1);
      is_local = FALSE;
    }

#ifdef O_SHIFT_STACKS
    p = valTermRef(A1);			/* may have shifted */
    deRef(p);
#endif

    if ( is_local )
    { gp[1] = ATOM_nil;
      gp[0] = consPtr(&gp[1], TAG_ATTVAR|STG_GLOBAL);
      *p = makeRef(&gp[0]);
    } else
    { gp[0] = ATOM_nil;
      *p = consPtr(&gp[0], TAG_ATTVAR|STG_GLOBAL);
    }
    Trail(p);

    succeed;
  }

  fail;
}


static int
PL_put_attr(term_t var, term_t att)
{ GET_LD

  Word p = valTermRef(var);
  deRef(p);

  if ( isAttVar(*p) )
  { Word v = valTermRef(att);
    Word av = valPAttVar(*p);

    TrailAssignment(av);		/* See setarg/3 implementation */
    deRef(v);				/* for comments */

    if ( isVar(*v) )
    { if ( v < av )
      { *av = makeRef(v);
      } else if ( av < v )
      { setVar(*av);
	*v = makeRef(av);
      } else
	setVar(*av);
    } else
      *av = *v;

    succeed;
  }

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructive assignment or adding in a list  of the form att(Name, Value,
Rest).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
put_attr(term_t list, atom_t name, term_t value)
{ GET_LD
  Word l;

  requireStack(global, 4 * sizeof(word));	/* avoid stack-shifting */
  l = valTermRef(list);

  for(;;)
  { deRef(l);
    
    if ( isNil(*l) )
    { Word at = allocGlobal(4);

      at[0] = FUNCTOR_att3;
      at[1] = name;
      at[2] = linkVal(valTermRef(value));
      at[3] = ATOM_nil;

      TrailAssignment(l);
      *l = consPtr(at, TAG_COMPOUND|STG_GLOBAL);
      succeed;
    } else if ( isTerm(*l) )
    { Functor f = valueTerm(*l);

      if ( f->definition == FUNCTOR_att3 )
      { Word n;

	deRef2(&f->arguments[0], n);
	if ( *n == name )
	{ TrailAssignment(&f->arguments[1]);
	  f->arguments[1] = linkVal(valTermRef(value));
	  succeed;
	} else
	{ l = &f->arguments[2];
	}
      } else
	fail;
    } else
      fail;
  }
}


static int
get_attr(term_t list, atom_t name, term_t value)
{ GET_LD
  Word l = valTermRef(list);

  for(;;)
  { deRef(l);
    
    if ( isTerm(*l) )
    { Functor f = valueTerm(*l);

      if ( f->definition == FUNCTOR_att3 )
      { Word n;

	deRef2(&f->arguments[0], n);
	if ( *n == name )
	{ return unify_ptrs(valTermRef(value), &f->arguments[1] PASS_LD);
	} else
	{ l = &f->arguments[2];
	}
      } else
	fail;
    } else
      fail;
  }
}

		 /*******************************
		 *	     PREDICATES		*
		 *******************************/

static
PRED_IMPL("attvar", 1, attvar, 0)
{ PRED_LD
  term_t t = A1;
  Word p = valTermRef(t);

  deRef(p);
  if ( isAttVar(*p) )
    succeed;

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
make_new_attvar(+Var)

Creates a new attributed variable  with   attribute  []. If the variable
lives on the local stack, a new variable  is created on the global stack
and the old  one  is  made  a   reference.  Otherwise  the  variable  is
immediately bound to  an  attributed  variable.   This  is  required  as
attributed variables can outlive  the  local   frame  in  which they are
created.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static
PRED_IMPL("make_new_attvar", 1, make_new_attvar, 0)
{ if ( PL_make_new_attvar(A1) )
    succeed;

  return PL_error("make_new_attvar", 1, NULL, ERR_TYPE, ATOM_var, A1);
}


static
PRED_IMPL("get_attr", 2, get_attr, 0)
{ PRED_LD
  term_t t = A1;
  Word p = valTermRef(t);

  deRef(p);
  if ( isAttVar(*p) )
  { Word av = valPAttVar(*p);

    return unify_ptrs(valTermRef(A2), av PASS_LD);
  }

  fail;
}


static
PRED_IMPL("put_attr", 2, put_attr, 0)
{ if ( PL_put_attr(A1, A2) )
    succeed;
  
  return PL_error("put_attr", 2, NULL, ERR_TYPE, ATOM_attvar, A1);
}


static
PRED_IMPL("get_attr", 3, get_attr3, 0) /* +Var, +Name, -Value */
{ PRED_LD
  term_t al = PL_new_term_ref();
  atom_t name;

  if ( !PL_get_atom_ex(A2, &name) )
    fail;
  if ( !PL_get_attr(A1, al) )
    fail;
  
  return get_attr(al, name, A3);
}


static
PRED_IMPL("put_attr", 3, put_attr3, 0)	/* +Var, +Name, +Value */
{ PRED_LD
  term_t al = PL_new_term_ref();
  atom_t name;

  if ( !PL_get_atom_ex(A2, &name) )
    fail;
  if ( PL_is_variable(A1) )
    PL_make_new_attvar(A1);

  if ( !PL_get_attr(A1, al) )
    return PL_error("put_attr", 3, NULL, ERR_TYPE, ATOM_var, A1);
  
  return put_attr(al, name, A3);
}


static
PRED_IMPL("$get_wakeup", 1, get_wakeup, 0)
{ PRED_LD

  if ( !PL_is_variable(LD->attvar.head) )
    return PL_unify(LD->attvar.head, A1);

  fail;
}

		 /*******************************
		 *	    REGISTRATION	*
		 *******************************/

BeginPredDefs(attvar)
  PRED_DEF("attvar", 1, attvar, 0)
  PRED_DEF("make_new_attvar", 1, make_new_attvar, 0)
  PRED_DEF("get_attr", 2, get_attr, 0)
  PRED_DEF("put_attr", 2, put_attr, 0)
  PRED_DEF("put_attr", 3, put_attr3, 0)
  PRED_DEF("get_attr", 3, get_attr3, 0)
  PRED_DEF("$get_wakeup", 1, get_wakeup, 0)
EndPredDefs

#endif /*O_ATTVAR*/
