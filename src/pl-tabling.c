/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, VU University Amsterdam
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

#include "pl-incl.h"
#include "pl-tabling.h"

static worklist_set *
global_worklist(void)
{ GET_LD

  if ( !LD->tabling.worklist )
  { worklist_set *wl = PL_malloc(sizeof(*wl));
    initBuffer(&wl->members);
    LD->tabling.worklist = wl;
  }

  return LD->tabling.worklist;
}

		 /*******************************
		 *     THE GLOBAL WORKLIST	*
		 *******************************/

static void
add_global_worklist(worklist *wl)
{ worklist_set *wls = global_worklist();

  addBuffer(&wls->members, wl, worklist*);
  wl->in_global_wl = TRUE;
}


static worklist *
pop_worklist(void)
{ worklist_set *wls = global_worklist();

  if ( !isEmptyBuffer(&wls->members) )
  { worklist *wl = popBuffer(&wls->members, worklist*);
    wl->in_global_wl = FALSE;

    return wl;
  }

  return NULL;
}


		 /*******************************
		 *  ANSWER/SUSPENSION CLUSTERS	*
		 *******************************/

static cluster *
new_answer_cluster(trie_node *first)
{ cluster *c;

  c = PL_malloc(sizeof(*c));
  c->type = CLUSTER_ANSWERS;
  initBuffer(&c->members);
  addBuffer(&c->members, first, trie_node*);

  return c;
}

static void
add_to_answer_cluster(cluster *c, trie_node *answer)
{ addBuffer(&c->members, answer, trie_node*);
}

static cluster *
new_suspension_cluster(term_t first)
{ cluster *c;

  c = PL_malloc(sizeof(*c));
  c->type = CLUSTER_SUSPENSIONS;
  initBuffer(&c->members);
  addBuffer(&c->members, PL_record(first), record_t);

  return c;
}

static void
add_to_suspension_cluster(cluster *c, term_t suspension)
{ addBuffer(&c->members, PL_record(suspension), record_t);
}


		 /*******************************
		 *	   TABLE WORKLIST	*
		 *******************************/

static worklist *
new_worklist(trie *trie)
{ worklist *wl;

  wl = PL_malloc(sizeof(*wl));
  memset(wl, 0, sizeof(*wl));
  wl->magic = WORKLIST_MAGIC;
  wl->table = trie;
  trie->data.worklist = wl;

  return wl;
}


static void
wkl_append_left(worklist *wl, cluster *c)
{ if ( wl->head )
  { c->prev = NULL;
    c->next = wl->head;
    wl->head->prev = c;
    wl->head = c;
  } else
  { c->next = c->prev = NULL;
    wl->head = wl->tail = c;
  }
}


static void
wkl_append_right(worklist *wl, cluster *c)
{ if ( wl->tail )
  { c->next = NULL;
    c->prev = wl->tail;
    wl->tail->next = c;
    wl->tail = c;
  } else
  { c->next = c->prev = NULL;
    wl->head = wl->tail = c;
  }
}


static void
potentially_add_to_global_worklist(worklist *wl)
{ if ( !wl->in_global_wl )
    add_global_worklist(wl);
}


static void
wkl_add_answer(worklist *wl, trie_node *node)
{ potentially_add_to_global_worklist(wl);
  if ( wl->head && wl->head->type == CLUSTER_ANSWERS )
  { add_to_answer_cluster(wl->head, node);
  } else
  { cluster *c = new_answer_cluster(node);
    wkl_append_left(wl, c);
    if ( !wl->riac )
      wl->riac = c;
  }
}


static void
wkl_add_suspension(worklist *wl, term_t suspension)
{ potentially_add_to_global_worklist(wl);
  if ( wl->tail && wl->tail->type == CLUSTER_SUSPENSIONS )
  { add_to_suspension_cluster(wl->tail, suspension);
  } else
  { cluster *c = new_suspension_cluster(suspension);
    wkl_append_right(wl, c);
    if ( c->prev && c->prev->type == CLUSTER_ANSWERS )
      wl->riac = c->prev;
  }
}


		 /*******************************
		 *	PROLOG CONNECTION	*
		 *******************************/

static int
get_worklist(term_t t, worklist **wlp)
{ GET_LD
  void *ptr;

  if ( PL_get_pointer(t, &ptr) )
  { worklist *wl = ptr;
    assert(wl->magic == WORKLIST_MAGIC);
    *wlp = wl;
    return TRUE;
  }

  return PL_type_error("worklist", t);
}

static int
get_trie_node(term_t t, trie_node **np)
{ GET_LD
  void *ptr;

  if ( PL_get_pointer(t, &ptr) )
  { trie_node *n = ptr;
    *np = n;
    return TRUE;
  }

  return PL_type_error("trie_node", t);
}


/** '$tbl_new_worklist'(-Worklist, +Trie) is det.
 *
 * Create a new worklist for Trie add add it it the global worklist
 * set.
 */

static
PRED_IMPL("$tbl_new_worklist", 2, tbl_new_worklist, 0)
{ PRED_LD
  trie *trie;

  if ( get_trie(A2, &trie) )
  { worklist *wl = new_worklist(trie);

    add_global_worklist(wl);
    return PL_unify_pointer(A1, wl);
  }

  return FALSE;
}


/** '$tbl_pop_worklist'(-Worklist) is semidet.
 *
 * Pop next worklist from the global worklist.
 */

static
PRED_IMPL("$tbl_pop_worklist", 1, tbl_pop_worklist, 0)
{ PRED_LD
  worklist *wl;

  if ( (wl=pop_worklist()) )
    return PL_unify_pointer(A1, wl);

  return FALSE;
}

/** '$tbl_wkl_add_answer'(+Worklist, +AnswerHandle) is det.
 *
 * Add an answer to the worklist. The   answer  is represented by a trie
 * node in the answer table.
 */

static
PRED_IMPL("$tbl_wkl_add_answer", 2, tbl_wkl_add_answer, 0)
{ worklist *wl;
  trie_node *node = NULL;		/* keep compiler happy */

  if ( get_worklist(A1, &wl) &&
       get_trie_node(A2, &node) )
  { wkl_add_answer(wl, node);
    return TRUE;
  }

  return FALSE;
}

/** '$tbl_wkl_add_suspension'(+Worklist, +Suspension) is det.
 *
 * Add a suspension to the worklist.
 */

static
PRED_IMPL("$tbl_wkl_add_suspension", 2, tbl_wkl_add_suspension, 0)
{ worklist *wl;

  if ( get_worklist(A1, &wl) )
  { wkl_add_suspension(wl, A2);
    return TRUE;
  }

  return FALSE;
}



		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(tabling)
  PRED_DEF("$tbl_new_worklist",	      2, tbl_new_worklist,	 0)
  PRED_DEF("$tbl_pop_worklist",	      1, tbl_pop_worklist,	 0)
  PRED_DEF("$tbl_wkl_add_answer",     2, tbl_wkl_add_answer,	 0)
  PRED_DEF("$tbl_wkl_add_suspension", 2, tbl_wkl_add_suspension, 0)
EndPredDefs
