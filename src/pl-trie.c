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
#include "pl-trie.h"
#include "pl-indirect.h"
#include "pl-termwalk.c"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file implements tries of  terms.  The   trie  itself  lives  in the
program space and is represented by a (symbol) handle. This implies that
tries are subject to garbage collection.

A path through a trie represents a  sequence of tokens. For representing
terms, these tokens are functor symbols,   variables  and atomic values.
The _value_ associated with a  term  always   appears  in  a _leaf_ node
because a sequence that represents a term   is  _never_ the prefix of of
the sequence of another term.

TODO
  - Query and limit size of the tries
  - Avoid using a hash-table for small number of branches
  - Thread safe reclaiming
    - Reclaim single-child node after moving to a hash
  - Provide pruning of the trie (thread safe)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


static void trie_destroy(trie *trie);

		 /*******************************
		 *	       SYMBOL		*
		 *******************************/

typedef struct tref
{ trie *trie;				/* represented trie */
} tref;

static int
write_trie_ref(IOSTREAM *s, atom_t aref, int flags)
{ tref *ref = PL_blob_data(aref, NULL, NULL);
  (void)flags;

  Sfprintf(s, "<trie>(%p)", ref->trie);
  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GC a message queue from the atom  garbage collector. This should be fine
because atoms in messages do  not  have   locked  atoms,  so  we are not
calling atom functions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
release_trie_ref(atom_t aref)
{ tref *ref = PL_blob_data(aref, NULL, NULL);
  trie *t;

  if ( (t=ref->trie) )
    trie_destroy(t);			/* can be called twice */

  return TRUE;
}


static int
save_trie(atom_t aref, IOSTREAM *fd)
{ tref *ref = PL_blob_data(aref, NULL, NULL);
  (void)fd;

  return PL_warning("Cannot save reference to <trie>(%p)", ref->trie);
}


static atom_t
load_trie(IOSTREAM *fd)
{ (void)fd;

  return PL_new_atom("<saved-trie-ref>");
}


static PL_blob_t trie_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE,
  "trie",
  release_trie_ref,
  NULL,
  write_trie_ref,
  NULL,
  save_trie,
  load_trie
};

		 /*******************************
		 *	     THE TRIE		*
		 *******************************/

static trie_node       *new_trie_node(word key);
static void		clear_vars(Word k, size_t var_number ARG_LD);
static void		trie_empty(trie *trie);
static void		destroy_node(trie_node *n);
static void		clear_node(trie_node *n);

static inline void
acquire_key(word key)
{ if ( isAtom(key) )
    PL_register_atom(key);
}

static inline void
release_key(word key)
{ if ( isAtom(key) )
    PL_unregister_atom(key);
}


static trie*
trie_create(void)
{ trie *trie;

  if ( (trie = PL_malloc(sizeof(*trie))) )
  { memset(trie, 0, sizeof(*trie));

    return trie;
  } else
  { PL_resource_error("memory");
    return NULL;
  }
}


static void
trie_destroy(trie *trie)
{ DEBUG(MSG_TRIE_GC, Sdprintf("Destroying trie %p\n", trie));
  PL_free(trie);
}


static void
trie_empty(trie *trie)
{ indirect_table *it = trie->indirects;

  clear_node(&trie->root);			/* TBD: verify not accessed */
  if ( it && COMPARE_AND_SWAP(&trie->indirects, it, NULL) )
    destroy_indirect_table(it);
}


static trie_node *
get_child(trie_node *n, word key ARG_LD)
{ trie_children children = n->children;

  if ( children.any )
  { switch( children.any->type )
    { case TN_KEY:
	if ( children.key->key == key )
	  return children.key->child;
        return NULL;
      case TN_HASHED:
	return lookupHTable(children.hash->table, (void*)key);
      default:
	assert(0);
    }
  }

  return NULL;
}


static trie_node *
new_trie_node(word key)
{ trie_node *n = PL_malloc(sizeof(*n));

  memset(n, 0, sizeof(*n));
  acquire_key(key);
  n->key = key;

  return n;
}


static void
destroy_hnode(trie_children_hashed *hnode)
{ destroyHTable(hnode->table);
}

static void
clear_node(trie_node *n)
{ trie_children children = n->children;

  release_key(n->key);
  if ( children.any &&
       COMPARE_AND_SWAP(&n->children.any, children.any, NULL) )
  { switch( children.any->type )
    { case TN_KEY:
        PL_free(children.key);
	break;
      case TN_HASHED:
	destroy_hnode(children.hash);
        break;
    }
  }
}

static void
destroy_node(trie_node *n)
{ clear_node(n);
  PL_free(n);
}


static void
free_hnode_symbol(void *key, void *value)
{ (void)key;

  destroy_node(value);
}


static trie_node *
insert_child(trie_node *n, word key ARG_LD)
{ for(;;)
  { trie_children children = n->children;

    if ( children.any )
    { switch( children.any->type )
      { case TN_KEY:
	{ if ( children.key->key == key )
	  { return children.key->child;
	  } else
	  { trie_children_hashed *hnode = PL_malloc(sizeof(*hnode));
	    trie_node *new = new_trie_node(key);

	    hnode->type  = TN_HASHED;
	    hnode->table = newHTable(4);
	    hnode->table->free_symbol = free_hnode_symbol;
	    addHTable(hnode->table, (void*)children.key->key,
				    children.key->child);
	    addHTable(hnode->table, (void*)key, (void*)new);

	    if ( COMPARE_AND_SWAP(&n->children.hash, children.any, hnode) )
	    { new->parent = n;
	      return new;
	    }
	    destroy_hnode(hnode);
	    continue;
	  }
	}
	case TN_HASHED:
	{ trie_node *new = new_trie_node(key);
	  trie_node *old = addHTable(children.hash->table,
				     (void*)key, (void*)new);

	  if ( new == old )
	  { new->parent = n;
	  } else
	  { destroy_node(new);
	  }
	  return old;
	}
	default:
	  assert(0);
      }
    } else
    { trie_children_key *child = PL_malloc(sizeof(*child));

      child->type  = TN_KEY;
      child->key   = key;
      child->child = new_trie_node(key);

      if ( COMPARE_AND_SWAP(&n->children.key, NULL, child) )
      { child->child->parent = n;
	return child->child;
      }
      destroy_node(child->child);
      PL_free(child);
    }
  }
}


static trie_node *
follow_node(trie_node *n, word value, int add ARG_LD)
{ trie_node *child;

  if ( (child=get_child(n, value PASS_LD)) )
    return child;

  if ( add )
    return insert_child(n, value PASS_LD);
  else
    return NULL;
}


static word
trie_intern_indirect(trie *trie, word w, int add ARG_LD)
{ for(;;)
  { if ( trie->indirects )
    { return intern_indirect(trie->indirects, w, add PASS_LD);
    } else
    { indirect_table *newtab = new_indirect_table();

      if ( !COMPARE_AND_SWAP(&trie->indirects, NULL, newtab) )
	destroy_indirect_table(newtab);
    }
  }
}


#define TRIE_LOOKUP_CONTAINS_ATTVAR	-10
#define TRIE_LOOKUP_CYCLIC		-11

static int
trie_lookup(trie *trie, trie_node **nodep, Word k, int add ARG_LD)
{ term_agenda agenda;
  Word p;
  trie_node *node = &trie->root;
  size_t var_number = 0;
  int rc = TRUE;
  int compounds = 0;

  initTermAgenda(&agenda, 1, k);
  while( node && (p=nextTermAgenda(&agenda)) )
  { word w = *p;

    switch( tag(w) )
    { case TAG_VAR:
	if ( isVar(w) )
	  *p = w = ((((word)++var_number))<<LMASK_BITS)|TAG_VAR;
        node = follow_node(node, w, add PASS_LD);
	break;
      case TAG_ATTVAR:
	rc = TRIE_LOOKUP_CONTAINS_ATTVAR;
        node = NULL;
        break;
      case TAG_COMPOUND:
      { Functor f = valueTerm(w);
        int arity = arityFunctor(f->definition);
	node = follow_node(node, f->definition, add PASS_LD);

	if ( ++compounds == 1000 && !is_acyclic(p PASS_LD) )
	{ rc = TRIE_LOOKUP_CYCLIC;
	  node = NULL;
	} else
	{ pushWorkAgenda(&agenda, arity, f->arguments);
	}
	break;
      }
      default:
      { if ( !isIndirect(w) )
	{ node = follow_node(node, w, add PASS_LD);
	} else
	{ word i = trie_intern_indirect(trie, w, add PASS_LD);

	  if ( i )
	    node = follow_node(node, i, add PASS_LD);
	  else
	    node = NULL;
	}
      }
    }
  }
  clearTermAgenda(&agenda);
  clear_vars(k, var_number PASS_LD);

  if ( rc == TRUE )
  { if ( node )
      *nodep = node;
    else
      rc = FALSE;
  }

  return rc;
}


static void
clear_vars(Word k, size_t var_number ARG_LD)
{ if ( var_number > 0 )
  { term_agenda agenda;
    Word p;

    initTermAgenda(&agenda, 1, k);
    while( var_number > 0 && (p=nextTermAgenda(&agenda)) )
    { word w = *p;

      switch( tag(w) )
      { case TAG_VAR:
	{ if ( !isVar(*p) )
	  { setVar(*p);
	    --var_number;
	  }
	  break;
	}
        case TAG_COMPOUND:
	{ Functor f = valueTerm(w);
	  int arity = arityFunctor(f->definition);

	  pushWorkAgenda(&agenda, arity, f->arguments);
	  break;
	}
      }
    }
    clearTermAgenda(&agenda);

    assert(var_number == 0);
  }
}



		 /*******************************
		 *	  PROLOG BINDING	*
		 *******************************/

#define unify_trie(t, trie) unify_trie__LD(t, trie PASS_LD)

static int
unify_trie__LD(term_t t, trie *trie ARG_LD)
{ return PL_unify_atom(t, trie->symbol);
}

static int
get_trie(term_t t, trie **tp)
{ void *data;
  PL_blob_t *type;

  if ( PL_get_blob(t, &data, NULL, &type) && type == &trie_blob )
  { tref *ref = data;

    if ( ref->trie->magic == TRIE_MAGIC )
    { *tp = ref->trie;
      return TRUE;
    }

    return PL_existence_error("trie", t);
  }

  return PL_type_error("trie", t);
}


static int
trie_error(int rc, term_t culprit)
{ switch(rc)
  { case TRIE_LOOKUP_CONTAINS_ATTVAR:
      return PL_type_error("free_of_attvar", culprit);
    case TRIE_LOOKUP_CYCLIC:
      return PL_type_error("acyclic_term", culprit);
    default:
      return FALSE;
  }
}

static
PRED_IMPL("trie_new", 1, trie_new, 0)
{ PRED_LD
  tref ref;

  if ( (ref.trie = trie_create()) )
  { int new;

    ref.trie->symbol = lookupBlob((void*)&ref, sizeof(ref),
				   &trie_blob, &new);
    ref.trie->magic = TRIE_MAGIC;

    return unify_trie(A1, ref.trie);
  }

  return FALSE;
}


static
PRED_IMPL("trie_destroy", 1, trie_destroy, 0)
{ trie *trie;

  if ( get_trie(A1, &trie) )
  { trie_empty(trie);
    trie->magic = TRIE_CMAGIC;

    return TRUE;
  }

  return FALSE;
}


/**
 * trie_insert(+Trie, +Key, +Value) is semidet.
 *
 * True if Key was added as a new   key  to the trie and associated with
 * Value. False if Key was already in the trie with Value
 *
 * @error permission_error if Key was associated with a different value
 */

static
PRED_IMPL("trie_insert", 3, trie_insert, 0)
{ PRED_LD
  trie *trie;

  if ( get_trie(A1, &trie) )
  { Word kp, vp;
    trie_node *node;
    int rc;

    kp = valTermRef(A2);
    vp = valTermRef(A3);
    deRef(vp);

    if ( !isAtomic(*vp) || isFloat(*vp) )
      return PL_type_error("primitive", A3);
    if ( isBignum(*vp) )
      return PL_domain_error("primitive", A3);

    if ( (rc=trie_lookup(trie, &node, kp, TRUE PASS_LD)) == TRUE )
    { if ( node->value )
      { if ( node->value == *vp )
	  return FALSE;				/* already in trie */
	return PL_permission_error("modify", "trie_key", A2);
      }
      acquire_key(*vp);
      node->value = *vp;

      return TRUE;
    }

    return trie_error(rc, A2);
  }

  return FALSE;
}


/**
 * trie_insert_new(+Trie, +Term, -Handle) is semidet.
 *
 * Add Term to Trie and unify Handle with a handle to the term.
 * Fails if Term is already in Trie.
 *
 * @bug Handle is currently a pointer.  In future versions we will
 * use a dynamic array for the trie nodes and return an integer to
 * guarantee safe lookup.
 */

static
PRED_IMPL("trie_insert_new", 3, trie_insert_new, 0)
{ PRED_LD
  trie *trie;

  if ( get_trie(A1, &trie) )
  { Word kp;
    trie_node *node;
    int rc;

    kp = valTermRef(A2);

    if ( (rc=trie_lookup(trie, &node, kp, TRUE PASS_LD)) == TRUE )
    { if ( node->value )
      { if ( node->value == ATOM_nil )
	  return FALSE;				/* already in trie */
	return PL_permission_error("modify", "trie_key", A2);
      }
      node->value = ATOM_nil;

      return PL_unify_pointer(A3, node);
    }

    return trie_error(rc, A2);
  }

  return FALSE;
}


static
PRED_IMPL("trie_lookup", 3, trie_lookup, 0)
{ PRED_LD
  trie *trie;

  if ( get_trie(A1, &trie) )
  { Word kp;
    trie_node *node;
    int rc;

    kp = valTermRef(A2);

    if ( (rc=trie_lookup(trie, &node, kp, FALSE PASS_LD)) == TRUE )
    { if ( node->value )
	return _PL_unify_atomic(A3, node->value);
      return FALSE;
    }

    return trie_error(rc, A2);
  }

  return FALSE;
}

#define NVARS_FAST 100

typedef struct
{ term_agenda agenda;
  int is_compound;
  Word gp;					/* global pointer */
  Word vp;					/* result location */
  Word *varp;					/* variable pointers */
  word result;					/* final term */
  trie *trie;					/* trie we work on */
  Word varp_buf[NVARS_FAST];			/* variable pointer buffer */
} build_state;

static int
init_build_state(build_state *state, trie *trie,
		 size_t gsize, unsigned int nvars ARG_LD)
{ int rc;

  if ( (rc=ensureGlobalSpace(gsize, ALLOW_GC)) != TRUE )
    return raiseStackOverflow(rc);

  state->is_compound = FALSE;
  state->vp   = &state->result;
  state->gp   = gTop;
  state->trie = trie;
  state->varp = nvars <= NVARS_FAST
		       ? state->varp_buf
		       : PL_malloc(nvars*sizeof(*state->varp));
  memset(state->varp, 0, nvars*sizeof(*state->varp));

  return TRUE;
}

static void
clear_build_state(build_state *state)
{ if ( state->varp != state->varp_buf )
    PL_free(state->varp);
}

static size_t
key_gsize(trie *trie, word key)
{ if ( tagex(key) == (TAG_ATOM|STG_GLOBAL) )
    return arityFunctor(key)+1;
  if ( isIndirect(key) )
    return gsize_indirect(trie->indirects, key);

  return 0;
}

static unsigned int
key_nvar(word key)
{ if ( tag(key) == TAG_VAR )
    return (unsigned int)(key>>LMASK_BITS);
  return 0;
}

static int
eval_key(build_state *state, word key ARG_LD)
{ if ( tagex(key) == (TAG_ATOM|STG_GLOBAL) )
  { size_t arity = arityFunctor(key);

    *state->vp = consPtr(state->gp, TAG_COMPOUND|STG_GLOBAL);
    DEBUG(MSG_TRIE_PUT_TERM,
	  Sdprintf("Term %s at %s\n",
		   functorName(key), print_addr(state->gp,NULL)));

    *state->gp++ = key;
    if ( !state->is_compound )
    { initTermAgenda(&state->agenda, arity, state->gp);
      state->is_compound = TRUE;
    } else
    { if ( !pushWorkAgenda(&state->agenda, arity, state->gp) )
      { clearTermAgenda(&state->agenda);
	return raiseStackOverflow(MEMORY_OVERFLOW);
      }
    }
    state->gp += arity;
  } else if ( tag(key) == TAG_VAR )
  { unsigned int index = (unsigned int)(key>>LMASK_BITS) - 1;

    DEBUG(MSG_TRIE_PUT_TERM,
	  Sdprintf("var %d at %s\n", (int)index,
		   print_addr(state->vp,NULL)));

    if ( !state->varp[index] )
    { setVar(*state->vp);
      state->varp[index] = state->vp;
    } else
    { *state->vp = makeRefG(state->varp[index]);
    }
  } else
  { DEBUG(MSG_TRIE_PUT_TERM,
	  Sdprintf("%s at %s\n",
		   print_val(key, NULL), print_addr(state->vp,NULL)));
    if ( isAtom(key) )
      pushVolatileAtom(key);
    if ( !isIndirect(key) )
    { *state->vp = key;
    } else
    { *state->vp = extern_indirect(state->trie->indirects,
				   key, &state->gp PASS_LD);
    }
  }

  if ( state->is_compound )
    state->vp = nextTermAgendaNoDeRef(&state->agenda);

  return TRUE;
}


/**
 * trie_term(+Handle, -Term) is det.
 *
 * Retrieve a term for a handle returned by trie_insert_new/3.
 */

#define MAX_FAST 256

static
PRED_IMPL("trie_term", 2, trie_term, 0)
{ PRED_LD
  word fast[MAX_FAST];
  Word keys = fast;
  size_t keys_allocated = MAX_FAST;
  size_t kc = 0;
  void *ptr;
  int rc = TRUE;

  if ( PL_get_pointer_ex(A1, &ptr) )
  { trie_node *node;
    trie *trie_ptr;
    size_t gsize = 0;
    unsigned int nvars = 0;
    build_state state;
    ssize_t i;
						/* get the keys */
    for(node = ptr; node->parent; node = node->parent )
    { if ( kc == keys_allocated )
      { keys_allocated *= 2;
	if ( keys == fast )
	{ if ( (keys = malloc(sizeof(*keys)*keys_allocated)) )
	    memcpy(keys, fast, sizeof(fast));
	  else
	    return PL_resource_error("memory");
	} else
	{ Word newkeys;
	  if ( !(newkeys=realloc(keys, sizeof(*keys)*keys_allocated)) )
	  { free(keys);
	    return PL_resource_error("memory");
	  }
	}
      }

      keys[kc++] = node->key;
    }
    trie_ptr = (trie *)((char*)node - offsetof(trie, root));
    assert(trie_ptr->magic == TRIE_MAGIC);

    for(i=0; i<kc; i++)				/* compute sizes */
    { unsigned nv;

      gsize += key_gsize(trie_ptr, keys[i]);
      if ( (nv=key_nvar(keys[i])) > nvars )
	nvars = nv;
    }

    if ( init_build_state(&state, trie_ptr, gsize, nvars PASS_LD) )
    { for(i=kc-1; i>=0; i--)
      { if ( !eval_key(&state, keys[i] PASS_LD) )
	{ rc = FALSE;
	  break;
	}
      }

      clear_build_state(&state);
      if ( rc )
      { term_t v;

	gTop = state.gp;
	v = PL_new_term_ref();
	*valTermRef(v) = state.result;
	DEBUG(CHK_SECURE, PL_check_data(v));

	rc = PL_unify(A2, v);
      }
    }

    if ( keys != fast )
      free(keys);
  } else
    rc = FALSE;

  return rc;
}


/**
 * trie_gen(+Trie, ?Key, -Value) is nondet.
 *
 * True when Key-Value appears in Trie.
 *
 * This needs to keep  a  list  of   choice  points  for  each node with
 * multiple children. Eventually, this is probably going to be a virtual
 * machine extension, using real choice points.
 */

typedef struct trie_choice
{ union
  { void *any;
    TableEnum table;
  } choice;
  word key;
  trie_node *child;
  size_t gsize;
  unsigned int nvars;
  struct trie_choice *next;
  struct trie_choice *prev;
} trie_choice;

typedef struct
{ trie_choice *head;		/* head of trie nodes */
  trie_choice *tail;		/* tail of trie nodes */
  trie        *trie;		/* trie we operate on */
} trie_gen_state;


static void
clear_trie_state(trie_gen_state *state)
{ trie_choice *ch, *next;

  for(ch=state->head; ch; ch=next)
  { next = ch->next;

    if ( ch->choice.table )
      freeTableEnum(ch->choice.table);
    PL_free(ch);
  }
}


trie_choice *
add_choice(trie_gen_state *state, trie_node *node)
{ trie_choice *ch = PL_malloc(sizeof(*ch));
  trie_children children = node->children;
  size_t psize = state->tail ? state->tail->gsize : 0;
  unsigned int nvars = state->tail ? state->tail->nvars : 0;
  unsigned int keyvar;

  if ( children.any )
  { switch( children.any->type )
    { case TN_KEY:
      {	ch->key    = children.key->key;
	ch->child  = children.key->child;
        ch->choice.any = NULL;
	break;
      }
      case TN_HASHED:
      { void *k, *v;

	ch->choice.table = newTableEnum(children.hash->table);
        advanceTableEnum(ch->choice.table, &k, &v);
	ch->key   = (word)k;
	ch->child = (trie_node*)v;
	break;
      }
      default:
	assert(0);
    }
  } else
  { memset(ch, 0, sizeof(*ch));
    ch->child = node;
  }

  ch->gsize = psize + key_gsize(state->trie, ch->key);
  if ( (keyvar=key_nvar(ch->key)) > nvars )
  { DEBUG(MSG_TRIE_PUT_TERM, Sdprintf("Got var %d\n", keyvar));
    nvars = keyvar;
  }
  ch->nvars = nvars;

  ch->next = NULL;
  ch->prev = state->tail;
  if ( state->tail )
    state->tail->next = ch;
  else
    state->head = ch;
  state->tail = ch;

  return ch;
}


static int
descent_node(trie_gen_state *state, trie_choice *ch)
{ while( ch->child->children.any )
  { ch = add_choice(state, ch->child);
  }

  return ch->child->value != 0;
}


static trie_choice *
previous_choice(trie_gen_state *state)
{ trie_choice *ch = state->tail;

  if ( ch->choice.table )
    freeTableEnum(ch->choice.table);
  state->tail = ch->prev;
  if ( state->tail )
    state->tail->next = NULL;
  else
    state->head = NULL;
  PL_free(ch);

  return state->tail;
}


static int
advance_node(trie_choice *ch)
{ if ( ch->choice.table )
  { void *k, *v;

    if ( advanceTableEnum(ch->choice.table, &k, &v) )
    { ch->key   = (word)k;
      ch->child = (trie_node*)v;

      return TRUE;
    }
  }

  return FALSE;
}


static int
next_choice(trie_gen_state *state)
{ trie_choice *ch;

  for( ch = state->tail; ch; ch = previous_choice(state) )
  { if ( advance_node(ch) &&
	 descent_node(state, ch) )
      return TRUE;
  }

  return FALSE;
}


static int
put_trie_term(term_t term, Word value, trie_gen_state *gstate ARG_LD)
{ int rc = TRUE;
  trie_choice *ch;
  build_state bstate;

  if ( init_build_state(&bstate,
			gstate->trie,
			gstate->tail->gsize,
			gstate->tail->nvars+1 PASS_LD) )
  { for( ch = gstate->head; ch; ch = ch->next )
    { if ( !eval_key(&bstate, ch->key PASS_LD) )
      { rc = FALSE;
	break;
      }
      if ( !ch->next )
      { *value = ch->child->value;
      }
    }

    clear_build_state(&bstate);
    if ( rc )
    { gTop = bstate.gp;
      *valTermRef(term) = bstate.result;
      DEBUG(CHK_SECURE, PL_check_data(term));
    }
  } else
    rc = FALSE;

  return rc;
}


static
PRED_IMPL("trie_gen", 3, trie_gen, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  trie_gen_state state_buf;
  trie_gen_state *state;
  term_t key;
  word value;
  fid_t fid;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { trie *trie;

      if ( get_trie(A1, &trie) )
      { state = &state_buf;
	memset(state, 0, sizeof(*state));

	if ( trie->root.children.any )
	{ state->trie = trie;
	  if ( !descent_node(state, add_choice(state, &trie->root)) &&
	       !next_choice(state) )
	  { clear_trie_state(state);
	    return FALSE;
	  }
	  break;
	}
      }
      return FALSE;
    }
    case FRG_REDO:
      state = CTX_PTR;
      break;
    case FRG_CUTTED:
      state = CTX_PTR;
      clear_trie_state(state);
      freeForeignState(state, sizeof(*state));
      return TRUE;
    default:
      assert(0);
      return FALSE;
  }

  key = PL_new_term_ref();
  fid = PL_open_foreign_frame();

  for( ; state->head; next_choice(state) )
  { if ( !put_trie_term(key, &value, state PASS_LD) )
    { PL_close_foreign_frame(fid);
      return FALSE;				/* resource error */
    }
    if ( PL_unify(A2, key) && _PL_unify_atomic(A3, value) )
    { if ( next_choice(state) )
      { if ( state == &state_buf )
	{ state = allocForeignState(sizeof(*state));
	  memcpy(state, &state_buf, sizeof(*state));
	}
	PL_close_foreign_frame(fid);
	ForeignRedoPtr(state);
      } else
      { clear_trie_state(state);
	PL_close_foreign_frame(fid);
	return TRUE;
      }
    } else if ( PL_exception(0) )
    { return FALSE;				/* error */
    } else
    { PL_rewind_foreign_frame(fid);
    }
  }

  clear_trie_state(state);
  PL_close_foreign_frame(fid);
  return FALSE;
}

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(trie)
  PRED_DEF("trie_new",            1, trie_new,           0)
  PRED_DEF("trie_destroy",        1, trie_destroy,       0)
  PRED_DEF("trie_insert",         3, trie_insert,        0)
  PRED_DEF("trie_insert_new",     3, trie_insert_new,    0)
  PRED_DEF("trie_lookup",         3, trie_lookup,        0)
  PRED_DEF("trie_term",		  2, trie_term,		 0)
  PRED_DEF("trie_gen",            3, trie_gen,           PL_FA_NONDETERMINISTIC)
EndPredDefs

void
initTries(void)
{ PL_register_blob_type(&trie_blob);
}
