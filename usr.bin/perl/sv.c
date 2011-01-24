/*    sv.c
 *
 *    Copyright (C) 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
 *    2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009 by Larry Wall
 *    and others
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 */

/*
 * 'I wonder what the Entish is for "yes" and "no",' he thought.
 *                                                      --Pippin
 *
 *     [p.480 of _The Lord of the Rings_, III/iv: "Treebeard"]
 */

/*
 *
 *
 * This file contains the code that creates, manipulates and destroys
 * scalar values (SVs). The other types (AV, HV, GV, etc.) reuse the
 * structure of an SV, so their creation and destruction is handled
 * here; higher-level functions are in av.c, hv.c, and so on. Opcode
 * level functions (eg. substr, split, join) for each of the types are
 * in the pp*.c files.
 */

#include "EXTERN.h"
#define PERL_IN_SV_C
#include "perl.h"
#include "regcomp.h"

#define FCALL *f

#ifdef __Lynx__
/* Missing proto on LynxOS */
  char *gconvert(double, int, int,  char *);
#endif

#ifdef PERL_UTF8_CACHE_ASSERT
/* if adding more checks watch out for the following tests:
 *   t/op/index.t t/op/length.t t/op/pat.t t/op/substr.t
 *   lib/utf8.t lib/Unicode/Collate/t/index.t
 * --jhi
 */
#   define ASSERT_UTF8_CACHE(cache) \
    STMT_START { if (cache) { assert((cache)[0] <= (cache)[1]); \
			      assert((cache)[2] <= (cache)[3]); \
			      assert((cache)[3] <= (cache)[1]);} \
			      } STMT_END
#else
#   define ASSERT_UTF8_CACHE(cache) NOOP
#endif

#ifdef PERL_OLD_COPY_ON_WRITE
#define SV_COW_NEXT_SV(sv)	INT2PTR(SV *,SvUVX(sv))
#define SV_COW_NEXT_SV_SET(current,next)	SvUV_set(current, PTR2UV(next))
/* This is a pessimistic view. Scalar must be purely a read-write PV to copy-
   on-write.  */
#endif

/* ============================================================================

=head1 Allocation and deallocation of SVs.

An SV (or AV, HV, etc.) is allocated in two parts: the head (struct
sv, av, hv...) contains type and reference count information, and for
many types, a pointer to the body (struct xrv, xpv, xpviv...), which
contains fields specific to each type.  Some types store all they need
in the head, so don't have a body.

In all but the most memory-paranoid configuations (ex: PURIFY), heads
and bodies are allocated out of arenas, which by default are
approximately 4K chunks of memory parcelled up into N heads or bodies.
Sv-bodies are allocated by their sv-type, guaranteeing size
consistency needed to allocate safely from arrays.

For SV-heads, the first slot in each arena is reserved, and holds a
link to the next arena, some flags, and a note of the number of slots.
Snaked through each arena chain is a linked list of free items; when
this becomes empty, an extra arena is allocated and divided up into N
items which are threaded into the free list.

SV-bodies are similar, but they use arena-sets by default, which
separate the link and info from the arena itself, and reclaim the 1st
slot in the arena.  SV-bodies are further described later.

The following global variables are associated with arenas:

    PL_sv_arenaroot	pointer to list of SV arenas
    PL_sv_root		pointer to list of free SV structures

    PL_body_arenas	head of linked-list of body arenas
    PL_body_roots[]	array of pointers to list of free bodies of svtype
			arrays are indexed by the svtype needed

A few special SV heads are not allocated from an arena, but are
instead directly created in the interpreter structure, eg PL_sv_undef.
The size of arenas can be changed from the default by setting
PERL_ARENA_SIZE appropriately at compile time.

The SV arena serves the secondary purpose of allowing still-live SVs
to be located and destroyed during final cleanup.

At the lowest level, the macros new_SV() and del_SV() grab and free
an SV head.  (If debugging with -DD, del_SV() calls the function S_del_sv()
to return the SV to the free list with error checking.) new_SV() calls
more_sv() / sv_add_arena() to add an extra arena if the free list is empty.
SVs in the free list have their SvTYPE field set to all ones.

At the time of very final cleanup, sv_free_arenas() is called from
perl_destruct() to physically free all the arenas allocated since the
start of the interpreter.

The function visit() scans the SV arenas list, and calls a specified
function for each SV it finds which is still live - ie which has an SvTYPE
other than all 1's, and a non-zero SvREFCNT. visit() is used by the
following functions (specified as [function that calls visit()] / [function
called by visit() for each SV]):

    sv_report_used() / do_report_used()
			dump all remaining SVs (debugging aid)

    sv_clean_objs() / do_clean_objs(),do_clean_named_objs()
			Attempt to free all objects pointed to by RVs,
			and, unless DISABLE_DESTRUCTOR_KLUDGE is defined,
			try to do the same for all objects indirectly
			referenced by typeglobs too.  Called once from
			perl_destruct(), prior to calling sv_clean_all()
			below.

    sv_clean_all() / do_clean_all()
			SvREFCNT_dec(sv) each remaining SV, possibly
			triggering an sv_free(). It also sets the
			SVf_BREAK flag on the SV to indicate that the
			refcnt has been artificially lowered, and thus
			stopping sv_free() from giving spurious warnings
			about SVs which unexpectedly have a refcnt
			of zero.  called repeatedly from perl_destruct()
			until there are no SVs left.

=head2 Arena allocator API Summary

Private API to rest of sv.c

    new_SV(),  del_SV(),

    new_XIV(), del_XIV(),
    new_XNV(), del_XNV(),
    etc

Public API:

    sv_report_used(), sv_clean_objs(), sv_clean_all(), sv_free_arenas()

=cut

 * ========================================================================= */

/*
 * "A time to plant, and a time to uproot what was planted..."
 */

void
Perl_offer_nice_chunk(pTHX_ void *const chunk, const U32 chunk_size)
{
    dVAR;
    void *new_chunk;
    U32 new_chunk_size;

    PERL_ARGS_ASSERT_OFFER_NICE_CHUNK;

    new_chunk = (void *)(chunk);
    new_chunk_size = (chunk_size);
    if (new_chunk_size > PL_nice_chunk_size) {
	Safefree(PL_nice_chunk);
	PL_nice_chunk = (char *) new_chunk;
	PL_nice_chunk_size = new_chunk_size;
    } else {
	Safefree(chunk);
    }
}

#ifdef PERL_MEM_LOG
#  define MEM_LOG_NEW_SV(sv, file, line, func)	\
	    Perl_mem_log_new_sv(sv, file, line, func)
#  define MEM_LOG_DEL_SV(sv, file, line, func)	\
	    Perl_mem_log_del_sv(sv, file, line, func)
#else
#  define MEM_LOG_NEW_SV(sv, file, line, func)	NOOP
#  define MEM_LOG_DEL_SV(sv, file, line, func)	NOOP
#endif

#ifdef DEBUG_LEAKING_SCALARS
#  define FREE_SV_DEBUG_FILE(sv) Safefree((sv)->sv_debug_file)
#  define DEBUG_SV_SERIAL(sv)						    \
    DEBUG_m(PerlIO_printf(Perl_debug_log, "0x%"UVxf": (%05ld) del_SV\n",    \
	    PTR2UV(sv), (long)(sv)->sv_debug_serial))
#else
#  define FREE_SV_DEBUG_FILE(sv)
#  define DEBUG_SV_SERIAL(sv)	NOOP
#endif

#ifdef PERL_POISON
#  define SvARENA_CHAIN(sv)	((sv)->sv_u.svu_rv)
#  define SvARENA_CHAIN_SET(sv,val)	(sv)->sv_u.svu_rv = MUTABLE_SV((val))
/* Whilst I'd love to do this, it seems that things like to check on
   unreferenced scalars
#  define POSION_SV_HEAD(sv)	PoisonNew(sv, 1, struct STRUCT_SV)
*/
#  define POSION_SV_HEAD(sv)	PoisonNew(&SvANY(sv), 1, void *), \
				PoisonNew(&SvREFCNT(sv), 1, U32)
#else
#  define SvARENA_CHAIN(sv)	SvANY(sv)
#  define SvARENA_CHAIN_SET(sv,val)	SvANY(sv) = (void *)(val)
#  define POSION_SV_HEAD(sv)
#endif

/* Mark an SV head as unused, and add to free list.
 *
 * If SVf_BREAK is set, skip adding it to the free list, as this SV had
 * its refcount artificially decremented during global destruction, so
 * there may be dangling pointers to it. The last thing we want in that
 * case is for it to be reused. */

#define plant_SV(p) \
    STMT_START {					\
	const U32 old_flags = SvFLAGS(p);			\
	MEM_LOG_DEL_SV(p, __FILE__, __LINE__, FUNCTION__);  \
	DEBUG_SV_SERIAL(p);				\
	FREE_SV_DEBUG_FILE(p);				\
	POSION_SV_HEAD(p);				\
	SvFLAGS(p) = SVTYPEMASK;			\
	if (!(old_flags & SVf_BREAK)) {		\
	    SvARENA_CHAIN_SET(p, PL_sv_root);	\
	    PL_sv_root = (p);				\
	}						\
	--PL_sv_count;					\
    } STMT_END

#define uproot_SV(p) \
    STMT_START {					\
	(p) = PL_sv_root;				\
	PL_sv_root = MUTABLE_SV(SvARENA_CHAIN(p));		\
	++PL_sv_count;					\
    } STMT_END


/* make some more SVs by adding another arena */

STATIC SV*
S_more_sv(pTHX)
{
    dVAR;
    SV* sv;

    if (PL_nice_chunk) {
	sv_add_arena(PL_nice_chunk, PL_nice_chunk_size, 0);
	PL_nice_chunk = NULL;
        PL_nice_chunk_size = 0;
    }
    else {
	char *chunk;                /* must use New here to match call to */
	Newx(chunk,PERL_ARENA_SIZE,char);  /* Safefree() in sv_free_arenas() */
	sv_add_arena(chunk, PERL_ARENA_SIZE, 0);
    }
    uproot_SV(sv);
    return sv;
}

/* new_SV(): return a new, empty SV head */

#ifdef DEBUG_LEAKING_SCALARS
/* provide a real function for a debugger to play with */
STATIC SV*
S_new_SV(pTHX_ const char *file, int line, const char *func)
{
    SV* sv;

    if (PL_sv_root)
	uproot_SV(sv);
    else
	sv = S_more_sv(aTHX);
    SvANY(sv) = 0;
    SvREFCNT(sv) = 1;
    SvFLAGS(sv) = 0;
    sv->sv_debug_optype = PL_op ? PL_op->op_type : 0;
    sv->sv_debug_line = (U16) (PL_parser && PL_parser->copline != NOLINE
		? PL_parser->copline
		:  PL_curcop
		    ? CopLINE(PL_curcop)
		    : 0
	    );
    sv->sv_debug_inpad = 0;
    sv->sv_debug_cloned = 0;
    sv->sv_debug_file = PL_curcop ? savepv(CopFILE(PL_curcop)): NULL;

    sv->sv_debug_serial = PL_sv_serial++;

    MEM_LOG_NEW_SV(sv, file, line, func);
    DEBUG_m(PerlIO_printf(Perl_debug_log, "0x%"UVxf": (%05ld) new_SV (from %s:%d [%s])\n",
	    PTR2UV(sv), (long)sv->sv_debug_serial, file, line, func));

    return sv;
}
#  define new_SV(p) (p)=S_new_SV(aTHX_ __FILE__, __LINE__, FUNCTION__)

#else
#  define new_SV(p) \
    STMT_START {					\
	if (PL_sv_root)					\
	    uproot_SV(p);				\
	else						\
	    (p) = S_more_sv(aTHX);			\
	SvANY(p) = 0;					\
	SvREFCNT(p) = 1;				\
	SvFLAGS(p) = 0;					\
	MEM_LOG_NEW_SV(p, __FILE__, __LINE__, FUNCTION__);  \
    } STMT_END
#endif


/* del_SV(): return an empty SV head to the free list */

#ifdef DEBUGGING

#define del_SV(p) \
    STMT_START {					\
	if (DEBUG_D_TEST)				\
	    del_sv(p);					\
	else						\
	    plant_SV(p);				\
    } STMT_END

STATIC void
S_del_sv(pTHX_ SV *p)
{
    dVAR;

    PERL_ARGS_ASSERT_DEL_SV;

    if (DEBUG_D_TEST) {
	SV* sva;
	bool ok = 0;
	for (sva = PL_sv_arenaroot; sva; sva = MUTABLE_SV(SvANY(sva))) {
	    const SV * const sv = sva + 1;
	    const SV * const svend = &sva[SvREFCNT(sva)];
	    if (p >= sv && p < svend) {
		ok = 1;
		break;
	    }
	}
	if (!ok) {
	    Perl_ck_warner_d(aTHX_ packWARN(WARN_INTERNAL),
			     "Attempt to free non-arena SV: 0x%"UVxf
			     pTHX__FORMAT, PTR2UV(p) pTHX__VALUE);
	    return;
	}
    }
    plant_SV(p);
}

#else /* ! DEBUGGING */

#define del_SV(p)   plant_SV(p)

#endif /* DEBUGGING */


/*
=head1 SV Manipulation Functions

=for apidoc sv_add_arena

Given a chunk of memory, link it to the head of the list of arenas,
and split it into a list of free SVs.

=cut
*/

static void
S_sv_add_arena(pTHX_ char *const ptr, const U32 size, const U32 flags)
{
    dVAR;
    SV *const sva = MUTABLE_SV(ptr);
    register SV* sv;
    register SV* svend;

    PERL_ARGS_ASSERT_SV_ADD_ARENA;

    /* The first SV in an arena isn't an SV. */
    SvANY(sva) = (void *) PL_sv_arenaroot;		/* ptr to next arena */
    SvREFCNT(sva) = size / sizeof(SV);		/* number of SV slots */
    SvFLAGS(sva) = flags;			/* FAKE if not to be freed */

    PL_sv_arenaroot = sva;
    PL_sv_root = sva + 1;

    svend = &sva[SvREFCNT(sva) - 1];
    sv = sva + 1;
    while (sv < svend) {
	SvARENA_CHAIN_SET(sv, (sv + 1));
#ifdef DEBUGGING
	SvREFCNT(sv) = 0;
#endif
	/* Must always set typemask because it's always checked in on cleanup
	   when the arenas are walked looking for objects.  */
	SvFLAGS(sv) = SVTYPEMASK;
	sv++;
    }
    SvARENA_CHAIN_SET(sv, 0);
#ifdef DEBUGGING
    SvREFCNT(sv) = 0;
#endif
    SvFLAGS(sv) = SVTYPEMASK;
}

/* visit(): call the named function for each non-free SV in the arenas
 * whose flags field matches the flags/mask args. */

STATIC I32
S_visit(pTHX_ SVFUNC_t f, const U32 flags, const U32 mask)
{
    dVAR;
    SV* sva;
    I32 visited = 0;

    PERL_ARGS_ASSERT_VISIT;

    for (sva = PL_sv_arenaroot; sva; sva = MUTABLE_SV(SvANY(sva))) {
	register const SV * const svend = &sva[SvREFCNT(sva)];
	register SV* sv;
	for (sv = sva + 1; sv < svend; ++sv) {
	    if (SvTYPE(sv) != SVTYPEMASK
		    && (sv->sv_flags & mask) == flags
		    && SvREFCNT(sv))
	    {
		(FCALL)(aTHX_ sv);
		++visited;
	    }
	}
    }
    return visited;
}

#ifdef DEBUGGING

/* called by sv_report_used() for each live SV */

static void
do_report_used(pTHX_ SV *const sv)
{
    if (SvTYPE(sv) != SVTYPEMASK) {
	PerlIO_printf(Perl_debug_log, "****\n");
	sv_dump(sv);
    }
}
#endif

/*
=for apidoc sv_report_used

Dump the contents of all SVs not yet freed. (Debugging aid).

=cut
*/

void
Perl_sv_report_used(pTHX)
{
#ifdef DEBUGGING
    visit(do_report_used, 0, 0);
#else
    PERL_UNUSED_CONTEXT;
#endif
}

/* called by sv_clean_objs() for each live SV */

static void
do_clean_objs(pTHX_ SV *const ref)
{
    dVAR;
    assert (SvROK(ref));
    {
	SV * const target = SvRV(ref);
	if (SvOBJECT(target)) {
	    DEBUG_D((PerlIO_printf(Perl_debug_log, "Cleaning object ref:\n "), sv_dump(ref)));
	    if (SvWEAKREF(ref)) {
		sv_del_backref(target, ref);
		SvWEAKREF_off(ref);
		SvRV_set(ref, NULL);
	    } else {
		SvROK_off(ref);
		SvRV_set(ref, NULL);
		SvREFCNT_dec(target);
	    }
	}
    }

    /* XXX Might want to check arrays, etc. */
}

/* called by sv_clean_objs() for each live SV */

#ifndef DISABLE_DESTRUCTOR_KLUDGE
static void
do_clean_named_objs(pTHX_ SV *const sv)
{
    dVAR;
    assert(SvTYPE(sv) == SVt_PVGV);
    assert(isGV_with_GP(sv));
    if (GvGP(sv)) {
	if ((
#ifdef PERL_DONT_CREATE_GVSV
	     GvSV(sv) &&
#endif
	     SvOBJECT(GvSV(sv))) ||
	     (GvAV(sv) && SvOBJECT(GvAV(sv))) ||
	     (GvHV(sv) && SvOBJECT(GvHV(sv))) ||
	     /* In certain rare cases GvIOp(sv) can be NULL, which would make SvOBJECT(GvIO(sv)) dereference NULL. */
	     (GvIO(sv) ? (SvFLAGS(GvIOp(sv)) & SVs_OBJECT) : 0) ||
	     (GvCV(sv) && SvOBJECT(GvCV(sv))) )
	{
	    DEBUG_D((PerlIO_printf(Perl_debug_log, "Cleaning named glob object:\n "), sv_dump(sv)));
	    SvFLAGS(sv) |= SVf_BREAK;
	    SvREFCNT_dec(sv);
	}
    }
}
#endif

/*
=for apidoc sv_clean_objs

Attempt to destroy all objects not yet freed

=cut
*/

void
Perl_sv_clean_objs(pTHX)
{
    dVAR;
    PL_in_clean_objs = TRUE;
    visit(do_clean_objs, SVf_ROK, SVf_ROK);
#ifndef DISABLE_DESTRUCTOR_KLUDGE
    /* some barnacles may yet remain, clinging to typeglobs */
    visit(do_clean_named_objs, SVt_PVGV|SVpgv_GP, SVTYPEMASK|SVp_POK|SVpgv_GP);
#endif
    PL_in_clean_objs = FALSE;
}

/* called by sv_clean_all() for each live SV */

static void
do_clean_all(pTHX_ SV *const sv)
{
    dVAR;
    if (sv == (const SV *) PL_fdpid || sv == (const SV *)PL_strtab) {
	/* don't clean pid table and strtab */
	return;
    }
    DEBUG_D((PerlIO_printf(Perl_debug_log, "Cleaning loops: SV at 0x%"UVxf"\n", PTR2UV(sv)) ));
    SvFLAGS(sv) |= SVf_BREAK;
    SvREFCNT_dec(sv);
}

/*
=for apidoc sv_clean_all

Decrement the refcnt of each remaining SV, possibly triggering a
cleanup. This function may have to be called multiple times to free
SVs which are in complex self-referential hierarchies.

=cut
*/

I32
Perl_sv_clean_all(pTHX)
{
    dVAR;
    I32 cleaned;
    PL_in_clean_all = TRUE;
    cleaned = visit(do_clean_all, 0,0);
    PL_in_clean_all = FALSE;
    return cleaned;
}

/*
  ARENASETS: a meta-arena implementation which separates arena-info
  into struct arena_set, which contains an array of struct
  arena_descs, each holding info for a single arena.  By separating
  the meta-info from the arena, we recover the 1st slot, formerly
  borrowed for list management.  The arena_set is about the size of an
  arena, avoiding the needless malloc overhead of a naive linked-list.

  The cost is 1 arena-set malloc per ~320 arena-mallocs, + the unused
  memory in the last arena-set (1/2 on average).  In trade, we get
  back the 1st slot in each arena (ie 1.7% of a CV-arena, less for
  smaller types).  The recovery of the wasted space allows use of
  small arenas for large, rare body types, by changing array* fields
  in body_details_by_type[] below.
*/
struct arena_desc {
    char       *arena;		/* the raw storage, allocated aligned */
    size_t      size;		/* its size ~4k typ */
    svtype	utype;		/* bodytype stored in arena */
};

struct arena_set;

/* Get the maximum number of elements in set[] such that struct arena_set
   will fit within PERL_ARENA_SIZE, which is probably just under 4K, and
   therefore likely to be 1 aligned memory page.  */

#define ARENAS_PER_SET  ((PERL_ARENA_SIZE - sizeof(struct arena_set*) \
			  - 2 * sizeof(int)) / sizeof (struct arena_desc))

struct arena_set {
    struct arena_set* next;
    unsigned int   set_size;	/* ie ARENAS_PER_SET */
    unsigned int   curr;	/* index of next available arena-desc */
    struct arena_desc set[ARENAS_PER_SET];
};

/*
=for apidoc sv_free_arenas

Deallocate the memory used by all arenas. Note that all the individual SV
heads and bodies within the arenas must already have been freed.

=cut
*/
void
Perl_sv_free_arenas(pTHX)
{
    dVAR;
    SV* sva;
    SV* svanext;
    unsigned int i;

    /* Free arenas here, but be careful about fake ones.  (We assume
       contiguity of the fake ones with the corresponding real ones.) */

    for (sva = PL_sv_arenaroot; sva; sva = svanext) {
	svanext = MUTABLE_SV(SvANY(sva));
	while (svanext && SvFAKE(svanext))
	    svanext = MUTABLE_SV(SvANY(svanext));

	if (!SvFAKE(sva))
	    Safefree(sva);
    }

    {
	struct arena_set *aroot = (struct arena_set*) PL_body_arenas;

	while (aroot) {
	    struct arena_set *current = aroot;
	    i = aroot->curr;
	    while (i--) {
		assert(aroot->set[i].arena);
		Safefree(aroot->set[i].arena);
	    }
	    aroot = aroot->next;
	    Safefree(current);
	}
    }
    PL_body_arenas = 0;

    i = PERL_ARENA_ROOTS_SIZE;
    while (i--)
	PL_body_roots[i] = 0;

    Safefree(PL_nice_chunk);
    PL_nice_chunk = NULL;
    PL_nice_chunk_size = 0;
    PL_sv_arenaroot = 0;
    PL_sv_root = 0;
}

/*
  Here are mid-level routines that manage the allocation of bodies out
  of the various arenas.  There are 5 kinds of arenas:

  1. SV-head arenas, which are discussed and handled above
  2. regular body arenas
  3. arenas for reduced-size bodies
  4. Hash-Entry arenas
  5. pte arenas (thread related)

  Arena types 2 & 3 are chained by body-type off an array of
  arena-root pointers, which is indexed by svtype.  Some of the
  larger/less used body types are malloced singly, since a large
  unused block of them is wasteful.  Also, several svtypes dont have
  bodies; the data fits into the sv-head itself.  The arena-root
  pointer thus has a few unused root-pointers (which may be hijacked
  later for arena types 4,5)

  3 differs from 2 as an optimization; some body types have several
  unused fields in the front of the structure (which are kept in-place
  for consistency).  These bodies can be allocated in smaller chunks,
  because the leading fields arent accessed.  Pointers to such bodies
  are decremented to point at the unused 'ghost' memory, knowing that
  the pointers are used with offsets to the real memory.

  HE, HEK arenas are managed separately, with separate code, but may
  be merge-able later..

  PTE arenas are not sv-bodies, but they share these mid-level
  mechanics, so are considered here.  The new mid-level mechanics rely
  on the sv_type of the body being allocated, so we just reserve one
  of the unused body-slots for PTEs, then use it in those (2) PTE
  contexts below (line ~10k)
*/

/* get_arena(size): this creates custom-sized arenas
   TBD: export properly for hv.c: S_more_he().
*/
void*
Perl_get_arena(pTHX_ const size_t arena_size, const svtype bodytype)
{
    dVAR;
    struct arena_desc* adesc;
    struct arena_set *aroot = (struct arena_set*) PL_body_arenas;
    unsigned int curr;

    /* shouldnt need this
    if (!arena_size)	arena_size = PERL_ARENA_SIZE;
    */

    /* may need new arena-set to hold new arena */
    if (!aroot || aroot->curr >= aroot->set_size) {
	struct arena_set *newroot;
	Newxz(newroot, 1, struct arena_set);
	newroot->set_size = ARENAS_PER_SET;
	newroot->next = aroot;
	aroot = newroot;
	PL_body_arenas = (void *) newroot;
	DEBUG_m(PerlIO_printf(Perl_debug_log, "new arenaset %p\n", (void*)aroot));
    }

    /* ok, now have arena-set with at least 1 empty/available arena-desc */
    curr = aroot->curr++;
    adesc = &(aroot->set[curr]);
    assert(!adesc->arena);
    
    Newx(adesc->arena, arena_size, char);
    adesc->size = arena_size;
    adesc->utype = bodytype;
    DEBUG_m(PerlIO_printf(Perl_debug_log, "arena %d added: %p size %"UVuf"\n", 
			  curr, (void*)adesc->arena, (UV)arena_size));

    return adesc->arena;
}


/* return a thing to the free list */

#define del_body(thing, root)			\
    STMT_START {				\
	void ** const thing_copy = (void **)thing;\
	*thing_copy = *root;			\
	*root = (void*)thing_copy;		\
    } STMT_END

/* 

=head1 SV-Body Allocation

Allocation of SV-bodies is similar to SV-heads, differing as follows;
the allocation mechanism is used for many body types, so is somewhat
more complicated, it uses arena-sets, and has no need for still-live
SV detection.

At the outermost level, (new|del)_X*V macros return bodies of the
appropriate type.  These macros call either (new|del)_body_type or
(new|del)_body_allocated macro pairs, depending on specifics of the
type.  Most body types use the former pair, the latter pair is used to
allocate body types with "ghost fields".

"ghost fields" are fields that are unused in certain types, and
consequently don't need to actually exist.  They are declared because
they're part of a "base type", which allows use of functions as
methods.  The simplest examples are AVs and HVs, 2 aggregate types
which don't use the fields which support SCALAR semantics.

For these types, the arenas are carved up into appropriately sized
chunks, we thus avoid wasted memory for those unaccessed members.
When bodies are allocated, we adjust the pointer back in memory by the
size of the part not allocated, so it's as if we allocated the full
structure.  (But things will all go boom if you write to the part that
is "not there", because you'll be overwriting the last members of the
preceding structure in memory.)

We calculate the correction using the STRUCT_OFFSET macro on the first
member present. If the allocated structure is smaller (no initial NV
actually allocated) then the net effect is to subtract the size of the NV
from the pointer, to return a new pointer as if an initial NV were actually
allocated. (We were using structures named *_allocated for this, but
this turned out to be a subtle bug, because a structure without an NV
could have a lower alignment constraint, but the compiler is allowed to
optimised accesses based on the alignment constraint of the actual pointer
to the full structure, for example, using a single 64 bit load instruction
because it "knows" that two adjacent 32 bit members will be 8-byte aligned.)

This is the same trick as was used for NV and IV bodies. Ironically it
doesn't need to be used for NV bodies any more, because NV is now at
the start of the structure. IV bodies don't need it either, because
they are no longer allocated.

In turn, the new_body_* allocators call S_new_body(), which invokes
new_body_inline macro, which takes a lock, and takes a body off the
linked list at PL_body_roots[sv_type], calling S_more_bodies() if
necessary to refresh an empty list.  Then the lock is released, and
the body is returned.

S_more_bodies calls get_arena(), and carves it up into an array of N
bodies, which it strings into a linked list.  It looks up arena-size
and body-size from the body_details table described below, thus
supporting the multiple body-types.

If PURIFY is defined, or PERL_ARENA_SIZE=0, arenas are not used, and
the (new|del)_X*V macros are mapped directly to malloc/free.

*/

/* 

For each sv-type, struct body_details bodies_by_type[] carries
parameters which control these aspects of SV handling:

Arena_size determines whether arenas are used for this body type, and if
so, how big they are.  PURIFY or PERL_ARENA_SIZE=0 set this field to
zero, forcing individual mallocs and frees.

Body_size determines how big a body is, and therefore how many fit into
each arena.  Offset carries the body-pointer adjustment needed for
"ghost fields", and is used in *_allocated macros.

But its main purpose is to parameterize info needed in
Perl_sv_upgrade().  The info here dramatically simplifies the function
vs the implementation in 5.8.8, making it table-driven.  All fields
are used for this, except for arena_size.

For the sv-types that have no bodies, arenas are not used, so those
PL_body_roots[sv_type] are unused, and can be overloaded.  In
something of a special case, SVt_NULL is borrowed for HE arenas;
PL_body_roots[HE_SVSLOT=SVt_NULL] is filled by S_more_he, but the
bodies_by_type[SVt_NULL] slot is not used, as the table is not
available in hv.c.

PTEs also use arenas, but are never seen in Perl_sv_upgrade. Nonetheless,
they get their own slot in bodies_by_type[PTE_SVSLOT =SVt_IV], so they can
just use the same allocation semantics.  At first, PTEs were also
overloaded to a non-body sv-type, but this yielded hard-to-find malloc
bugs, so was simplified by claiming a new slot.  This choice has no
consequence at this time.

*/

struct body_details {
    U8 body_size;	/* Size to allocate  */
    U8 copy;		/* Size of structure to copy (may be shorter)  */
    U8 offset;
    unsigned int type : 4;	    /* We have space for a sanity check.  */
    unsigned int cant_upgrade : 1;  /* Cannot upgrade this type */
    unsigned int zero_nv : 1;	    /* zero the NV when upgrading from this */
    unsigned int arena : 1;	    /* Allocated from an arena */
    size_t arena_size;		    /* Size of arena to allocate */
};

#define HADNV FALSE
#define NONV TRUE


#ifdef PURIFY
/* With -DPURFIY we allocate everything directly, and don't use arenas.
   This seems a rather elegant way to simplify some of the code below.  */
#define HASARENA FALSE
#else
#define HASARENA TRUE
#endif
#define NOARENA FALSE

/* Size the arenas to exactly fit a given number of bodies.  A count
   of 0 fits the max number bodies into a PERL_ARENA_SIZE.block,
   simplifying the default.  If count > 0, the arena is sized to fit
   only that many bodies, allowing arenas to be used for large, rare
   bodies (XPVFM, XPVIO) without undue waste.  The arena size is
   limited by PERL_ARENA_SIZE, so we can safely oversize the
   declarations.
 */
#define FIT_ARENA0(body_size)				\
    ((size_t)(PERL_ARENA_SIZE / body_size) * body_size)
#define FIT_ARENAn(count,body_size)			\
    ( count * body_size <= PERL_ARENA_SIZE)		\
    ? count * body_size					\
    : FIT_ARENA0 (body_size)
#define FIT_ARENA(count,body_size)			\
    count 						\
    ? FIT_ARENAn (count, body_size)			\
    : FIT_ARENA0 (body_size)

/* Calculate the length to copy. Specifically work out the length less any
   final padding the compiler needed to add.  See the comment in sv_upgrade
   for why copying the padding proved to be a bug.  */

#define copy_length(type, last_member) \
	STRUCT_OFFSET(type, last_member) \
	+ sizeof (((type*)SvANY((const SV *)0))->last_member)

static const struct body_details bodies_by_type[] = {
    { sizeof(HE), 0, 0, SVt_NULL,
      FALSE, NONV, NOARENA, FIT_ARENA(0, sizeof(HE)) },

    /* The bind placeholder pretends to be an RV for now.
       Also it's marked as "can't upgrade" to stop anyone using it before it's
       implemented.  */
    { 0, 0, 0, SVt_BIND, TRUE, NONV, NOARENA, 0 },

    /* IVs are in the head, so the allocation size is 0.
       However, the slot is overloaded for PTEs.  */
    { sizeof(struct ptr_tbl_ent), /* This is used for PTEs.  */
      sizeof(IV), /* This is used to copy out the IV body.  */
      STRUCT_OFFSET(XPVIV, xiv_iv), SVt_IV, FALSE, NONV,
      NOARENA /* IVS don't need an arena  */,
      /* But PTEs need to know the size of their arena  */
      FIT_ARENA(0, sizeof(struct ptr_tbl_ent))
    },

    /* 8 bytes on most ILP32 with IEEE doubles */
    { sizeof(NV), sizeof(NV), 0, SVt_NV, FALSE, HADNV, HASARENA,
      FIT_ARENA(0, sizeof(NV)) },

    /* 8 bytes on most ILP32 with IEEE doubles */
    { sizeof(XPV) - STRUCT_OFFSET(XPV, xpv_cur),
      copy_length(XPV, xpv_len) - STRUCT_OFFSET(XPV, xpv_cur),
      + STRUCT_OFFSET(XPV, xpv_cur),
      SVt_PV, FALSE, NONV, HASARENA,
      FIT_ARENA(0, sizeof(XPV) - STRUCT_OFFSET(XPV, xpv_cur)) },

    /* 12 */
    { sizeof(XPVIV) - STRUCT_OFFSET(XPV, xpv_cur),
      copy_length(XPVIV, xiv_u) - STRUCT_OFFSET(XPV, xpv_cur),
      + STRUCT_OFFSET(XPVIV, xpv_cur),
      SVt_PVIV, FALSE, NONV, HASARENA,
      FIT_ARENA(0, sizeof(XPV) - STRUCT_OFFSET(XPV, xpv_cur)) },

    /* 20 */
    { sizeof(XPVNV), copy_length(XPVNV, xiv_u), 0, SVt_PVNV, FALSE, HADNV,
      HASARENA, FIT_ARENA(0, sizeof(XPVNV)) },

    /* 28 */
    { sizeof(XPVMG), copy_length(XPVMG, xmg_stash), 0, SVt_PVMG, FALSE, HADNV,
      HASARENA, FIT_ARENA(0, sizeof(XPVMG)) },

    /* something big */
    { sizeof(regexp) - STRUCT_OFFSET(regexp, xpv_cur),
      sizeof(regexp) - STRUCT_OFFSET(regexp, xpv_cur),
      + STRUCT_OFFSET(regexp, xpv_cur),
      SVt_REGEXP, FALSE, NONV, HASARENA,
      FIT_ARENA(0, sizeof(regexp) - STRUCT_OFFSET(regexp, xpv_cur))
    },

    /* 48 */
    { sizeof(XPVGV), sizeof(XPVGV), 0, SVt_PVGV, TRUE, HADNV,
      HASARENA, FIT_ARENA(0, sizeof(XPVGV)) },
    
    /* 64 */
    { sizeof(XPVLV), sizeof(XPVLV), 0, SVt_PVLV, TRUE, HADNV,
      HASARENA, FIT_ARENA(0, sizeof(XPVLV)) },

    { sizeof(XPVAV) - STRUCT_OFFSET(XPVAV, xav_fill),
      copy_length(XPVAV, xmg_stash) - STRUCT_OFFSET(XPVAV, xav_fill),
      + STRUCT_OFFSET(XPVAV, xav_fill),
      SVt_PVAV, TRUE, NONV, HASARENA,
      FIT_ARENA(0, sizeof(XPVAV) - STRUCT_OFFSET(XPVAV, xav_fill)) },

    { sizeof(XPVHV) - STRUCT_OFFSET(XPVHV, xhv_fill),
      copy_length(XPVHV, xmg_stash) - STRUCT_OFFSET(XPVHV, xhv_fill),
      + STRUCT_OFFSET(XPVHV, xhv_fill),
      SVt_PVHV, TRUE, NONV, HASARENA,
      FIT_ARENA(0, sizeof(XPVHV) - STRUCT_OFFSET(XPVHV, xhv_fill)) },

    /* 56 */
    { sizeof(XPVCV) - STRUCT_OFFSET(XPVCV, xpv_cur),
      sizeof(XPVCV) - STRUCT_OFFSET(XPVCV, xpv_cur),
      + STRUCT_OFFSET(XPVCV, xpv_cur),
      SVt_PVCV, TRUE, NONV, HASARENA,
      FIT_ARENA(0, sizeof(XPVCV) - STRUCT_OFFSET(XPVCV, xpv_cur)) },

    { sizeof(XPVFM) - STRUCT_OFFSET(XPVFM, xpv_cur),
      sizeof(XPVFM) - STRUCT_OFFSET(XPVFM, xpv_cur),
      + STRUCT_OFFSET(XPVFM, xpv_cur),
      SVt_PVFM, TRUE, NONV, NOARENA,
      FIT_ARENA(20, sizeof(XPVFM) - STRUCT_OFFSET(XPVFM, xpv_cur)) },

    /* XPVIO is 84 bytes, fits 48x */
    { sizeof(XPVIO) - STRUCT_OFFSET(XPVIO, xpv_cur),
      sizeof(XPVIO) - STRUCT_OFFSET(XPVIO, xpv_cur),
      + STRUCT_OFFSET(XPVIO, xpv_cur),
      SVt_PVIO, TRUE, NONV, HASARENA,
      FIT_ARENA(24, sizeof(XPVIO) - STRUCT_OFFSET(XPVIO, xpv_cur)) },
};

#define new_body_type(sv_type)		\
    (void *)((char *)S_new_body(aTHX_ sv_type))

#define del_body_type(p, sv_type)	\
    del_body(p, &PL_body_roots[sv_type])


#define new_body_allocated(sv_type)		\
    (void *)((char *)S_new_body(aTHX_ sv_type)	\
	     - bodies_by_type[sv_type].offset)

#define del_body_allocated(p, sv_type)		\
    del_body(p + bodies_by_type[sv_type].offset, &PL_body_roots[sv_type])


#define my_safemalloc(s)	(void*)safemalloc(s)
#define my_safecalloc(s)	(void*)safecalloc(s, 1)
#define my_safefree(p)	safefree((char*)p)

#ifdef PURIFY

#define new_XNV()	my_safemalloc(sizeof(XPVNV))
#define del_XNV(p)	my_safefree(p)

#define new_XPVNV()	my_safemalloc(sizeof(XPVNV))
#define del_XPVNV(p)	my_safefree(p)

#define new_XPVAV()	my_safemalloc(sizeof(XPVAV))
#define del_XPVAV(p)	my_safefree(p)

#define new_XPVHV()	my_safemalloc(sizeof(XPVHV))
#define del_XPVHV(p)	my_safefree(p)

#define new_XPVMG()	my_safemalloc(sizeof(XPVMG))
#define del_XPVMG(p)	my_safefree(p)

#define new_XPVGV()	my_safemalloc(sizeof(XPVGV))
#define del_XPVGV(p)	my_safefree(p)

#else /* !PURIFY */

#define new_XNV()	new_body_type(SVt_NV)
#define del_XNV(p)	del_body_type(p, SVt_NV)

#define new_XPVNV()	new_body_type(SVt_PVNV)
#define del_XPVNV(p)	del_body_type(p, SVt_PVNV)

#define new_XPVAV()	new_body_allocated(SVt_PVAV)
#define del_XPVAV(p)	del_body_allocated(p, SVt_PVAV)

#define new_XPVHV()	new_body_allocated(SVt_PVHV)
#define del_XPVHV(p)	del_body_allocated(p, SVt_PVHV)

#define new_XPVMG()	new_body_type(SVt_PVMG)
#define del_XPVMG(p)	del_body_type(p, SVt_PVMG)

#define new_XPVGV()	new_body_type(SVt_PVGV)
#define del_XPVGV(p)	del_body_type(p, SVt_PVGV)

#endif /* PURIFY */

/* no arena for you! */

#define new_NOARENA(details) \
	my_safemalloc((details)->body_size + (details)->offset)
#define new_NOARENAZ(details) \
	my_safecalloc((details)->body_size + (details)->offset)

STATIC void *
S_more_bodies (pTHX_ const svtype sv_type)
{
    dVAR;
    void ** const root = &PL_body_roots[sv_type];
    const struct body_details * const bdp = &bodies_by_type[sv_type];
    const size_t body_size = bdp->body_size;
    char *start;
    const char *end;
    const size_t arena_size = Perl_malloc_good_size(bdp->arena_size);
#if defined(DEBUGGING) && !defined(PERL_GLOBAL_STRUCT_PRIVATE)
    static bool done_sanity_check;

    /* PERL_GLOBAL_STRUCT_PRIVATE cannot coexist with global
     * variables like done_sanity_check. */
    if (!done_sanity_check) {
	unsigned int i = SVt_LAST;

	done_sanity_check = TRUE;

	while (i--)
	    assert (bodies_by_type[i].type == i);
    }
#endif

    assert(bdp->arena_size);

    start = (char*) Perl_get_arena(aTHX_ arena_size, sv_type);

    end = start + arena_size - 2 * body_size;

    /* computed count doesnt reflect the 1st slot reservation */
#if defined(MYMALLOC) || defined(HAS_MALLOC_GOOD_SIZE)
    DEBUG_m(PerlIO_printf(Perl_debug_log,
			  "arena %p end %p arena-size %d (from %d) type %d "
			  "size %d ct %d\n",
			  (void*)start, (void*)end, (int)arena_size,
			  (int)bdp->arena_size, sv_type, (int)body_size,
			  (int)arena_size / (int)body_size));
#else
    DEBUG_m(PerlIO_printf(Perl_debug_log,
			  "arena %p end %p arena-size %d type %d size %d ct %d\n",
			  (void*)start, (void*)end,
			  (int)bdp->arena_size, sv_type, (int)body_size,
			  (int)bdp->arena_size / (int)body_size));
#endif
    *root = (void *)start;

    while (start <= end) {
	char * const next = start + body_size;
	*(void**) start = (void *)next;
	start = next;
    }
    *(void **)start = 0;

    return *root;
}

/* grab a new thing from the free list, allocating more if necessary.
   The inline version is used for speed in hot routines, and the
   function using it serves the rest (unless PURIFY).
*/
#define new_body_inline(xpv, sv_type) \
    STMT_START { \
	void ** const r3wt = &PL_body_roots[sv_type]; \
	xpv = (PTR_TBL_ENT_t*) (*((void **)(r3wt))      \
	  ? *((void **)(r3wt)) : more_bodies(sv_type)); \
	*(r3wt) = *(void**)(xpv); \
    } STMT_END

#ifndef PURIFY

STATIC void *
S_new_body(pTHX_ const svtype sv_type)
{
    dVAR;
    void *xpv;
    new_body_inline(xpv, sv_type);
    return xpv;
}

#endif

static const struct body_details fake_rv =
    { 0, 0, 0, SVt_IV, FALSE, NONV, NOARENA, 0 };

/*
=for apidoc sv_upgrade

Upgrade an SV to a more complex form.  Generally adds a new body type to the
SV, then copies across as much information as possible from the old body.
You generally want to use the C<SvUPGRADE> macro wrapper. See also C<svtype>.

=cut
*/

void
Perl_sv_upgrade(pTHX_ register SV *const sv, svtype new_type)
{
    dVAR;
    void*	old_body;
    void*	new_body;
    const svtype old_type = SvTYPE(sv);
    const struct body_details *new_type_details;
    const struct body_details *old_type_details
	= bodies_by_type + old_type;
    SV *referant = NULL;

    PERL_ARGS_ASSERT_SV_UPGRADE;

    if (old_type == new_type)
	return;

    /* This clause was purposefully added ahead of the early return above to
       the shared string hackery for (sort {$a <=> $b} keys %hash), with the
       inference by Nick I-S that it would fix other troublesome cases. See
       changes 7162, 7163 (f130fd4589cf5fbb24149cd4db4137c8326f49c1 and parent)

       Given that shared hash key scalars are no longer PVIV, but PV, there is
       no longer need to unshare so as to free up the IVX slot for its proper
       purpose. So it's safe to move the early return earlier.  */

    if (new_type != SVt_PV && SvIsCOW(sv)) {
	sv_force_normal_flags(sv, 0);
    }

    old_body = SvANY(sv);

    /* Copying structures onto other structures that have been neatly zeroed
       has a subtle gotcha. Consider XPVMG

       +------+------+------+------+------+-------+-------+
       |     NV      | CUR  | LEN  |  IV  | MAGIC | STASH |
       +------+------+------+------+------+-------+-------+
       0      4      8     12     16     20      24      28

       where NVs are aligned to 8 bytes, so that sizeof that structure is
       actually 32 bytes long, with 4 bytes of padding at the end:

       +------+------+------+------+------+-------+-------+------+
       |     NV      | CUR  | LEN  |  IV  | MAGIC | STASH | ???  |
       +------+------+------+------+------+-------+-------+------+
       0      4      8     12     16     20      24      28     32

       so what happens if you allocate memory for this structure:

       +------+------+------+------+------+-------+-------+------+------+...
       |     NV      | CUR  | LEN  |  IV  | MAGIC | STASH |  GP  | NAME |
       +------+------+------+------+------+-------+-------+------+------+...
       0      4      8     12     16     20      24      28     32     36

       zero it, then copy sizeof(XPVMG) bytes on top of it? Not quite what you
       expect, because you copy the area marked ??? onto GP. Now, ??? may have
       started out as zero once, but it's quite possible that it isn't. So now,
       rather than a nicely zeroed GP, you have it pointing somewhere random.
       Bugs ensue.

       (In fact, GP ends up pointing at a previous GP structure, because the
       principle cause of the padding in XPVMG getting garbage is a copy of
       sizeof(XPVMG) bytes from a XPVGV structure in sv_unglob. Right now
       this happens to be moot because XPVGV has been re-ordered, with GP
       no longer after STASH)

       So we are careful and work out the size of used parts of all the
       structures.  */

    switch (old_type) {
    case SVt_NULL:
	break;
    case SVt_IV:
	if (SvROK(sv)) {
	    referant = SvRV(sv);
	    old_type_details = &fake_rv;
	    if (new_type == SVt_NV)
		new_type = SVt_PVNV;
	} else {
	    if (new_type < SVt_PVIV) {
		new_type = (new_type == SVt_NV)
		    ? SVt_PVNV : SVt_PVIV;
	    }
	}
	break;
    case SVt_NV:
	if (new_type < SVt_PVNV) {
	    new_type = SVt_PVNV;
	}
	break;
    case SVt_PV:
	assert(new_type > SVt_PV);
	assert(SVt_IV < SVt_PV);
	assert(SVt_NV < SVt_PV);
	break;
    case SVt_PVIV:
	break;
    case SVt_PVNV:
	break;
    case SVt_PVMG:
	/* Because the XPVMG of PL_mess_sv isn't allocated from the arena,
	   there's no way that it can be safely upgraded, because perl.c
	   expects to Safefree(SvANY(PL_mess_sv))  */
	assert(sv != PL_mess_sv);
	/* This flag bit is used to mean other things in other scalar types.
	   Given that it only has meaning inside the pad, it shouldn't be set
	   on anything that can get upgraded.  */
	assert(!SvPAD_TYPED(sv));
	break;
    default:
	if (old_type_details->cant_upgrade)
	    Perl_croak(aTHX_ "Can't upgrade %s (%" UVuf ") to %" UVuf,
		       sv_reftype(sv, 0), (UV) old_type, (UV) new_type);
    }

    if (old_type > new_type)
	Perl_croak(aTHX_ "sv_upgrade from type %d down to type %d",
		(int)old_type, (int)new_type);

    new_type_details = bodies_by_type + new_type;

    SvFLAGS(sv) &= ~SVTYPEMASK;
    SvFLAGS(sv) |= new_type;

    /* This can't happen, as SVt_NULL is <= all values of new_type, so one of
       the return statements above will have triggered.  */
    assert (new_type != SVt_NULL);
    switch (new_type) {
    case SVt_IV:
	assert(old_type == SVt_NULL);
	SvANY(sv) = (XPVIV*)((char*)&(sv->sv_u.svu_iv) - STRUCT_OFFSET(XPVIV, xiv_iv));
	SvIV_set(sv, 0);
	return;
    case SVt_NV:
	assert(old_type == SVt_NULL);
	SvANY(sv) = new_XNV();
	SvNV_set(sv, 0);
	return;
    case SVt_PVHV:
    case SVt_PVAV:
	assert(new_type_details->body_size);

#ifndef PURIFY	
	assert(new_type_details->arena);
	assert(new_type_details->arena_size);
	/* This points to the start of the allocated area.  */
	new_body_inline(new_body, new_type);
	Zero(new_body, new_type_details->body_size, char);
	new_body = ((char *)new_body) - new_type_details->offset;
#else
	/* We always allocated the full length item with PURIFY. To do this
	   we fake things so that arena is false for all 16 types..  */
	new_body = new_NOARENAZ(new_type_details);
#endif
	SvANY(sv) = new_body;
	if (new_type == SVt_PVAV) {
	    AvMAX(sv)	= -1;
	    AvFILLp(sv)	= -1;
	    AvREAL_only(sv);
	    if (old_type_details->body_size) {
		AvALLOC(sv) = 0;
	    } else {
		/* It will have been zeroed when the new body was allocated.
		   Lets not write to it, in case it confuses a write-back
		   cache.  */
	    }
	} else {
	    assert(!SvOK(sv));
	    SvOK_off(sv);
#ifndef NODEFAULT_SHAREKEYS
	    HvSHAREKEYS_on(sv);         /* key-sharing on by default */
#endif
	    HvMAX(sv) = 7; /* (start with 8 buckets) */
	    if (old_type_details->body_size) {
		HvFILL(sv) = 0;
	    } else {
		/* It will have been zeroed when the new body was allocated.
		   Lets not write to it, in case it confuses a write-back
		   cache.  */
	    }
	}

	/* SVt_NULL isn't the only thing upgraded to AV or HV.
	   The target created by newSVrv also is, and it can have magic.
	   However, it never has SvPVX set.
	*/
	if (old_type == SVt_IV) {
	    assert(!SvROK(sv));
	} else if (old_type >= SVt_PV) {
	    assert(SvPVX_const(sv) == 0);
	}

	if (old_type >= SVt_PVMG) {
	    SvMAGIC_set(sv, ((XPVMG*)old_body)->xmg_u.xmg_magic);
	    SvSTASH_set(sv, ((XPVMG*)old_body)->xmg_stash);
	} else {
	    sv->sv_u.svu_array = NULL; /* or svu_hash  */
	}
	break;


    case SVt_REGEXP:
	/* This ensures that SvTHINKFIRST(sv) is true, and hence that
	   sv_force_normal_flags(sv) is called.  */
	SvFAKE_on(sv);
    case SVt_PVIV:
	/* XXX Is this still needed?  Was it ever needed?   Surely as there is
	   no route from NV to PVIV, NOK can never be true  */
	assert(!SvNOKp(sv));
	assert(!SvNOK(sv));
    case SVt_PVIO:
    case SVt_PVFM:
    case SVt_PVGV:
    case SVt_PVCV:
    case SVt_PVLV:
    case SVt_PVMG:
    case SVt_PVNV:
    case SVt_PV:

	assert(new_type_details->body_size);
	/* We always allocated the full length item with PURIFY. To do this
	   we fake things so that arena is false for all 16 types..  */
	if(new_type_details->arena) {
	    /* This points to the start of the allocated area.  */
	    new_body_inline(new_body, new_type);
	    Zero(new_body, new_type_details->body_size, char);
	    new_body = ((char *)new_body) - new_type_details->offset;
	} else {
	    new_body = new_NOARENAZ(new_type_details);
	}
	SvANY(sv) = new_body;

	if (old_type_details->copy) {
	    /* There is now the potential for an upgrade from something without
	       an offset (PVNV or PVMG) to something with one (PVCV, PVFM)  */
	    int offset = old_type_details->offset;
	    int length = old_type_details->copy;

	    if (new_type_details->offset > old_type_details->offset) {
		const int difference
		    = new_type_details->offset - old_type_details->offset;
		offset += difference;
		length -= difference;
	    }
	    assert (length >= 0);
		
	    Copy((char *)old_body + offset, (char *)new_body + offset, length,
		 char);
	}

#ifndef NV_ZERO_IS_ALLBITS_ZERO
	/* If NV 0.0 is stores as all bits 0 then Zero() already creates a
	 * correct 0.0 for us.  Otherwise, if the old body didn't have an
	 * NV slot, but the new one does, then we need to initialise the
	 * freshly created NV slot with whatever the correct bit pattern is
	 * for 0.0  */
	if (old_type_details->zero_nv && !new_type_details->zero_nv
	    && !isGV_with_GP(sv))
	    SvNV_set(sv, 0);
#endif

	if (new_type == SVt_PVIO) {
	    IO * const io = MUTABLE_IO(sv);
	    GV *iogv = gv_fetchpvs("IO::File::", GV_ADD, SVt_PVHV);

	    SvOBJECT_on(io);
	    /* Clear the stashcache because a new IO could overrule a package
	       name */
	    hv_clear(PL_stashcache);

	    SvSTASH_set(io, MUTABLE_HV(SvREFCNT_inc(GvHV(iogv))));
	    IoPAGE_LEN(sv) = 60;
	}
	if (old_type < SVt_PV) {
	    /* referant will be NULL unless the old type was SVt_IV emulating
	       SVt_RV */
	    sv->sv_u.svu_rv = referant;
	}
	break;
    default:
	Perl_croak(aTHX_ "panic: sv_upgrade to unknown type %lu",
		   (unsigned long)new_type);
    }

    if (old_type > SVt_IV) { /* SVt_IVs are overloaded for PTEs */
#ifdef PURIFY
	my_safefree(old_body);
#else
	/* Note that there is an assumption that all bodies of types that
	   can be upgraded came from arenas. Only the more complex non-
	   upgradable types are allowed to be directly malloc()ed.  */
	assert(old_type_details->arena);
	del_body((void*)((char*)old_body + old_type_details->offset),
		 &PL_body_roots[old_type]);
#endif
    }
}

/*
=for apidoc sv_backoff

Remove any string offset. You should normally use the C<SvOOK_off> macro
wrapper instead.

=cut
*/

int
Perl_sv_backoff(pTHX_ register SV *const sv)
{
    STRLEN delta;
    const char * const s = SvPVX_const(sv);

    PERL_ARGS_ASSERT_SV_BACKOFF;
    PERL_UNUSED_CONTEXT;

    assert(SvOOK(sv));
    assert(SvTYPE(sv) != SVt_PVHV);
    assert(SvTYPE(sv) != SVt_PVAV);

    SvOOK_offset(sv, delta);
    
    SvLEN_set(sv, SvLEN(sv) + delta);
    SvPV_set(sv, SvPVX(sv) - delta);
    Move(s, SvPVX(sv), SvCUR(sv)+1, char);
    SvFLAGS(sv) &= ~SVf_OOK;
    return 0;
}

/*
=for apidoc sv_grow

Expands the character buffer in the SV.  If necessary, uses C<sv_unref> and
upgrades the SV to C<SVt_PV>.  Returns a pointer to the character buffer.
Use the C<SvGROW> wrapper instead.

=cut
*/

char *
Perl_sv_grow(pTHX_ register SV *const sv, register STRLEN newlen)
{
    register char *s;

    PERL_ARGS_ASSERT_SV_GROW;

    if (PL_madskills && newlen >= 0x100000) {
	PerlIO_printf(Perl_debug_log,
		      "Allocation too large: %"UVxf"\n", (UV)newlen);
    }
#ifdef HAS_64K_LIMIT
    if (newlen >= 0x10000) {
	PerlIO_printf(Perl_debug_log,
		      "Allocation too large: %"UVxf"\n", (UV)newlen);
	my_exit(1);
    }
#endif /* HAS_64K_LIMIT */
    if (SvROK(sv))
	sv_unref(sv);
    if (SvTYPE(sv) < SVt_PV) {
	sv_upgrade(sv, SVt_PV);
	s = SvPVX_mutable(sv);
    }
    else if (SvOOK(sv)) {	/* pv is offset? */
	sv_backoff(sv);
	s = SvPVX_mutable(sv);
	if (newlen > SvLEN(sv))
	    newlen += 10 * (newlen - SvCUR(sv)); /* avoid copy each time */
#ifdef HAS_64K_LIMIT
	if (newlen >= 0x10000)
	    newlen = 0xFFFF;
#endif
    }
    else
	s = SvPVX_mutable(sv);

    if (newlen > SvLEN(sv)) {		/* need more room? */
#ifndef Perl_safesysmalloc_size
	newlen = PERL_STRLEN_ROUNDUP(newlen);
#endif
	if (SvLEN(sv) && s) {
	    s = (char*)saferealloc(s, newlen);
	}
	else {
	    s = (char*)safemalloc(newlen);
	    if (SvPVX_const(sv) && SvCUR(sv)) {
	        Move(SvPVX_const(sv), s, (newlen < SvCUR(sv)) ? newlen : SvCUR(sv), char);
	    }
	}
	SvPV_set(sv, s);
#ifdef Perl_safesysmalloc_size
	/* Do this here, do it once, do it right, and then we will never get
	   called back into sv_grow() unless there really is some growing
	   needed.  */
	SvLEN_set(sv, Perl_safesysmalloc_size(s));
#else
        SvLEN_set(sv, newlen);
#endif
    }
    return s;
}

/*
=for apidoc sv_setiv

Copies an integer into the given SV, upgrading first if necessary.
Does not handle 'set' magic.  See also C<sv_setiv_mg>.

=cut
*/

void
Perl_sv_setiv(pTHX_ register SV *const sv, const IV i)
{
    dVAR;

    PERL_ARGS_ASSERT_SV_SETIV;

    SV_CHECK_THINKFIRST_COW_DROP(sv);
    switch (SvTYPE(sv)) {
    case SVt_NULL:
    case SVt_NV:
	sv_upgrade(sv, SVt_IV);
	break;
    case SVt_PV:
	sv_upgrade(sv, SVt_PVIV);
	break;

    case SVt_PVGV:
	if (!isGV_with_GP(sv))
	    break;
    case SVt_PVAV:
    case SVt_PVHV:
    case SVt_PVCV:
    case SVt_PVFM:
    case SVt_PVIO:
	Perl_croak(aTHX_ "Can't coerce %s to integer in %s", sv_reftype(sv,0),
		   OP_DESC(PL_op));
    default: NOOP;
    }
    (void)SvIOK_only(sv);			/* validate number */
    SvIV_set(sv, i);
    SvTAINT(sv);
}

/*
=for apidoc sv_setiv_mg

Like C<sv_setiv>, but also handles 'set' magic.

=cut
*/

void
Perl_sv_setiv_mg(pTHX_ register SV *const sv, const IV i)
{
    PERL_ARGS_ASSERT_SV_SETIV_MG;

    sv_setiv(sv,i);
    SvSETMAGIC(sv);
}

/*
=for apidoc sv_setuv

Copies an unsigned integer into the given SV, upgrading first if necessary.
Does not handle 'set' magic.  See also C<sv_setuv_mg>.

=cut
*/

void
Perl_sv_setuv(pTHX_ register SV *const sv, const UV u)
{
    PERL_ARGS_ASSERT_SV_SETUV;

    /* With these two if statements:
       u=1.49  s=0.52  cu=72.49  cs=10.64  scripts=270  tests=20865

       without
       u=1.35  s=0.47  cu=73.45  cs=11.43  scripts=270  tests=20865

       If you wish to remove them, please benchmark to see what the effect is
    */
    if (u <= (UV)IV_MAX) {
       sv_setiv(sv, (IV)u);
       return;
    }
    sv_setiv(sv, 0);
    SvIsUV_on(sv);
    SvUV_set(sv, u);
}

/*
=for apidoc sv_setuv_mg

Like C<sv_setuv>, but also handles 'set' magic.

=cut
*/

void
Perl_sv_setuv_mg(pTHX_ register SV *const sv, const UV u)
{
    PERL_ARGS_ASSERT_SV_SETUV_MG;

    sv_setuv(sv,u);
    SvSETMAGIC(sv);
}

/*
=for apidoc sv_setnv

Copies a double into the given SV, upgrading first if necessary.
Does not handle 'set' magic.  See also C<sv_setnv_mg>.

=cut
*/

void
Perl_sv_setnv(pTHX_ register SV *const sv, const NV num)
{
    dVAR;

    PERL_ARGS_ASSERT_SV_SETNV;

    SV_CHECK_THINKFIRST_COW_DROP(sv);
    switch (SvTYPE(sv)) {
    case SVt_NULL:
    case SVt_IV:
	sv_upgrade(sv, SVt_NV);
	break;
    case SVt_PV:
    case SVt_PVIV:
	sv_upgrade(sv, SVt_PVNV);
	break;

    case SVt_PVGV:
	if (!isGV_with_GP(sv))
	    break;
    case SVt_PVAV:
    case SVt_PVHV:
    case SVt_PVCV:
    case SVt_PVFM:
    case SVt_PVIO:
	Perl_croak(aTHX_ "Can't coerce %s to number in %s", sv_reftype(sv,0),
		   OP_NAME(PL_op));
    default: NOOP;
    }
    SvNV_set(sv, num);
    (void)SvNOK_only(sv);			/* validate number */
    SvTAINT(sv);
}

/*
=for apidoc sv_setnv_mg

Like C<sv_setnv>, but also handles 'set' magic.

=cut
*/

void
Perl_sv_setnv_mg(pTHX_ register SV *const sv, const NV num)
{
    PERL_ARGS_ASSERT_SV_SETNV_MG;

    sv_setnv(sv,num);
    SvSETMAGIC(sv);
}

/* Print an "isn't numeric" warning, using a cleaned-up,
 * printable version of the offending string
 */

STATIC void
S_not_a_number(pTHX_ SV *const sv)
{
     dVAR;
     SV *dsv;
     char tmpbuf[64];
     const char *pv;

     PERL_ARGS_ASSERT_NOT_A_NUMBER;

     if (DO_UTF8(sv)) {
          dsv = newSVpvs_flags("", SVs_TEMP);
          pv = sv_uni_display(dsv, sv, 10, 0);
     } else {
	  char *d = tmpbuf;
	  const char * const limit = tmpbuf + sizeof(tmpbuf) - 8;
	  /* each *s can expand to 4 chars + "...\0",
	     i.e. need room for 8 chars */
	
	  const char *s = SvPVX_const(sv);
	  const char * const end = s + SvCUR(sv);
	  for ( ; s < end && d < limit; s++ ) {
	       int ch = *s & 0xFF;
	       if (ch & 128 && !isPRINT_LC(ch)) {
		    *d++ = 'M';
		    *d++ = '-';
		    ch &= 127;
	       }
	       if (ch == '\n') {
		    *d++ = '\\';
		    *d++ = 'n';
	       }
	       else if (ch == '\r') {
		    *d++ = '\\';
		    *d++ = 'r';
	       }
	       else if (ch == '\f') {
		    *d++ = '\\';
		    *d++ = 'f';
	       }
	       else if (ch == '\\') {
		    *d++ = '\\';
		    *d++ = '\\';
	       }
	       else if (ch == '\0') {
		    *d++ = '\\';
		    *d++ = '0';
	       }
	       else if (isPRINT_LC(ch))
		    *d++ = ch;
	       else {
		    *d++ = '^';
		    *d++ = toCTRL(ch);
	       }
	  }
	  if (s < end) {
	       *d++ = '.';
	       *d++ = '.';
	       *d++ = '.';
	  }
	  *d = '\0';
	  pv = tmpbuf;
    }

    if (PL_op)
	Perl_warner(aTHX_ packWARN(WARN_NUMERIC),
		    "Argument \"%s\" isn't numeric in %s", pv,
		    OP_DESC(PL_op));
    else
	Perl_warner(aTHX_ packWARN(WARN_NUMERIC),
		    "Argument \"%s\" isn't numeric", pv);
}

/*
=for apidoc looks_like_number

Test if the content of an SV looks like a number (or is a number).
C<Inf> and C<Infinity> are treated as numbers (so will not issue a
non-numeric warning), even if your atof() doesn't grok them.

=cut
*/

I32
Perl_looks_like_number(pTHX_ SV *const sv)
{
    register const char *sbegin;
    STRLEN len;

    PERL_ARGS_ASSERT_LOOKS_LIKE_NUMBER;

    if (SvPOK(sv)) {
	sbegin = SvPVX_const(sv);
	len = SvCUR(sv);
    }
    else if (SvPOKp(sv))
	sbegin = SvPV_const(sv, len);
    else
	return SvFLAGS(sv) & (SVf_NOK|SVp_NOK|SVf_IOK|SVp_IOK);
    return grok_number(sbegin, len, NULL);
}

STATIC bool
S_glob_2number(pTHX_ GV * const gv)
{
    const U32 wasfake = SvFLAGS(gv) & SVf_FAKE;
    SV *const buffer = sv_newmortal();

    PERL_ARGS_ASSERT_GLOB_2NUMBER;

    /* FAKE globs can get coerced, so need to turn this off temporarily if it
       is on.  */
    SvFAKE_off(gv);
    gv_efullname3(buffer, gv, "*");
    SvFLAGS(gv) |= wasfake;

    /* We know that all GVs stringify to something that is not-a-number,
	so no need to test that.  */
    if (ckWARN(WARN_NUMERIC))
	not_a_number(buffer);
    /* We just want something true to return, so that S_sv_2iuv_common
	can tail call us and return true.  */
    return TRUE;
}

/* Actually, ISO C leaves conversion of UV to IV undefined, but
   until proven guilty, assume that things are not that bad... */

/*
   NV_PRESERVES_UV:

   As 64 bit platforms often have an NV that doesn't preserve all bits of
   an IV (an assumption perl has been based on to date) it becomes necessary
   to remove the assumption that the NV always carries enough precision to
   recreate the IV whenever needed, and that the NV is the canonical form.
   Instead, IV/UV and NV need to be given equal rights. So as to not lose
   precision as a side effect of conversion (which would lead to insanity
   and the dragon(s) in t/op/numconvert.t getting very angry) the intent is
   1) to distinguish between IV/UV/NV slots that have cached a valid
      conversion where precision was lost and IV/UV/NV slots that have a
      valid conversion which has lost no precision
   2) to ensure that if a numeric conversion to one form is requested that
      would lose precision, the precise conversion (or differently
      imprecise conversion) is also performed and cached, to prevent
      requests for different numeric formats on the same SV causing
      lossy conversion chains. (lossless conversion chains are perfectly
      acceptable (still))


   flags are used:
   SvIOKp is true if the IV slot contains a valid value
   SvIOK  is true only if the IV value is accurate (UV if SvIOK_UV true)
   SvNOKp is true if the NV slot contains a valid value
   SvNOK  is true only if the NV value is accurate

   so
   while converting from PV to NV, check to see if converting that NV to an
   IV(or UV) would lose accuracy over a direct conversion from PV to
   IV(or UV). If it would, cache both conversions, return NV, but mark
   SV as IOK NOKp (ie not NOK).

   While converting from PV to IV, check to see if converting that IV to an
   NV would lose accuracy over a direct conversion from PV to NV. If it
   would, cache both conversions, flag similarly.

   Before, the SV value "3.2" could become NV=3.2 IV=3 NOK, IOK quite
   correctly because if IV & NV were set NV *always* overruled.
   Now, "3.2" will become NV=3.2 IV=3 NOK, IOKp, because the flag's meaning
   changes - now IV and NV together means that the two are interchangeable:
   SvIVX == (IV) SvNVX && SvNVX == (NV) SvIVX;

   The benefit of this is that operations such as pp_add know that if
   SvIOK is true for both left and right operands, then integer addition
   can be used instead of floating point (for cases where the result won't
   overflow). Before, floating point was always used, which could lead to
   loss of precision compared with integer addition.

   * making IV and NV equal status should make maths accurate on 64 bit
     platforms
   * may speed up maths somewhat if pp_add and friends start to use
     integers when possible instead of fp. (Hopefully the overhead in
     looking for SvIOK and checking for overflow will not outweigh the
     fp to integer speedup)
   * will slow down integer operations (callers of SvIV) on "inaccurate"
     values, as the change from SvIOK to SvIOKp will cause a call into
     sv_2iv each time rather than a macro access direct to the IV slot
   * should speed up number->string conversion on integers as IV is
     favoured when IV and NV are equally accurate

   ####################################################################
   You had better be using SvIOK_notUV if you want an IV for arithmetic:
   SvIOK is true if (IV or UV), so you might be getting (IV)SvUV.
   On the other hand, SvUOK is true iff UV.
   ####################################################################

   Your mileage will vary depending your CPU's relative fp to integer
   performance ratio.
*/

#ifndef NV_PRESERVES_UV
#  define IS_NUMBER_UNDERFLOW_IV 1
#  define IS_NUMBER_UNDERFLOW_UV 2
#  define IS_NUMBER_IV_AND_UV    2
#  define IS_NUMBER_OVERFLOW_IV  4
#  define IS_NUMBER_OVERFLOW_UV  5

/* sv_2iuv_non_preserve(): private routine for use by sv_2iv() and sv_2uv() */

/* For sv_2nv these three cases are "SvNOK and don't bother casting"  */
STATIC int
S_sv_2iuv_non_preserve(pTHX_ register SV *const sv
#  ifdef DEBUGGING
		       , I32 numtype
#  endif
		       )
{
    dVAR;

    PERL_ARGS_ASSERT_SV_2IUV_NON_PRESERVE;

    DEBUG_c(PerlIO_printf(Perl_debug_log,"sv_2iuv_non '%s', IV=0x%"UVxf" NV=%"NVgf" inttype=%"UVXf"\n", SvPVX_const(sv), SvIVX(sv), SvNVX(sv), (UV)numtype));
    if (SvNVX(sv) < (NV)IV_MIN) {
	(void)SvIOKp_on(sv);
	(void)SvNOK_on(sv);
	SvIV_set(sv, IV_MIN);
	return IS_NUMBER_UNDERFLOW_IV;
    }
    if (SvNVX(sv) > (NV)UV_MAX) {
	(void)SvIOKp_on(sv);
	(void)SvNOK_on(sv);
	SvIsUV_on(sv);
	SvUV_set(sv, UV_MAX);
	return IS_NUMBER_OVERFLOW_UV;
    }
    (void)SvIOKp_on(sv);
    (void)SvNOK_on(sv);
    /* Can't use strtol etc to convert this string.  (See truth table in
       sv_2iv  */
    if (SvNVX(sv) <= (UV)IV_MAX) {
        SvIV_set(sv, I_V(SvNVX(sv)));
        if ((NV)(SvIVX(sv)) == SvNVX(sv)) {
            SvIOK_on(sv); /* Integer is precise. NOK, IOK */
        } else {
            /* Integer is imprecise. NOK, IOKp */
        }
        return SvNVX(sv) < 0 ? IS_NUMBER_UNDERFLOW_UV : IS_NUMBER_IV_AND_UV;
    }
    SvIsUV_on(sv);
    SvUV_set(sv, U_V(SvNVX(sv)));
    if ((NV)(SvUVX(sv)) == SvNVX(sv)) {
        if (SvUVX(sv) == UV_MAX) {
            /* As we know that NVs don't preserve UVs, UV_MAX cannot
               possibly be preserved by NV. Hence, it must be overflow.
               NOK, IOKp */
            return IS_NUMBER_OVERFLOW_UV;
        }
        SvIOK_on(sv); /* Integer is precise. NOK, UOK */
    } else {
        /* Integer is imprecise. NOK, IOKp */
    }
    return IS_NUMBER_OVERFLOW_IV;
}
#endif /* !NV_PRESERVES_UV*/

STATIC bool
S_sv_2iuv_common(pTHX_ SV *const sv)
{
    dVAR;

    PERL_ARGS_ASSERT_SV_2IUV_COMMON;

    if (SvNOKp(sv)) {
	/* erm. not sure. *should* never get NOKp (without NOK) from sv_2nv
	 * without also getting a cached IV/UV from it at the same time
	 * (ie PV->NV conversion should detect loss of accuracy and cache
	 * IV or UV at same time to avoid this. */
	/* IV-over-UV optimisation - choose to cache IV if possible */

	if (SvTYPE(sv) == SVt_NV)
	    sv_upgrade(sv, SVt_PVNV);

	(void)SvIOKp_on(sv);	/* Must do this first, to clear any SvOOK */
	/* < not <= as for NV doesn't preserve UV, ((NV)IV_MAX+1) will almost
	   certainly cast into the IV range at IV_MAX, whereas the correct
	   answer is the UV IV_MAX +1. Hence < ensures that dodgy boundary
	   cases go to UV */
#if defined(NAN_COMPARE_BROKEN) && defined(Perl_isnan)
	if (Perl_isnan(SvNVX(sv))) {
	    SvUV_set(sv, 0);
	    SvIsUV_on(sv);
	    return FALSE;
	}
#endif
	if (SvNVX(sv) < (NV)IV_MAX + 0.5) {
	    SvIV_set(sv, I_V(SvNVX(sv)));
	    if (SvNVX(sv) == (NV) SvIVX(sv)
#ifndef NV_PRESERVES_UV
		&& (((UV)1 << NV_PRESERVES_UV_BITS) >
		    (UV)(SvIVX(sv) > 0 ? SvIVX(sv) : -SvIVX(sv)))
		/* Don't flag it as "accurately an integer" if the number
		   came from a (by definition imprecise) NV operation, and
		   we're outside the range of NV integer precision */
#endif
		) {
		if (SvNOK(sv))
		    SvIOK_on(sv);  /* Can this go wrong with rounding? NWC */
		else {
		    /* scalar has trailing garbage, eg "42a" */
		}
		DEBUG_c(PerlIO_printf(Perl_debug_log,
				      "0x%"UVxf" iv(%"NVgf" => %"IVdf") (precise)\n",
				      PTR2UV(sv),
				      SvNVX(sv),
				      SvIVX(sv)));

	    } else {
		/* IV not precise.  No need to convert from PV, as NV
		   conversion would already have cached IV if it detected
		   that PV->IV would be better than PV->NV->IV
		   flags already correct - don't set public IOK.  */
		DEBUG_c(PerlIO_printf(Perl_debug_log,
				      "0x%"UVxf" iv(%"NVgf" => %"IVdf") (imprecise)\n",
				      PTR2UV(sv),
				      SvNVX(sv),
				      SvIVX(sv)));
	    }
	    /* Can the above go wrong if SvIVX == IV_MIN and SvNVX < IV_MIN,
	       but the cast (NV)IV_MIN rounds to a the value less (more
	       negative) than IV_MIN which happens to be equal to SvNVX ??
	       Analogous to 0xFFFFFFFFFFFFFFFF rounding up to NV (2**64) and
	       NV rounding back to 0xFFFFFFFFFFFFFFFF, so UVX == UV(NVX) and
	       (NV)UVX == NVX are both true, but the values differ. :-(
	       Hopefully for 2s complement IV_MIN is something like
	       0x8000000000000000 which will be exact. NWC */
	}
	else {
	    SvUV_set(sv, U_V(SvNVX(sv)));
	    if (
		(SvNVX(sv) == (NV) SvUVX(sv))
#ifndef  NV_PRESERVES_UV
		/* Make sure it's not 0xFFFFFFFFFFFFFFFF */
		/*&& (SvUVX(sv) != UV_MAX) irrelevant with code below */
		&& (((UV)1 << NV_PRESERVES_UV_BITS) > SvUVX(sv))
		/* Don't flag it as "accurately an integer" if the number
		   came from a (by definition imprecise) NV operation, and
		   we're outside the range of NV integer precision */
#endif
		&& SvNOK(sv)
		)
		SvIOK_on(sv);
	    SvIsUV_on(sv);
	    DEBUG_c(PerlIO_printf(Perl_debug_log,
				  "0x%"UVxf" 2iv(%"UVuf" => %"IVdf") (as unsigned)\n",
				  PTR2UV(sv),
				  SvUVX(sv),
				  SvUVX(sv)));
	}
    }
    else if (SvPOKp(sv) && SvLEN(sv)) {
	UV value;
	const int numtype = grok_number(SvPVX_const(sv), SvCUR(sv), &value);
	/* We want to avoid a possible problem when we cache an IV/ a UV which
	   may be later translated to an NV, and the resulting NV is not
	   the same as the direct translation of the initial string
	   (eg 123.456 can shortcut to the IV 123 with atol(), but we must
	   be careful to ensure that the value with the .456 is around if the
	   NV value is requested in the future).
	
	   This means that if we cache such an IV/a UV, we need to cache the
	   NV as well.  Moreover, we trade speed for space, and do not
	   cache the NV if we are sure it's not needed.
	 */

	/* SVt_PVNV is one higher than SVt_PVIV, hence this order  */
	if ((numtype & (IS_NUMBER_IN_UV | IS_NUMBER_NOT_INT))
	     == IS_NUMBER_IN_UV) {
	    /* It's definitely an integer, only upgrade to PVIV */
	    if (SvTYPE(sv) < SVt_PVIV)
		sv_upgrade(sv, SVt_PVIV);
	    (void)SvIOK_on(sv);
	} else if (SvTYPE(sv) < SVt_PVNV)
	    sv_upgrade(sv, SVt_PVNV);

	/* If NVs preserve UVs then we only use the UV value if we know that
	   we aren't going to call atof() below. If NVs don't preserve UVs
	   then the value returned may have more precision than atof() will
	   return, even though value isn't perfectly accurate.  */
	if ((numtype & (IS_NUMBER_IN_UV
#ifdef NV_PRESERVES_UV
			| IS_NUMBER_NOT_INT
#endif
	    )) == IS_NUMBER_IN_UV) {
	    /* This won't turn off the public IOK flag if it was set above  */
	    (void)SvIOKp_on(sv);

	    if (!(numtype & IS_NUMBER_NEG)) {
		/* positive */;
		if (value <= (UV)IV_MAX) {
		    SvIV_set(sv, (IV)value);
		} else {
		    /* it didn't overflow, and it was positive. */
		    SvUV_set(sv, value);
		    SvIsUV_on(sv);
		}
	    } else {
		/* 2s complement assumption  */
		if (value <= (UV)IV_MIN) {
		    SvIV_set(sv, -(IV)value);
		} else {
		    /* Too negative for an IV.  This is a double upgrade, but
		       I'm assuming it will be rare.  */
		    if (SvTYPE(sv) < SVt_PVNV)
			sv_upgrade(sv, SVt_PVNV);
		    SvNOK_on(sv);
		    SvIOK_off(sv);
		    SvIOKp_on(sv);
		    SvNV_set(sv, -(NV)value);
		    SvIV_set(sv, IV_MIN);
		}
	    }
	}
	/* For !NV_PRESERVES_UV and IS_NUMBER_IN_UV and IS_NUMBER_NOT_INT we
           will be in the previous block to set the IV slot, and the next
           block to set the NV slot.  So no else here.  */
	
	if ((numtype & (IS_NUMBER_IN_UV | IS_NUMBER_NOT_INT))
	    != IS_NUMBER_IN_UV) {
	    /* It wasn't an (integer that doesn't overflow the UV). */
	    SvNV_set(sv, Atof(SvPVX_const(sv)));

	    if (! numtype && ckWARN(WARN_NUMERIC))
		not_a_number(sv);

#if defined(USE_LONG_DOUBLE)
	    DEBUG_c(PerlIO_printf(Perl_debug_log, "0x%"UVxf" 2iv(%" PERL_PRIgldbl ")\n",
				  PTR2UV(sv), SvNVX(sv)));
#else
	    DEBUG_c(PerlIO_printf(Perl_debug_log, "0x%"UVxf" 2iv(%"NVgf")\n",
				  PTR2UV(sv), SvNVX(sv)));
#endif

#ifdef NV_PRESERVES_UV
            (void)SvIOKp_on(sv);
            (void)SvNOK_on(sv);
            if (SvNVX(sv) < (NV)IV_MAX + 0.5) {
                SvIV_set(sv, I_V(SvNVX(sv)));
                if ((NV)(SvIVX(sv)) == SvNVX(sv)) {
                    SvIOK_on(sv);
                } else {
		    NOOP;  /* Integer is imprecise. NOK, IOKp */
                }
                /* UV will not work better than IV */
            } else {
                if (SvNVX(sv) > (NV)UV_MAX) {
                    SvIsUV_on(sv);
                    /* Integer is inaccurate. NOK, IOKp, is UV */
                    SvUV_set(sv, UV_MAX);
                } else {
                    SvUV_set(sv, U_V(SvNVX(sv)));
                    /* 0xFFFFFFFFFFFFFFFF not an issue in here, NVs
                       NV preservse UV so can do correct comparison.  */
                    if ((NV)(SvUVX(sv)) == SvNVX(sv)) {
                        SvIOK_on(sv);
                    } else {
			NOOP;   /* Integer is imprecise. NOK, IOKp, is UV */
                    }
                }
		SvIsUV_on(sv);
            }
#else /* NV_PRESERVES_UV */
            if ((numtype & (IS_NUMBER_IN_UV | IS_NUMBER_NOT_INT))
                == (IS_NUMBER_IN_UV | IS_NUMBER_NOT_INT)) {
                /* The IV/UV slot will have been set from value returned by
                   grok_number above.  The NV slot has just been set using
                   Atof.  */
	        SvNOK_on(sv);
                assert (SvIOKp(sv));
            } else {
                if (((UV)1 << NV_PRESERVES_UV_BITS) >
                    U_V(SvNVX(sv) > 0 ? SvNVX(sv) : -SvNVX(sv))) {
                    /* Small enough to preserve all bits. */
                    (void)SvIOKp_on(sv);
                    SvNOK_on(sv);
                    SvIV_set(sv, I_V(SvNVX(sv)));
                    if ((NV)(SvIVX(sv)) == SvNVX(sv))
                        SvIOK_on(sv);
                    /* Assumption: first non-preserved integer is < IV_MAX,
                       this NV is in the preserved range, therefore: */
                    if (!(U_V(SvNVX(sv) > 0 ? SvNVX(sv) : -SvNVX(sv))
                          < (UV)IV_MAX)) {
                        Perl_croak(aTHX_ "sv_2iv assumed (U_V(fabs((double)SvNVX(sv))) < (UV)IV_MAX) but SvNVX(sv)=%"NVgf" U_V is 0x%"UVxf", IV_MAX is 0x%"UVxf"\n", SvNVX(sv), U_V(SvNVX(sv)), (UV)IV_MAX);
                    }
                } else {
                    /* IN_UV NOT_INT
                         0      0	already failed to read UV.
                         0      1       already failed to read UV.
                         1      0       you won't get here in this case. IV/UV
                         	        slot set, public IOK, Atof() unneeded.
                         1      1       already read UV.
                       so there's no point in sv_2iuv_non_preserve() attempting
                       to use atol, strtol, strtoul etc.  */
#  ifdef DEBUGGING
                    sv_2iuv_non_preserve (sv, numtype);
#  else
                    sv_2iuv_non_preserve (sv);
#  endif
                }
            }
#endif /* NV_PRESERVES_UV */
	/* It might be more code efficient to go through the entire logic above
	   and conditionally set with SvIOKp_on() rather than SvIOK(), but it
	   gets complex and potentially buggy, so more programmer efficient
	   to do it this way, by turning off the public flags:  */
	if (!numtype)
	    SvFLAGS(sv) &= ~(SVf_IOK|SVf_NOK);
	}
    }
    else  {
	if (isGV_with_GP(sv))
	    return glob_2number(MUTABLE_GV(sv));

	if (!(SvFLAGS(sv) & SVs_PADTMP)) {
	    if (!PL_localizing && ckWARN(WARN_UNINITIALIZED))
		report_uninit(sv);
	}
	if (SvTYPE(sv) < SVt_IV)
	    /* Typically the caller expects that sv_any is not NULL now.  */
	    sv_upgrade(sv, SVt_IV);
	/* Return 0 from the caller.  */
	return TRUE;
    }
    return FALSE;
}

/*
=for apidoc sv_2iv_flags

Return the integer value of an SV, doing any necessary string
conversion.  If flags includes SV_GMAGIC, does an mg_get() first.
Normally used via the C<SvIV(sv)> and C<SvIVx(sv)> macros.

=cut
*/

IV
Perl_sv_2iv_flags(pTHX_ register SV *const sv, const I32 flags)
{
    dVAR;
    if (!sv)
	return 0;
    if (SvGMAGICAL(sv) || (SvTYPE(sv) == SVt_PVGV && SvVALID(sv))) {
	/* FBMs use the same flag bit as SVf_IVisUV, so must let them
	   cache IVs just in case. In practice it seems that they never
	   actually anywhere accessible by user Perl code, let alone get used
	   in anything other than a string context.  */
	if (flags & SV_GMAGIC)
	    mg_get(sv);
	if (SvIOKp(sv))
	    return SvIVX(sv);
	if (SvNOKp(sv)) {
	    return I_V(SvNVX(sv));
	}
	if (SvPOKp(sv) && SvLEN(sv)) {
	    UV value;
	    const int numtype
		= grok_number(SvPVX_const(sv), SvCUR(sv), &value);

	    if ((numtype & (IS_NUMBER_IN_UV | IS_NUMBER_NOT_INT))
		== IS_NUMBER_IN_UV) {
		/* It's definitely an integer */
		if (numtype & IS_NUMBER_NEG) {
		    if (value < (UV)IV_MIN)
			return -(IV)value;
		} else {
		    if (value < (UV)IV_MAX)
			return (IV)value;
		}
	    }
	    if (!numtype) {
		if (ckWARN(WARN_NUMERIC))
		    not_a_number(sv);
	    }
	    return I_V(Atof(SvPVX_const(sv)));
	}
        if (SvROK(sv)) {
	    goto return_rok;
	}
	assert(SvTYPE(sv) >= SVt_PVMG);
	/* This falls through to the report_uninit inside S_sv_2iuv_common.  */
    } else if (SvTHINKFIRST(sv)) {
	if (SvROK(sv)) {
	return_rok:
	    if (SvAMAGIC(sv)) {
		SV * const tmpstr=AMG_CALLun(sv,numer);
		if (tmpstr && (!SvROK(tmpstr) || (SvRV(tmpstr) != SvRV(sv)))) {
		    return SvIV(tmpstr);
		}
	    }
	    return PTR2IV(SvRV(sv));
	}
	if (SvIsCOW(sv)) {
	    sv_force_normal_flags(sv, 0);
	}
	if (SvREADONLY(sv) && !SvOK(sv)) {
	    if (ckWARN(WARN_UNINITIALIZED))
		report_uninit(sv);
	    return 0;
	}
    }
    if (!SvIOKp(sv)) {
	if (S_sv_2iuv_common(aTHX_ sv))
	    return 0;
    }
    DEBUG_c(PerlIO_printf(Perl_debug_log, "0x%"UVxf" 2iv(%"IVdf")\n",
	PTR2UV(sv),SvIVX(sv)));
    return SvIsUV(sv) ? (IV)SvUVX(sv) : SvIVX(sv);
}

/*
=for apidoc sv_2uv_flags

Return the unsigned integer value of an SV, doing any necessary string
conversion.  If flags includes SV_GMAGIC, does an mg_get() first.
Normally used via the C<SvUV(sv)> and C<SvUVx(sv)> macros.

=cut
*/

UV
Perl_sv_2uv_flags(pTHX_ register SV *const sv, const I32 flags)
{
    dVAR;
    if (!sv)
	return 0;
    if (SvGMAGICAL(sv) || (SvTYPE(sv) == SVt_PVGV && SvVALID(sv))) {
	/* FBMs use the same flag bit as SVf_IVisUV, so must let them
	   cache IVs just in case.  */
	if (flags & SV_GMAGIC)
	    mg_get(sv);
	if (SvIOKp(sv))
	    return SvUVX(sv);
	if (SvNOKp(sv))
	    return U_V(SvNVX(sv));
	if (SvPOKp(sv) && SvLEN(sv)) {
	    UV value;
	    const int numtype
		= grok_number(SvPVX_const(sv), SvCUR(sv), &value);

	    if ((numtype & (IS_NUMBER_IN_UV | IS_NUMBER_NOT_INT))
		== IS_NUMBER_IN_UV) {
		/* It's definitely an integer */
		if (!(numtype & IS_NUMBER_NEG))
		    return value;
	    }
	    if (!numtype) {
		if (ckWARN(WARN_NUMERIC))
		    not_a_number(sv);
	    }
	    return U_V(Atof(SvPVX_const(sv)));
	}
        if (SvROK(sv)) {
	    goto return_rok;
	}
	assert(SvTYPE(sv) >= SVt_PVMG);
	/* This falls through to the report_uninit inside S_sv_2iuv_common.  */
    } else if (SvTHINKFIRST(sv)) {
	if (SvROK(sv)) {
	return_rok:
	    if (SvAMAGIC(sv)) {
		SV *const tmpstr = AMG_CALLun(sv,numer);
		if (tmpstr && (!SvROK(tmpstr) || (SvRV(tmpstr) != SvRV(sv)))) {
		    return SvUV(tmpstr);
		}
	    }
	    return PTR2UV(SvRV(sv));
	}
	if (SvIsCOW(sv)) {
	    sv_force_normal_flags(sv, 0);
	}
	if (SvREADONLY(sv) && !SvOK(sv)) {
	    if (ckWARN(WARN_UNINITIALIZED))
		report_uninit(sv);
	    return 0;
	}
    }
    if (!SvIOKp(sv)) {
	if (S_sv_2iuv_common(aTHX_ sv))
	    return 0;
    }

    DEBUG_c(PerlIO_printf(Perl_debug_log, "0x%"UVxf" 2uv(%"UVuf")\n",
			  PTR2UV(sv),SvUVX(sv)));
    return SvIsUV(sv) ? SvUVX(sv) : (UV)SvIVX(sv);
}

/*
=for apidoc sv_2nv

Return the num value of an SV, doing any necessary string or integer
conversion, magic etc. Normally used via the C<SvNV(sv)> and C<SvNVx(sv)>
macros.

=cut
*/

NV
Perl_sv_2nv(pTHX_ register SV *const sv)
{
    dVAR;
    if (!sv)
	return 0.0;
    if (SvGMAGICAL(sv) || (SvTYPE(sv) == SVt_PVGV && SvVALID(sv))) {
	/* FBMs use the same flag bit as SVf_IVisUV, so must let them
	   cache IVs just in case.  */
	mg_get(sv);
	if (SvNOKp(sv))
	    return SvNVX(sv);
	if ((SvPOKp(sv) && SvLEN(sv)) && !SvIOKp(sv)) {
	    if (!SvIOKp(sv) && ckWARN(WARN_NUMERIC) &&
		!grok_number(SvPVX_const(sv), SvCUR(sv), NULL))
		not_a_number(sv);
	    return Atof(SvPVX_const(sv));
	}
	if (SvIOKp(sv)) {
	    if (SvIsUV(sv))
		return (NV)SvUVX(sv);
	    else
		return (NV)SvIVX(sv);
	}
        if (SvROK(sv)) {
	    goto return_rok;
	}
	assert(SvTYPE(sv) >= SVt_PVMG);
	/* This falls through to the report_uninit near the end of the
	   function. */
    } else if (SvTHINKFIRST(sv)) {
	if (SvROK(sv)) {
	return_rok:
	    if (SvAMAGIC(sv)) {
		SV *const tmpstr = AMG_CALLun(sv,numer);
                if (tmpstr && (!SvROK(tmpstr) || (SvRV(tmpstr) != SvRV(sv)))) {
		    return SvNV(tmpstr);
		}
	    }
	    return PTR2NV(SvRV(sv));
	}
	if (SvIsCOW(sv)) {
	    sv_force_normal_flags(sv, 0);
	}
	if (SvREADONLY(sv) && !SvOK(sv)) {
	    if (ckWARN(WARN_UNINITIALIZED))
		report_uninit(sv);
	    return 0.0;
	}
    }
    if (SvTYPE(sv) < SVt_NV) {
	/* The logic to use SVt_PVNV if necessary is in sv_upgrade.  */
	sv_upgrade(sv, SVt_NV);
#ifdef USE_LONG_DOUBLE
	DEBUG_c({
	    STORE_NUMERIC_LOCAL_SET_STANDARD();
	    PerlIO_printf(Perl_debug_log,
			  "0x%"UVxf" num(%" PERL_PRIgldbl ")\n",
			  PTR2UV(sv), SvNVX(sv));
	    RESTORE_NUMERIC_LOCAL();
	});
#else
	DEBUG_c({
	    STORE_NUMERIC_LOCAL_SET_STANDARD();
	    PerlIO_printf(Perl_debug_log, "0x%"UVxf" num(%"NVgf")\n",
			  PTR2UV(sv), SvNVX(sv));
	    RESTORE_NUMERIC_LOCAL();
	});
#endif
    }
    else if (SvTYPE(sv) < SVt_PVNV)
	sv_upgrade(sv, SVt_PVNV);
    if (SvNOKp(sv)) {
        return SvNVX(sv);
    }
    if (SvIOKp(sv)) {
	SvNV_set(sv, SvIsUV(sv) ? (NV)SvUVX(sv) : (NV)SvIVX(sv));
#ifdef NV_PRESERVES_UV
	if (SvIOK(sv))
	    SvNOK_on(sv);
	else
	    SvNOKp_on(sv);
#else
	/* Only set the public NV OK flag if this NV preserves the IV  */
	/* Check it's not 0xFFFFFFFFFFFFFFFF */
	if (SvIOK(sv) &&
	    SvIsUV(sv) ? ((SvUVX(sv) != UV_MAX)&&(SvUVX(sv) == U_V(SvNVX(sv))))
		       : (SvIVX(sv) == I_V(SvNVX(sv))))
	    SvNOK_on(sv);
	else
	    SvNOKp_on(sv);
#endif
    }
    else if (SvPOKp(sv) && SvLEN(sv)) {
	UV value;
	const int numtype = grok_number(SvPVX_const(sv), SvCUR(sv), &value);
	if (!SvIOKp(sv) && !numtype && ckWARN(WARN_NUMERIC))
	    not_a_number(sv);
#ifdef NV_PRESERVES_UV
	if ((numtype & (IS_NUMBER_IN_UV | IS_NUMBER_NOT_INT))
	    == IS_NUMBER_IN_UV) {
	    /* It's definitely an integer */
	    SvNV_set(sv, (numtype & IS_NUMBER_NEG) ? -(NV)value : (NV)value);
	} else
	    SvNV_set(sv, Atof(SvPVX_const(sv)));
	if (numtype)
	    SvNOK_on(sv);
	else
	    SvNOKp_on(sv);
#else
	SvNV_set(sv, Atof(SvPVX_const(sv)));
	/* Only set the public NV OK flag if this NV preserves the value in
	   the PV at least as well as an IV/UV would.
	   Not sure how to do this 100% reliably. */
	/* if that shift count is out of range then Configure's test is
	   wonky. We shouldn't be in here with NV_PRESERVES_UV_BITS ==
	   UV_BITS */
	if (((UV)1 << NV_PRESERVES_UV_BITS) >
	    U_V(SvNVX(sv) > 0 ? SvNVX(sv) : -SvNVX(sv))) {
	    SvNOK_on(sv); /* Definitely small enough to preserve all bits */
	} else if (!(numtype & IS_NUMBER_IN_UV)) {
            /* Can't use strtol etc to convert this string, so don't try.
               sv_2iv and sv_2uv will use the NV to convert, not the PV.  */
            SvNOK_on(sv);
        } else {
            /* value has been set.  It may not be precise.  */
	    if ((numtype & IS_NUMBER_NEG) && (value > (UV)IV_MIN)) {
		/* 2s complement assumption for (UV)IV_MIN  */
                SvNOK_on(sv); /* Integer is too negative.  */
            } else {
                SvNOKp_on(sv);
                SvIOKp_on(sv);

                if (numtype & IS_NUMBER_NEG) {
                    SvIV_set(sv, -(IV)value);
                } else if (value <= (UV)IV_MAX) {
		    SvIV_set(sv, (IV)value);
		} else {
		    SvUV_set(sv, value);
		    SvIsUV_on(sv);
		}

                if (numtype & IS_NUMBER_NOT_INT) {
                    /* I believe that even if the original PV had decimals,
                       they are lost beyond the limit of the FP precision.
                       However, neither is canonical, so both only get p
                       flags.  NWC, 2000/11/25 */
                    /* Both already have p flags, so do nothing */
                } else {
		    const NV nv = SvNVX(sv);
                    if (SvNVX(sv) < (NV)IV_MAX + 0.5) {
                        if (SvIVX(sv) == I_V(nv)) {
                            SvNOK_on(sv);
                        } else {
                            /* It had no "." so it must be integer.  */
                        }
			SvIOK_on(sv);
                    } else {
                        /* between IV_MAX and NV(UV_MAX).
                           Could be slightly > UV_MAX */

                        if (numtype & IS_NUMBER_NOT_INT) {
                            /* UV and NV both imprecise.  */
                        } else {
			    const UV nv_as_uv = U_V(nv);

                            if (value == nv_as_uv && SvUVX(sv) != UV_MAX) {
                                SvNOK_on(sv);
                            }
			    SvIOK_on(sv);
                        }
                    }
                }
            }
        }
	/* It might be more code efficient to go through the entire logic above
	   and conditionally set with SvNOKp_on() rather than SvNOK(), but it
	   gets complex and potentially buggy, so more programmer efficient
	   to do it this way, by turning off the public flags:  */
	if (!numtype)
	    SvFLAGS(sv) &= ~(SVf_IOK|SVf_NOK);
#endif /* NV_PRESERVES_UV */
    }
    else  {
	if (isGV_with_GP(sv)) {
	    glob_2number(MUTABLE_GV(sv));
	    return 0.0;
	}

	if (!PL_localizing && !(SvFLAGS(sv) & SVs_PADTMP) && ckWARN(WARN_UNINITIALIZED))
	    report_uninit(sv);
	assert (SvTYPE(sv) >= SVt_NV);
	/* Typically the caller expects that sv_any is not NULL now.  */
	/* XXX Ilya implies that this is a bug in callers that assume this
	   and ideally should be fixed.  */
	return 0.0;
    }
#if defined(USE_LONG_DOUBLE)
    DEBUG_c({
	STORE_NUMERIC_LOCAL_SET_STANDARD();
	PerlIO_printf(Perl_debug_log, "0x%"UVxf" 2nv(%" PERL_PRIgldbl ")\n",
		      PTR2UV(sv), SvNVX(sv));
	RESTORE_NUMERIC_LOCAL();
    });
#else
    DEBUG_c({
	STORE_NUMERIC_LOCAL_SET_STANDARD();
	PerlIO_printf(Perl_debug_log, "0x%"UVxf" 1nv(%"NVgf")\n",
		      PTR2UV(sv), SvNVX(sv));
	RESTORE_NUMERIC_LOCAL();
    });
#endif
    return SvNVX(sv);
}

/*
=for apidoc sv_2num

Return an SV with the numeric value of the source SV, doing any necessary
reference or overload conversion.  You must use the C<SvNUM(sv)> macro to
access this function.

=cut
*/

SV *
Perl_sv_2num(pTHX_ register SV *const sv)
{
    PERL_ARGS_ASSERT_SV_2NUM;

    if (!SvROK(sv))
	return sv;
    if (SvAMAGIC(sv)) {
	SV * const tmpsv = AMG_CALLun(sv,numer);
	if (tmpsv && (!SvROK(tmpsv) || (SvRV(tmpsv) != SvRV(sv))))
	    return sv_2num(tmpsv);
    }
    return sv_2mortal(newSVuv(PTR2UV(SvRV(sv))));
}

/* uiv_2buf(): private routine for use by sv_2pv_flags(): print an IV or
 * UV as a string towards the end of buf, and return pointers to start and
 * end of it.
 *
 * We assume that buf is at least TYPE_CHARS(UV) long.
 */

static char *
S_uiv_2buf(char *const buf, const IV iv, UV uv, const int is_uv, char **const peob)
{
    char *ptr = buf + TYPE_CHARS(UV);
    char * const ebuf = ptr;
    int sign;

    PERL_ARGS_ASSERT_UIV_2BUF;

    if (is_uv)
	sign = 0;
    else if (iv >= 0) {
	uv = iv;
	sign = 0;
    } else {
	uv = -iv;
	sign = 1;
    }
    do {
	*--ptr = '0' + (char)(uv % 10);
    } while (uv /= 10);
    if (sign)
	*--ptr = '-';
    *peob = ebuf;
    return ptr;
}

/*
=for apidoc sv_2pv_flags

Returns a pointer to the string value of an SV, and sets *lp to its length.
If flags includes SV_GMAGIC, does an mg_get() first. Coerces sv to a string
if necessary.
Normally invoked via the C<SvPV_flags> macro. C<sv_2pv()> and C<sv_2pv_nomg>
usually end up here too.

=cut
*/

char *
Perl_sv_2pv_flags(pTHX_ register SV *const sv, STRLEN *const lp, const I32 flags)
{
    dVAR;
    register char *s;

    if (!sv) {
	if (lp)
	    *lp = 0;
	return (char *)"";
    }
    if (SvGMAGICAL(sv)) {
	if (flags & SV_GMAGIC)
	    mg_get(sv);
	if (SvPOKp(sv)) {
	    if (lp)
		*lp = SvCUR(sv);
	    if (flags & SV_MUTABLE_RETURN)
		return SvPVX_mutable(sv);
	    if (flags & SV_CONST_RETURN)
		return (char *)SvPVX_const(sv);
	    return SvPVX(sv);
	}
	if (SvIOKp(sv) || SvNOKp(sv)) {
	    char tbuf[64];  /* Must fit sprintf/Gconvert of longest IV/NV */
	    STRLEN len;

	    if (SvIOKp(sv)) {
		len = SvIsUV(sv)
		    ? my_snprintf(tbuf, sizeof(tbuf), "%"UVuf, (UV)SvUVX(sv))
		    : my_snprintf(tbuf, sizeof(tbuf), "%"IVdf, (IV)SvIVX(sv));
	    } else {
		Gconvert(SvNVX(sv), NV_DIG, 0, tbuf);
		len = strlen(tbuf);
	    }
	    assert(!SvROK(sv));
	    {
		dVAR;

#ifdef FIXNEGATIVEZERO
		if (len == 2 && tbuf[0] == '-' && tbuf[1] == '0') {
		    tbuf[0] = '0';
		    tbuf[1] = 0;
		    len = 1;
		}
#endif
		SvUPGRADE(sv, SVt_PV);
		if (lp)
		    *lp = len;
		s = SvGROW_mutable(sv, len + 1);
		SvCUR_set(sv, len);
		SvPOKp_on(sv);
		return (char*)memcpy(s, tbuf, len + 1);
	    }
	}
        if (SvROK(sv)) {
	    goto return_rok;
	}
	assert(SvTYPE(sv) >= SVt_PVMG);
	/* This falls through to the report_uninit near the end of the
	   function. */
    } else if (SvTHINKFIRST(sv)) {
	if (SvROK(sv)) {
	return_rok:
            if (SvAMAGIC(sv)) {
		SV *const tmpstr = AMG_CALLun(sv,string);
		if (tmpstr && (!SvROK(tmpstr) || (SvRV(tmpstr) != SvRV(sv)))) {
		    /* Unwrap this:  */
		    /* char *pv = lp ? SvPV(tmpstr, *lp) : SvPV_nolen(tmpstr);
		     */

		    char *pv;
		    if ((SvFLAGS(tmpstr) & (SVf_POK)) == SVf_POK) {
			if (flags & SV_CONST_RETURN) {
			    pv = (char *) SvPVX_const(tmpstr);
			} else {
			    pv = (flags & SV_MUTABLE_RETURN)
				? SvPVX_mutable(tmpstr) : SvPVX(tmpstr);
			}
			if (lp)
			    *lp = SvCUR(tmpstr);
		    } else {
			pv = sv_2pv_flags(tmpstr, lp, flags);
		    }
		    if (SvUTF8(tmpstr))
			SvUTF8_on(sv);
		    else
			SvUTF8_off(sv);
		    return pv;
		}
	    }
	    {
		STRLEN len;
		char *retval;
		char *buffer;
		SV *const referent = SvRV(sv);

		if (!referent) {
		    len = 7;
		    retval = buffer = savepvn("NULLREF", len);
		} else if (SvTYPE(referent) == SVt_REGEXP) {
		    REGEXP * const re = (REGEXP *)MUTABLE_PTR(referent);
		    I32 seen_evals = 0;

		    assert(re);
			
		    /* If the regex is UTF-8 we want the containing scalar to
		       have an UTF-8 flag too */
		    if (RX_UTF8(re))
			SvUTF8_on(sv);
		    else
			SvUTF8_off(sv);	

		    if ((seen_evals = RX_SEEN_EVALS(re)))
			PL_reginterp_cnt += seen_evals;

		    if (lp)
			*lp = RX_WRAPLEN(re);
 
		    return RX_WRAPPED(re);
		} else {
		    const char *const typestr = sv_reftype(referent, 0);
		    const STRLEN typelen = strlen(typestr);
		    UV addr = PTR2UV(referent);
		    const char *stashname = NULL;
		    STRLEN stashnamelen = 0; /* hush, gcc */
		    const char *buffer_end;

		    if (SvOBJECT(referent)) {
			const HEK *const name = HvNAME_HEK(SvSTASH(referent));

			if (name) {
			    stashname = HEK_KEY(name);
			    stashnamelen = HEK_LEN(name);

			    if (HEK_UTF8(name)) {
				SvUTF8_on(sv);
			    } else {
				SvUTF8_off(sv);
			    }
			} else {
			    stashname = "__ANON__";
			    stashnamelen = 8;
			}
			len = stashnamelen + 1 /* = */ + typelen + 3 /* (0x */
			    + 2 * sizeof(UV) + 2 /* )\0 */;
		    } else {
			len = typelen + 3 /* (0x */
			    + 2 * sizeof(UV) + 2 /* )\0 */;
		    }

		    Newx(buffer, len, char);
		    buffer_end = retval = buffer + len;

		    /* Working backwards  */
		    *--retval = '\0';
		    *--retval = ')';
		    do {
			*--retval = PL_hexdigit[addr & 15];
		    } while (addr >>= 4);
		    *--retval = 'x';
		    *--retval = '0';
		    *--retval = '(';

		    retval -= typelen;
		    memcpy(retval, typestr, typelen);

		    if (stashname) {
			*--retval = '=';
			retval -= stashnamelen;
			memcpy(retval, stashname, stashnamelen);
		    }
		    /* retval may not neccesarily have reached the start of the
		       buffer here.  */
		    assert (retval >= buffer);

		    len = buffer_end - retval - 1; /* -1 for that \0  */
		}
		if (lp)
		    *lp = len;
		SAVEFREEPV(buffer);
		return retval;
	    }
	}
	if (SvREADONLY(sv) && !SvOK(sv)) {
	    if (lp)
		*lp = 0;
	    if (flags & SV_UNDEF_RETURNS_NULL)
		return NULL;
	    if (ckWARN(WARN_UNINITIALIZED))
		report_uninit(sv);
	    return (char *)"";
	}
    }
    if (SvIOK(sv) || ((SvIOKp(sv) && !SvNOKp(sv)))) {
	/* I'm assuming that if both IV and NV are equally valid then
	   converting the IV is going to be more efficient */
	const U32 isUIOK = SvIsUV(sv);
	char buf[TYPE_CHARS(UV)];
	char *ebuf, *ptr;
	STRLEN len;

	if (SvTYPE(sv) < SVt_PVIV)
	    sv_upgrade(sv, SVt_PVIV);
 	ptr = uiv_2buf(buf, SvIVX(sv), SvUVX(sv), isUIOK, &ebuf);
	len = ebuf - ptr;
	/* inlined from sv_setpvn */
	s = SvGROW_mutable(sv, len + 1);
	Move(ptr, s, len, char);
	s += len;
	*s = '\0';
    }
    else if (SvNOKp(sv)) {
	dSAVE_ERRNO;
	if (SvTYPE(sv) < SVt_PVNV)
	    sv_upgrade(sv, SVt_PVNV);
	/* The +20 is pure guesswork.  Configure test needed. --jhi */
	s = SvGROW_mutable(sv, NV_DIG + 20);
	/* some Xenix systems wipe out errno here */
#ifdef apollo
	if (SvNVX(sv) == 0.0)
	    my_strlcpy(s, "0", SvLEN(sv));
	else
#endif /*apollo*/
	{
	    Gconvert(SvNVX(sv), NV_DIG, 0, s);
	}
	RESTORE_ERRNO;
#ifdef FIXNEGATIVEZERO
        if (*s == '-' && s[1] == '0' && !s[2]) {
	    s[0] = '0';
	    s[1] = 0;
	}
#endif
	while (*s) s++;
#ifdef hcx
	if (s[-1] == '.')
	    *--s = '\0';
#endif
    }
    else {
	if (isGV_with_GP(sv)) {
	    GV *const gv = MUTABLE_GV(sv);
	    const U32 wasfake = SvFLAGS(gv) & SVf_FAKE;
	    SV *const buffer = sv_newmortal();

	    /* FAKE globs can get coerced, so need to turn this off temporarily
	       if it is on.  */
	    SvFAKE_off(gv);
	    gv_efullname3(buffer, gv, "*");
	    SvFLAGS(gv) |= wasfake;

	    if (SvPOK(buffer)) {
		if (lp) {
		    *lp = SvCUR(buffer);
		}
		return SvPVX(buffer);
	    }
	    else {
		if (lp)
		    *lp = 0;
		return (char *)"";
	    }
	}

	if (lp)
	    *lp = 0;
	if (flags & SV_UNDEF_RETURNS_NULL)
	    return NULL;
	if (!PL_localizing && !(SvFLAGS(sv) & SVs_PADTMP) && ckWARN(WARN_UNINITIALIZED))
	    report_uninit(sv);
	if (SvTYPE(sv) < SVt_PV)
	    /* Typically the caller expects that sv_any is not NULL now.  */
	    sv_upgrade(sv, SVt_PV);
	return (char *)"";
    }
    {
	const STRLEN len = s - SvPVX_const(sv);
	if (lp) 
	    *lp = len;
	SvCUR_set(sv, len);
    }
    SvPOK_on(sv);
    DEBUG_c(PerlIO_printf(Perl_debug_log, "0x%"UVxf" 2pv(%s)\n",
			  PTR2UV(sv),SvPVX_const(sv)));
    if (flags & SV_CONST_RETURN)
	return (char *)SvPVX_const(sv);
    if (flags & SV_MUTABLE_RETURN)
	return SvPVX_mutable(sv);
    return SvPVX(sv);
}

/*
=for apidoc sv_copypv

Copies a stringified representation of the source SV into the
destination SV.  Automatically performs any necessary mg_get and
coercion of numeric values into strings.  Guaranteed to preserve
UTF8 flag even from overloaded objects.  Similar in nature to
sv_2pv[_flags] but operates directly on an SV instead of just the
string.  Mostly uses sv_2pv_flags to do its work, except when that
would lose the UTF-8'ness of the PV.

=cut
*/

void
Perl_sv_copypv(pTHX_ SV *const dsv, register SV *const ssv)
{
    STRLEN len;
    const char * const s = SvPV_const(ssv,len);

    PERL_ARGS_ASSERT_SV_COPYPV;

    sv_setpvn(dsv,s,len);
    if (SvUTF8(ssv))
	SvUTF8_on(dsv);
    else
	SvUTF8_off(dsv);
}

/*
=for apidoc sv_2pvbyte

Return a pointer to the byte-encoded representation of the SV, and set *lp
to its length.  May cause the SV to be downgraded from UTF-8 as a
side-effect.

Usually accessed via the C<SvPVbyte> macro.

=cut
*/

char *
Perl_sv_2pvbyte(pTHX_ register SV *const sv, STRLEN *const lp)
{
    PERL_ARGS_ASSERT_SV_2PVBYTE;

    sv_utf8_downgrade(sv,0);
    return lp ? SvPV(sv,*lp) : SvPV_nolen(sv);
}

/*
=for apidoc sv_2pvutf8

Return a pointer to the UTF-8-encoded representation of the SV, and set *lp
to its length.  May cause the SV to be upgraded to UTF-8 as a side-effect.

Usually accessed via the C<SvPVutf8> macro.

=cut
*/

char *
Perl_sv_2pvutf8(pTHX_ register SV *const sv, STRLEN *const lp)
{
    PERL_ARGS_ASSERT_SV_2PVUTF8;

    sv_utf8_upgrade(sv);
    return lp ? SvPV(sv,*lp) : SvPV_nolen(sv);
}


/*
=for apidoc sv_2bool

This function is only called on magical items, and is only used by
sv_true() or its macro equivalent.

=cut
*/

bool
Perl_sv_2bool(pTHX_ register SV *const sv)
{
    dVAR;

    PERL_ARGS_ASSERT_SV_2BOOL;

    SvGETMAGIC(sv);

    if (!SvOK(sv))
	return 0;
    if (SvROK(sv)) {
	if (SvAMAGIC(sv)) {
	    SV * const tmpsv = AMG_CALLun(sv,bool_);
	    if (tmpsv && (!SvROK(tmpsv) || (SvRV(tmpsv) != SvRV(sv))))
		return (bool)SvTRUE(tmpsv);
	}
	return SvRV(sv) != 0;
    }
    if (SvPOKp(sv)) {
	register XPV* const Xpvtmp = (XPV*)SvANY(sv);
	if (Xpvtmp &&
		(*sv->sv_u.svu_pv > '0' ||
		Xpvtmp->xpv_cur > 1 ||
		(Xpvtmp->xpv_cur && *sv->sv_u.svu_pv != '0')))
	    return 1;
	else
	    return 0;
    }
    else {
	if (SvIOKp(sv))
	    return SvIVX(sv) != 0;
	else {
	    if (SvNOKp(sv))
		return SvNVX(sv) != 0.0;
	    else {
		if (isGV_with_GP(sv))
		    return TRUE;
		else
		    return FALSE;
	    }
	}
    }
}

/*
=for apidoc sv_utf8_upgrade

Converts the PV of an SV to its UTF-8-encoded form.
Forces the SV to string form if it is not already.
Will C<mg_get> on C<sv> if appropriate.
Always sets the SvUTF8 flag to avoid future validity checks even
if the whole string is the same in UTF-8 as not.
Returns the number of bytes in the converted string

This is not as a general purpose byte encoding to Unicode interface:
use the Encode extension for that.

=for apidoc sv_utf8_upgrade_nomg

Like sv_utf8_upgrade, but doesn't do magic on C<sv>

=for apidoc sv_utf8_upgrade_flags

Converts the PV of an SV to its UTF-8-encoded form.
Forces the SV to string form if it is not already.
Always sets the SvUTF8 flag to avoid future validity checks even
if all the bytes are invariant in UTF-8. If C<flags> has C<SV_GMAGIC> bit set,
will C<mg_get> on C<sv> if appropriate, else not.
Returns the number of bytes in the converted string
C<sv_utf8_upgrade> and
C<sv_utf8_upgrade_nomg> are implemented in terms of this function.

This is not as a general purpose byte encoding to Unicode interface:
use the Encode extension for that.

=cut

The grow version is currently not externally documented.  It adds a parameter,
extra, which is the number of unused bytes the string of 'sv' is guaranteed to
have free after it upon return.  This allows the caller to reserve extra space
that it intends to fill, to avoid extra grows.

Also externally undocumented for the moment is the flag SV_FORCE_UTF8_UPGRADE,
which can be used to tell this function to not first check to see if there are
any characters that are different in UTF-8 (variant characters) which would
force it to allocate a new string to sv, but to assume there are.  Typically
this flag is used by a routine that has already parsed the string to find that
there are such characters, and passes this information on so that the work
doesn't have to be repeated.

(One might think that the calling routine could pass in the position of the
first such variant, so it wouldn't have to be found again.  But that is not the
case, because typically when the caller is likely to use this flag, it won't be
calling this routine unless it finds something that won't fit into a byte.
Otherwise it tries to not upgrade and just use bytes.  But some things that
do fit into a byte are variants in utf8, and the caller may not have been
keeping track of these.)

If the routine itself changes the string, it adds a trailing NUL.  Such a NUL
isn't guaranteed due to having other routines do the work in some input cases,
or if the input is already flagged as being in utf8.

The speed of this could perhaps be improved for many cases if someone wanted to
write a fast function that counts the number of variant characters in a string,
especially if it could return the position of the first one.

*/

STRLEN
Perl_sv_utf8_upgrade_flags_grow(pTHX_ register SV *const sv, const I32 flags, STRLEN extra)
{
    dVAR;

    PERL_ARGS_ASSERT_SV_UTF8_UPGRADE_FLAGS_GROW;

    if (sv == &PL_sv_undef)
	return 0;
    if (!SvPOK(sv)) {
	STRLEN len = 0;
	if (SvREADONLY(sv) && (SvPOKp(sv) || SvIOKp(sv) || SvNOKp(sv))) {
	    (void) sv_2pv_flags(sv,&len, flags);
	    if (SvUTF8(sv)) {
		if (extra) SvGROW(sv, SvCUR(sv) + extra);
		return len;
	    }
	} else {
	    (void) SvPV_force(sv,len);
	}
    }

    if (SvUTF8(sv)) {
	if (extra) SvGROW(sv, SvCUR(sv) + extra);
	return SvCUR(sv);
    }

    if (SvIsCOW(sv)) {
        sv_force_normal_flags(sv, 0);
    }

    if (PL_encoding && !(flags & SV_UTF8_NO_ENCODING)) {
        sv_recode_to_utf8(sv, PL_encoding);
	if (extra) SvGROW(sv, SvCUR(sv) + extra);
	return SvCUR(sv);
    }

    if (SvCUR(sv) == 0) {
	if (extra) SvGROW(sv, extra);
    } else { /* Assume Latin-1/EBCDIC */
	/* This function could be much more efficient if we
	 * had a FLAG in SVs to signal if there are any variant
	 * chars in the PV.  Given that there isn't such a flag
	 * make the loop as fast as possible (although there are certainly ways
	 * to speed this up, eg. through vectorization) */
	U8 * s = (U8 *) SvPVX_const(sv);
	U8 * e = (U8 *) SvEND(sv);
	U8 *t = s;
	STRLEN two_byte_count = 0;
	
	if (flags & SV_FORCE_UTF8_UPGRADE) goto must_be_utf8;

	/* See if really will need to convert to utf8.  We mustn't rely on our
	 * incoming SV being well formed and having a trailing '\0', as certain
	 * code in pp_formline can send us partially built SVs. */

	while (t < e) {
	    const U8 ch = *t++;
	    if (NATIVE_IS_INVARIANT(ch)) continue;

	    t--;    /* t already incremented; re-point to first variant */
	    two_byte_count = 1;
	    goto must_be_utf8;
	}

	/* utf8 conversion not needed because all are invariants.  Mark as
	 * UTF-8 even if no variant - saves scanning loop */
	SvUTF8_on(sv);
	return SvCUR(sv);

must_be_utf8:

	/* Here, the string should be converted to utf8, either because of an
	 * input flag (two_byte_count = 0), or because a character that
	 * requires 2 bytes was found (two_byte_count = 1).  t points either to
	 * the beginning of the string (if we didn't examine anything), or to
	 * the first variant.  In either case, everything from s to t - 1 will
	 * occupy only 1 byte each on output.
	 *
	 * There are two main ways to convert.  One is to create a new string
	 * and go through the input starting from the beginning, appending each
	 * converted value onto the new string as we go along.  It's probably
	 * best to allocate enough space in the string for the worst possible
	 * case rather than possibly running out of space and having to
	 * reallocate and then copy what we've done so far.  Since everything
	 * from s to t - 1 is invariant, the destination can be initialized
	 * with these using a fast memory copy
	 *
	 * The other way is to figure out exactly how big the string should be
	 * by parsing the entire input.  Then you don't have to make it big
	 * enough to handle the worst possible case, and more importantly, if
	 * the string you already have is large enough, you don't have to
	 * allocate a new string, you can copy the last character in the input
	 * string to the final position(s) that will be occupied by the
	 * converted string and go backwards, stopping at t, since everything
	 * before that is invariant.
	 *
	 * There are advantages and disadvantages to each method.
	 *
	 * In the first method, we can allocate a new string, do the memory
	 * copy from the s to t - 1, and then proceed through the rest of the
	 * string byte-by-byte.
	 *
	 * In the second method, we proceed through the rest of the input
	 * string just calculating how big the converted string will be.  Then
	 * there are two cases:
	 *  1)	if the string has enough extra space to handle the converted
	 *	value.  We go backwards through the string, converting until we
	 *	get to the position we are at now, and then stop.  If this
	 *	position is far enough along in the string, this method is
	 *	faster than the other method.  If the memory copy were the same
	 *	speed as the byte-by-byte loop, that position would be about
	 *	half-way, as at the half-way mark, parsing to the end and back
	 *	is one complete string's parse, the same amount as starting
	 *	over and going all the way through.  Actually, it would be
	 *	somewhat less than half-way, as it's faster to just count bytes
	 *	than to also copy, and we don't have the overhead of allocating
	 *	a new string, changing the scalar to use it, and freeing the
	 *	existing one.  But if the memory copy is fast, the break-even
	 *	point is somewhere after half way.  The counting loop could be
	 *	sped up by vectorization, etc, to move the break-even point
	 *	further towards the beginning.
	 *  2)	if the string doesn't have enough space to handle the converted
	 *	value.  A new string will have to be allocated, and one might
	 *	as well, given that, start from the beginning doing the first
	 *	method.  We've spent extra time parsing the string and in
	 *	exchange all we've gotten is that we know precisely how big to
	 *	make the new one.  Perl is more optimized for time than space,
	 *	so this case is a loser.
	 * So what I've decided to do is not use the 2nd method unless it is
	 * guaranteed that a new string won't have to be allocated, assuming
	 * the worst case.  I also decided not to put any more conditions on it
	 * than this, for now.  It seems likely that, since the worst case is
	 * twice as big as the unknown portion of the string (plus 1), we won't
	 * be guaranteed enough space, causing us to go to the first method,
	 * unless the string is short, or the first variant character is near
	 * the end of it.  In either of these cases, it seems best to use the
	 * 2nd method.  The only circumstance I can think of where this would
	 * be really slower is if the string had once had much more data in it
	 * than it does now, but there is still a substantial amount in it  */

	{
	    STRLEN invariant_head = t - s;
	    STRLEN size = invariant_head + (e - t) * 2 + 1 + extra;
	    if (SvLEN(sv) < size) {

		/* Here, have decided to allocate a new string */

		U8 *dst;
		U8 *d;

		Newx(dst, size, U8);

		/* If no known invariants at the beginning of the input string,
		 * set so starts from there.  Otherwise, can use memory copy to
		 * get up to where we are now, and then start from here */

		if (invariant_head <= 0) {
		    d = dst;
		} else {
		    Copy(s, dst, invariant_head, char);
		    d = dst + invariant_head;
		}

		while (t < e) {
		    const UV uv = NATIVE8_TO_UNI(*t++);
		    if (UNI_IS_INVARIANT(uv))
			*d++ = (U8)UNI_TO_NATIVE(uv);
		    else {
			*d++ = (U8)UTF8_EIGHT_BIT_HI(uv);
			*d++ = (U8)UTF8_EIGHT_BIT_LO(uv);
		    }
		}
		*d = '\0';
		SvPV_free(sv); /* No longer using pre-existing string */
		SvPV_set(sv, (char*)dst);
		SvCUR_set(sv, d - dst);
		SvLEN_set(sv, size);
	    } else {

		/* Here, have decided to get the exact size of the string.
		 * Currently this happens only when we know that there is
		 * guaranteed enough space to fit the converted string, so
		 * don't have to worry about growing.  If two_byte_count is 0,
		 * then t points to the first byte of the string which hasn't
		 * been examined yet.  Otherwise two_byte_count is 1, and t
		 * points to the first byte in the string that will expand to
		 * two.  Depending on this, start examining at t or 1 after t.
		 * */

		U8 *d = t + two_byte_count;


		/* Count up the remaining bytes that expand to two */

		while (d < e) {
		    const U8 chr = *d++;
		    if (! NATIVE_IS_INVARIANT(chr)) two_byte_count++;
		}

		/* The string will expand by just the number of bytes that
		 * occupy two positions.  But we are one afterwards because of
		 * the increment just above.  This is the place to put the
		 * trailing NUL, and to set the length before we decrement */

		d += two_byte_count;
		SvCUR_set(sv, d - s);
		*d-- = '\0';


		/* Having decremented d, it points to the position to put the
		 * very last byte of the expanded string.  Go backwards through
		 * the string, copying and expanding as we go, stopping when we
		 * get to the part that is invariant the rest of the way down */

		e--;
		while (e >= t) {
		    const U8 ch = NATIVE8_TO_UNI(*e--);
		    if (UNI_IS_INVARIANT(ch)) {
			*d-- = UNI_TO_NATIVE(ch);
		    } else {
			*d-- = (U8)UTF8_EIGHT_BIT_LO(ch);
			*d-- = (U8)UTF8_EIGHT_BIT_HI(ch);
		    }
		}
	    }
	}
    }

    /* Mark as UTF-8 even if no variant - saves scanning loop */
    SvUTF8_on(sv);
    return SvCUR(sv);
}

/*
=for apidoc sv_utf8_downgrade

Attempts to convert the PV of an SV from characters to bytes.
If the PV contains a character that cannot fit
in a byte, this conversion will fail;
in this case, either returns false or, if C<fail_ok> is not
true, croaks.

This is not as a general purpose Unicode to byte encoding interface:
use the Encode extension for that.

=cut
*/

bool
Perl_sv_utf8_downgrade(pTHX_ register SV *const sv, const bool fail_ok)
{
    dVAR;

    PERL_ARGS_ASSERT_SV_UTF8_DOWNGRADE;

    if (SvPOKp(sv) && SvUTF8(sv)) {
        if (SvCUR(sv)) {
	    U8 *s;
	    STRLEN len;

            if (SvIsCOW(sv)) {
                sv_force_normal_flags(sv, 0);
            }
	    s = (U8 *) SvPV(sv, len);
	    if (!utf8_to_bytes(s, &len)) {
	        if (fail_ok)
		    return FALSE;
		else {
		    if (PL_op)
		        Perl_croak(aTHX_ "Wide character in %s",
				   OP_DESC(PL_op));
		    else
		        Perl_croak(aTHX_ "Wide character");
		}
	    }
	    SvCUR_set(sv, len);
	}
    }
    SvUTF8_off(sv);
    return TRUE;
}

/*
=for apidoc sv_utf8_encode

Converts the PV of an SV to UTF-8, but then turns the C<SvUTF8>
flag off so that it looks like octets again.

=cut
*/

void
Perl_sv_utf8_encode(pTHX_ register SV *const sv)
{
    PERL_ARGS_ASSERT_SV_UTF8_ENCODE;

    if (SvIsCOW(sv)) {
        sv_force_normal_flags(sv, 0);
    }
    if (SvREADONLY(sv)) {
	Perl_croak(aTHX_ "%s", PL_no_modify);
    }
    (void) sv_utf8_upgrade(sv);
    SvUTF8_off(sv);
}

/*
=for apidoc sv_utf8_decode

If the PV of the SV is an octet sequence in UTF-8
and contains a multiple-byte character, the C<SvUTF8> flag is turned on
so that it looks like a character. If the PV contains only single-byte
characters, the C<SvUTF8> flag stays being off.
Scans PV for validity and returns false if the PV is invalid UTF-8.

=cut
*/

bool
Perl_sv_utf8_decode(pTHX_ register SV *const sv)
{
    PERL_ARGS_ASSERT_SV_UTF8_DECODE;

    if (SvPOKp(sv)) {
        const U8 *c;
        const U8 *e;

	/* The octets may have got themselves encoded - get them back as
	 * bytes
	 */
	if (!sv_utf8_downgrade(sv, TRUE))
	    return FALSE;

        /* it is actually just a matter of turning the utf8 flag on, but
         * we want to make sure everything inside is valid utf8 first.
         */
        c = (const U8 *) SvPVX_const(sv);
	if (!is_utf8_string(c, SvCUR(sv)+1))
	    return FALSE;
        e = (const U8 *) SvEND(sv);
        while (c < e) {
	    const U8 ch = *c++;
            if (!UTF8_IS_INVARIANT(ch)) {
		SvUTF8_on(sv);
		break;
	    }
        }
    }
    return TRUE;
}

/*
=for apidoc sv_setsv

Copies the contents of the source SV C<ssv> into the destination SV
C<dsv>.  The source SV may be destroyed if it is mortal, so don't use this
function if the source SV needs to be reused. Does not handle 'set' magic.
Loosely speaking, it performs a copy-by-value, obliterating any previous
content of the destination.

You probably want to use one of the assortment of wrappers, such as
C<SvSetSV>, C<SvSetSV_nosteal>, C<SvSetMagicSV> and
C<SvSetMagicSV_nosteal>.

=for apidoc sv_setsv_flags

Copies the contents of the source SV C<ssv> into the destination SV
C<dsv>.  The source SV may be destroyed if it is mortal, so don't use this
function if the source SV needs to be reused. Does not handle 'set' magic.
Loosely speaking, it performs a copy-by-value, obliterating any previous
content of the destination.
If the C<flags> parameter has the C<SV_GMAGIC> bit set, will C<mg_get> on
C<ssv> if appropriate, else not. If the C<flags> parameter has the
C<NOSTEAL> bit set then the buffers of temps will not be stolen. <sv_setsv>
and C<sv_setsv_nomg> are implemented in terms of this function.

You probably want to use one of the assortment of wrappers, such as
C<SvSetSV>, C<SvSetSV_nosteal>, C<SvSetMagicSV> and
C<SvSetMagicSV_nosteal>.

This is the primary function for copying scalars, and most other
copy-ish functions and macros use this underneath.

=cut
*/

static void
S_glob_assign_glob(pTHX_ SV *const dstr, SV *const sstr, const int dtype)
{
    I32 mro_changes = 0; /* 1 = method, 2 = isa */

    PERL_ARGS_ASSERT_GLOB_ASSIGN_GLOB;

    if (dtype != SVt_PVGV) {
	const char * const name = GvNAME(sstr);
	const STRLEN len = GvNAMELEN(sstr);
	{
	    if (dtype >= SVt_PV) {
		SvPV_free(dstr);
		SvPV_set(dstr, 0);
		SvLEN_set(dstr, 0);
		SvCUR_set(dstr, 0);
	    }
	    SvUPGRADE(dstr, SVt_PVGV);
	    (void)SvOK_off(dstr);
	    /* FIXME - why are we doing this, then turning it off and on again
	       below?  */
	    isGV_with_GP_on(dstr);
	}
	GvSTASH(dstr) = GvSTASH(sstr);
	if (GvSTASH(dstr))
	    Perl_sv_add_backref(aTHX_ MUTABLE_SV(GvSTASH(dstr)), dstr);
	gv_name_set(MUTABLE_GV(dstr), name, len, GV_ADD);
	SvFAKE_on(dstr);	/* can coerce to non-glob */
    }

    if(GvGP(MUTABLE_GV(sstr))) {
        /* If source has method cache entry, clear it */
        if(GvCVGEN(sstr)) {
            SvREFCNT_dec(GvCV(sstr));
            GvCV(sstr) = NULL;
            GvCVGEN(sstr) = 0;
        }
        /* If source has a real method, then a method is
           going to change */
        else if(GvCV((const GV *)sstr)) {
            mro_changes = 1;
        }
    }

    /* If dest already had a real method, that's a change as well */
    if(!mro_changes && GvGP(MUTABLE_GV(dstr)) && GvCVu((const GV *)dstr)) {
        mro_changes = 1;
    }

    if(strEQ(GvNAME((const GV *)dstr),"ISA"))
        mro_changes = 2;

    gp_free(MUTABLE_GV(dstr));
    isGV_with_GP_off(dstr);
    (void)SvOK_off(dstr);
    isGV_with_GP_on(dstr);
    GvINTRO_off(dstr);		/* one-shot flag */
    GvGP(dstr) = gp_ref(GvGP(sstr));
    if (SvTAINTED(sstr))
	SvTAINT(dstr);
    if (GvIMPORTED(dstr) != GVf_IMPORTED
	&& CopSTASH_ne(PL_curcop, GvSTASH(dstr)))
	{
	    GvIMPORTED_on(dstr);
	}
    GvMULTI_on(dstr);
    if(mro_changes == 2) mro_isa_changed_in(GvSTASH(dstr));
    else if(mro_changes) mro_method_changed_in(GvSTASH(dstr));
    return;
}

static void
S_glob_assign_ref(pTHX_ SV *const dstr, SV *const sstr)
{
    SV * const sref = SvREFCNT_inc(SvRV(sstr));
    SV *dref = NULL;
    const int intro = GvINTRO(dstr);
    SV **location;
    U8 import_flag = 0;
    const U32 stype = SvTYPE(sref);

    PERL_ARGS_ASSERT_GLOB_ASSIGN_REF;

    if (intro) {
	GvINTRO_off(dstr);	/* one-shot flag */
	GvLINE(dstr) = CopLINE(PL_curcop);
	GvEGV(dstr) = MUTABLE_GV(dstr);
    }
    GvMULTI_on(dstr);
    switch (stype) {
    case SVt_PVCV:
	location = (SV **) &GvCV(dstr);
	import_flag = GVf_IMPORTED_CV;
	goto common;
    case SVt_PVHV:
	location = (SV **) &GvHV(dstr);
	import_flag = GVf_IMPORTED_HV;
	goto common;
    case SVt_PVAV:
	location = (SV **) &GvAV(dstr);
	import_flag = GVf_IMPORTED_AV;
	goto common;
    case SVt_PVIO:
	location = (SV **) &GvIOp(dstr);
	goto common;
    case SVt_PVFM:
	location = (SV **) &GvFORM(dstr);
	goto common;
    default:
	location = &GvSV(dstr);
	import_flag = GVf_IMPORTED_SV;
    common:
	if (intro) {
	    if (stype == SVt_PVCV) {
		/*if (GvCVGEN(dstr) && (GvCV(dstr) != (const CV *)sref || GvCVGEN(dstr))) {*/
		if (GvCVGEN(dstr)) {
		    SvREFCNT_dec(GvCV(dstr));
		    GvCV(dstr) = NULL;
		    GvCVGEN(dstr) = 0; /* Switch off cacheness. */
		}
	    }
	    SAVEGENERICSV(*location);
	}
	else
	    dref = *location;
	if (stype == SVt_PVCV && (*location != sref || GvCVGEN(dstr))) {
	    CV* const cv = MUTABLE_CV(*location);
	    if (cv) {
		if (!GvCVGEN((const GV *)dstr) &&
		    (CvROOT(cv) || CvXSUB(cv)))
		    {
			/* Redefining a sub - warning is mandatory if
			   it was a const and its value changed. */
			if (CvCONST(cv)	&& CvCONST((const CV *)sref)
			    && cv_const_sv(cv)
			    == cv_const_sv((const CV *)sref)) {
			    NOOP;
			    /* They are 2 constant subroutines generated from
			       the same constant. This probably means that
			       they are really the "same" proxy subroutine
			       instantiated in 2 places. Most likely this is
			       when a constant is exported twice.  Don't warn.
			    */
			}
			else if (ckWARN(WARN_REDEFINE)
				 || (CvCONST(cv)
				     && (!CvCONST((const CV *)sref)
					 || sv_cmp(cv_const_sv(cv),
						   cv_const_sv((const CV *)
							       sref))))) {
			    Perl_warner(aTHX_ packWARN(WARN_REDEFINE),
					(const char *)
					(CvCONST(cv)
					 ? "Constant subroutine %s::%s redefined"
					 : "Subroutine %s::%s redefined"),
					HvNAME_get(GvSTASH((const GV *)dstr)),
					GvENAME(MUTABLE_GV(dstr)));
			}
		    }
		if (!intro)
		    cv_ckproto_len(cv, (const GV *)dstr,
				   SvPOK(sref) ? SvPVX_const(sref) : NULL,
				   SvPOK(sref) ? SvCUR(sref) : 0);
	    }
	    GvCVGEN(dstr) = 0; /* Switch off cacheness. */
	    GvASSUMECV_on(dstr);
	    if(GvSTASH(dstr)) mro_method_changed_in(GvSTASH(dstr)); /* sub foo { 1 } sub bar { 2 } *bar = \&foo */
	}
	*location = sref;
	if (import_flag && !(GvFLAGS(dstr) & import_flag)
	    && CopSTASH_ne(PL_curcop, GvSTASH(dstr))) {
	    GvFLAGS(dstr) |= import_flag;
	}
	if (stype == SVt_PVAV && strEQ(GvNAME((GV*)dstr), "ISA")) {
	    sv_magic(sref, dstr, PERL_MAGIC_isa, NULL, 0);
	    mro_isa_changed_in(GvSTASH(dstr));
	}
	break;
    }
    SvREFCNT_dec(dref);
    if (SvTAINTED(sstr))
	SvTAINT(dstr);
    return;
}

void
Perl_sv_setsv_flags(pTHX_ SV *dstr, register SV* sstr, const I32 flags)
{
    dVAR;
    register U32 sflags;
    register int dtype;
    register svtype stype;

    PERL_ARGS_ASSERT_SV_SETSV_FLAGS;

    if (sstr == dstr)
	return;

    if (SvIS_FREED(dstr)) {
	Perl_croak(aTHX_ "panic: attempt to copy value %" SVf
		   " to a freed scalar %p", SVfARG(sstr), (void *)dstr);
    }
    SV_CHECK_THINKFIRST_COW_DROP(dstr);
    if (!sstr)
	sstr = &PL_sv_undef;
    if (SvIS_FREED(sstr)) {
	Perl_croak(aTHX_ "panic: attempt to copy freed scalar %p to %p",
		   (void*)sstr, (void*)dstr);
    }
    stype = SvTYPE(sstr);
    dtype = SvTYPE(dstr);

    (void)SvAMAGIC_off(dstr);
    if ( SvVOK(dstr) )
    {
	/* need to nuke the magic */
	mg_free(dstr);
    }

    /* There's a lot of redundancy below but we're going for speed here */

    switch (stype) {
    case SVt_NULL:
      undef_sstr:
	if (dtype != SVt_PVGV) {
	    (void)SvOK_off(dstr);
	    return;
	}
	break;
    case SVt_IV:
	if (SvIOK(sstr)) {
	    switch (dtype) {
	    case SVt_NULL:
		sv_upgrade(dstr, SVt_IV);
		break;
	    case SVt_NV:
	    case SVt_PV:
		sv_upgrade(dstr, SVt_PVIV);
		break;
	    case SVt_PVGV:
		goto end_of_first_switch;
	    }
	    (void)SvIOK_only(dstr);
	    SvIV_set(dstr,  SvIVX(sstr));
	    if (SvIsUV(sstr))
		SvIsUV_on(dstr);
	    /* SvTAINTED can only be true if the SV has taint magic, which in
	       turn means that the SV type is PVMG (or greater). This is the
	       case statement for SVt_IV, so this cannot be true (whatever gcov
	       may say).  */
	    assert(!SvTAINTED(sstr));
	    return;
	}
	if (!SvROK(sstr))
	    goto undef_sstr;
	if (dtype < SVt_PV && dtype != SVt_IV)
	    sv_upgrade(dstr, SVt_IV);
	break;

    case SVt_NV:
	if (SvNOK(sstr)) {
	    switch (dtype) {
	    case SVt_NULL:
	    case SVt_IV:
		sv_upgrade(dstr, SVt_NV);
		break;
	    case SVt_PV:
	    case SVt_PVIV:
		sv_upgrade(dstr, SVt_PVNV);
		break;
	    case SVt_PVGV:
		goto end_of_first_switch;
	    }
	    SvNV_set(dstr, SvNVX(sstr));
	    (void)SvNOK_only(dstr);
	    /* SvTAINTED can only be true if the SV has taint magic, which in
	       turn means that the SV type is PVMG (or greater). This is the
	       case statement for SVt_NV, so this cannot be true (whatever gcov
	       may say).  */
	    assert(!SvTAINTED(sstr));
	    return;
	}
	goto undef_sstr;

    case SVt_PVFM:
#ifdef PERL_OLD_COPY_ON_WRITE
	if ((SvFLAGS(sstr) & CAN_COW_MASK) == CAN_COW_FLAGS) {
	    if (dtype < SVt_PVIV)
		sv_upgrade(dstr, SVt_PVIV);
	    break;
	}
	/* Fall through */
#endif
    case SVt_PV:
	if (dtype < SVt_PV)
	    sv_upgrade(dstr, SVt_PV);
	break;
    case SVt_PVIV:
	if (dtype < SVt_PVIV)
	    sv_upgrade(dstr, SVt_PVIV);
	break;
    case SVt_PVNV:
	if (dtype < SVt_PVNV)
	    sv_upgrade(dstr, SVt_PVNV);
	break;
    default:
	{
	const char * const type = sv_reftype(sstr,0);
	if (PL_op)
	    Perl_croak(aTHX_ "Bizarre copy of %s in %s", type, OP_NAME(PL_op));
	else
	    Perl_croak(aTHX_ "Bizarre copy of %s", type);
	}
	break;

    case SVt_REGEXP:
	if (dtype < SVt_REGEXP)
	    sv_upgrade(dstr, SVt_REGEXP);
	break;

	/* case SVt_BIND: */
    case SVt_PVLV:
    case SVt_PVGV:
	if (isGV_with_GP(sstr) && dtype <= SVt_PVGV) {
	    glob_assign_glob(dstr, sstr, dtype);
	    return;
	}
	/* SvVALID means that this PVGV is playing at being an FBM.  */
	/*FALLTHROUGH*/

    case SVt_PVMG:
	if (SvGMAGICAL(sstr) && (flags & SV_GMAGIC)) {
	    mg_get(sstr);
	    if (SvTYPE(sstr) != stype) {
		stype = SvTYPE(sstr);
		if (isGV_with_GP(sstr) && stype == SVt_PVGV && dtype <= SVt_PVGV) {
		    glob_assign_glob(dstr, sstr, dtype);
		    return;
		}
	    }
	}
	if (stype == SVt_PVLV)
	    SvUPGRADE(dstr, SVt_PVNV);
	else
	    SvUPGRADE(dstr, (svtype)stype);
    }
 end_of_first_switch:

    /* dstr may have been upgraded.  */
    dtype = SvTYPE(dstr);
    sflags = SvFLAGS(sstr);

    if (dtype == SVt_PVCV || dtype == SVt_PVFM) {
	/* Assigning to a subroutine sets the prototype.  */
	if (SvOK(sstr)) {
	    STRLEN len;
	    const char *const ptr = SvPV_const(sstr, len);

            SvGROW(dstr, len + 1);
            Copy(ptr, SvPVX(dstr), len + 1, char);
            SvCUR_set(dstr, len);
	    SvPOK_only(dstr);
	    SvFLAGS(dstr) |= sflags & SVf_UTF8;
	} else {
	    SvOK_off(dstr);
	}
    } else if (dtype == SVt_PVAV || dtype == SVt_PVHV) {
	const char * const type = sv_reftype(dstr,0);
	if (PL_op)
	    Perl_croak(aTHX_ "Cannot copy to %s in %s", type, OP_NAME(PL_op));
	else
	    Perl_croak(aTHX_ "Cannot copy to %s", type);
    } else if (sflags & SVf_ROK) {
	if (isGV_with_GP(dstr) && dtype == SVt_PVGV
	    && SvTYPE(SvRV(sstr)) == SVt_PVGV && isGV_with_GP(SvRV(sstr))) {
	    sstr = SvRV(sstr);
	    if (sstr == dstr) {
		if (GvIMPORTED(dstr) != GVf_IMPORTED
		    && CopSTASH_ne(PL_curcop, GvSTASH(dstr)))
		{
		    GvIMPORTED_on(dstr);
		}
		GvMULTI_on(dstr);
		return;
	    }
	    glob_assign_glob(dstr, sstr, dtype);
	    return;
	}

	if (dtype >= SVt_PV) {
	    if (dtype == SVt_PVGV && isGV_with_GP(dstr)) {
		glob_assign_ref(dstr, sstr);
		return;
	    }
	    if (SvPVX_const(dstr)) {
		SvPV_free(dstr);
		SvLEN_set(dstr, 0);
                SvCUR_set(dstr, 0);
	    }
	}
	(void)SvOK_off(dstr);
	SvRV_set(dstr, SvREFCNT_inc(SvRV(sstr)));
	SvFLAGS(dstr) |= sflags & SVf_ROK;
	assert(!(sflags & SVp_NOK));
	assert(!(sflags & SVp_IOK));
	assert(!(sflags & SVf_NOK));
	assert(!(sflags & SVf_IOK));
    }
    else if (dtype == SVt_PVGV && isGV_with_GP(dstr)) {
	if (!(sflags & SVf_OK)) {
	    Perl_ck_warner(aTHX_ packWARN(WARN_MISC),
			   "Undefined value assigned to typeglob");
	}
	else {
	    GV *gv = gv_fetchsv(sstr, GV_ADD, SVt_PVGV);
	    if (dstr != (const SV *)gv) {
		if (GvGP(dstr))
		    gp_free(MUTABLE_GV(dstr));
		GvGP(dstr) = gp_ref(GvGP(gv));
	    }
	}
    }
    else if (dtype == SVt_REGEXP && stype == SVt_REGEXP) {
	reg_temp_copy((REGEXP*)dstr, (REGEXP*)sstr);
    }
    else if (sflags & SVp_POK) {
        bool isSwipe = 0;

	/*
	 * Check to see if we can just swipe the string.  If so, it's a
	 * possible small lose on short strings, but a big win on long ones.
	 * It might even be a win on short strings if SvPVX_const(dstr)
	 * has to be allocated and SvPVX_const(sstr) has to be freed.
	 * Likewise if we can set up COW rather than doing an actual copy, we
	 * drop to the else clause, as the swipe code and the COW setup code
	 * have much in common.
	 */

	/* Whichever path we take through the next code, we want this true,
	   and doing it now facilitates the COW check.  */
	(void)SvPOK_only(dstr);

	if (
	    /* If we're already COW then this clause is not true, and if COW
	       is allowed then we drop down to the else and make dest COW 
	       with us.  If caller hasn't said that we're allowed to COW
	       shared hash keys then we don't do the COW setup, even if the
	       source scalar is a shared hash key scalar.  */
            (((flags & SV_COW_SHARED_HASH_KEYS)
	       ? (sflags & (SVf_FAKE|SVf_READONLY)) != (SVf_FAKE|SVf_READONLY)
	       : 1 /* If making a COW copy is forbidden then the behaviour we
		       desire is as if the source SV isn't actually already
		       COW, even if it is.  So we act as if the source flags
		       are not COW, rather than actually testing them.  */
	      )
#ifndef PERL_OLD_COPY_ON_WRITE
	     /* The change that added SV_COW_SHARED_HASH_KEYS makes the logic
		when PERL_OLD_COPY_ON_WRITE is defined a little wrong.
		Conceptually PERL_OLD_COPY_ON_WRITE being defined should
		override SV_COW_SHARED_HASH_KEYS, because it means "always COW"
		but in turn, it's somewhat dead code, never expected to go
		live, but more kept as a placeholder on how to do it better
		in a newer implementation.  */
	     /* If we are COW and dstr is a suitable target then we drop down
		into the else and make dest a COW of us.  */
	     || (SvFLAGS(dstr) & CAN_COW_MASK) != CAN_COW_FLAGS
#endif
	     )
            &&
            !(isSwipe =
                 (sflags & SVs_TEMP) &&   /* slated for free anyway? */
                 !(sflags & SVf_OOK) &&   /* and not involved in OOK hack? */
	         (!(flags & SV_NOSTEAL)) &&
					/* and we're allowed to steal temps */
                 SvREFCNT(sstr) == 1 &&   /* and no other references to it? */
                 SvLEN(sstr) 	&&	  /* and really is a string */
	    			/* and won't be needed again, potentially */
	      !(PL_op && PL_op->op_type == OP_AASSIGN))
#ifdef PERL_OLD_COPY_ON_WRITE
            && ((flags & SV_COW_SHARED_HASH_KEYS)
		? (!((sflags & CAN_COW_MASK) == CAN_COW_FLAGS
		     && (SvFLAGS(dstr) & CAN_COW_MASK) == CAN_COW_FLAGS
		     && SvTYPE(sstr) >= SVt_PVIV && SvTYPE(sstr) != SVt_PVFM))
		: 1)
#endif
            ) {
            /* Failed the swipe test, and it's not a shared hash key either.
               Have to copy the string.  */
	    STRLEN len = SvCUR(sstr);
            SvGROW(dstr, len + 1);	/* inlined from sv_setpvn */
            Move(SvPVX_const(sstr),SvPVX(dstr),len,char);
            SvCUR_set(dstr, len);
            *SvEND(dstr) = '\0';
        } else {
            /* If PERL_OLD_COPY_ON_WRITE is not defined, then isSwipe will always
               be true in here.  */
            /* Either it's a shared hash key, or it's suitable for
               copy-on-write or we can swipe the string.  */
            if (DEBUG_C_TEST) {
                PerlIO_printf(Perl_debug_log, "Copy on write: sstr --> dstr\n");
                sv_dump(sstr);
                sv_dump(dstr);
            }
#ifdef PERL_OLD_COPY_ON_WRITE
            if (!isSwipe) {
                if ((sflags & (SVf_FAKE | SVf_READONLY))
                    != (SVf_FAKE | SVf_READONLY)) {
                    SvREADONLY_on(sstr);
                    SvFAKE_on(sstr);
                    /* Make the source SV into a loop of 1.
                       (about to become 2) */
                    SV_COW_NEXT_SV_SET(sstr, sstr);
                }
            }
#endif
            /* Initial code is common.  */
	    if (SvPVX_const(dstr)) {	/* we know that dtype >= SVt_PV */
		SvPV_free(dstr);
	    }

            if (!isSwipe) {
                /* making another shared SV.  */
                STRLEN cur = SvCUR(sstr);
                STRLEN len = SvLEN(sstr);
#ifdef PERL_OLD_COPY_ON_WRITE
                if (len) {
		    assert (SvTYPE(dstr) >= SVt_PVIV);
                    /* SvIsCOW_normal */
                    /* splice us in between source and next-after-source.  */
                    SV_COW_NEXT_SV_SET(dstr, SV_COW_NEXT_SV(sstr));
                    SV_COW_NEXT_SV_SET(sstr, dstr);
                    SvPV_set(dstr, SvPVX_mutable(sstr));
                } else
#endif
		{
                    /* SvIsCOW_shared_hash */
                    DEBUG_C(PerlIO_printf(Perl_debug_log,
                                          "Copy on write: Sharing hash\n"));

		    assert (SvTYPE(dstr) >= SVt_PV);
                    SvPV_set(dstr,
			     HEK_KEY(share_hek_hek(SvSHARED_HEK_FROM_PV(SvPVX_const(sstr)))));
		}
                SvLEN_set(dstr, len);
                SvCUR_set(dstr, cur);
                SvREADONLY_on(dstr);
                SvFAKE_on(dstr);
            }
            else
                {	/* Passes the swipe test.  */
                SvPV_set(dstr, SvPVX_mutable(sstr));
                SvLEN_set(dstr, SvLEN(sstr));
                SvCUR_set(dstr, SvCUR(sstr));

                SvTEMP_off(dstr);
                (void)SvOK_off(sstr);	/* NOTE: nukes most SvFLAGS on sstr */
                SvPV_set(sstr, NULL);
                SvLEN_set(sstr, 0);
                SvCUR_set(sstr, 0);
                SvTEMP_off(sstr);
            }
        }
	if (sflags & SVp_NOK) {
	    SvNV_set(dstr, SvNVX(sstr));
	}
	if (sflags & SVp_IOK) {
	    SvIV_set(dstr, SvIVX(sstr));
	    /* Must do this otherwise some other overloaded use of 0x80000000
	       gets confused. I guess SVpbm_VALID */
	    if (sflags & SVf_IVisUV)
		SvIsUV_on(dstr);
	}
	SvFLAGS(dstr) |= sflags & (SVf_IOK|SVp_IOK|SVf_NOK|SVp_NOK|SVf_UTF8);
	{
	    const MAGIC * const smg = SvVSTRING_mg(sstr);
	    if (smg) {
		sv_magic(dstr, NULL, PERL_MAGIC_vstring,
			 smg->mg_ptr, smg->mg_len);
		SvRMAGICAL_on(dstr);
	    }
	}
    }
    else if (sflags & (SVp_IOK|SVp_NOK)) {
	(void)SvOK_off(dstr);
	SvFLAGS(dstr) |= sflags & (SVf_IOK|SVp_IOK|SVf_IVisUV|SVf_NOK|SVp_NOK);
	if (sflags & SVp_IOK) {
	    /* XXXX Do we want to set IsUV for IV(ROK)?  Be extra safe... */
	    SvIV_set(dstr, SvIVX(sstr));
	}
	if (sflags & SVp_NOK) {
	    SvNV_set(dstr, SvNVX(sstr));
	}
    }
    else {
	if (isGV_with_GP(sstr)) {
	    /* This stringification rule for globs is spread in 3 places.
	       This feels bad. FIXME.  */
	    const U32 wasfake = sflags & SVf_FAKE;

	    /* FAKE globs can get coerced, so need to turn this off
	       temporarily if it is on.  */
	    SvFAKE_off(sstr);
	    gv_efullname3(dstr, MUTABLE_GV(sstr), "*");
	    SvFLAGS(sstr) |= wasfake;
	}
	else
	    (void)SvOK_off(dstr);
    }
    if (SvTAINTED(sstr))
	SvTAINT(dstr);
}

/*
=for apidoc sv_setsv_mg

Like C<sv_setsv>, but also handles 'set' magic.

=cut
*/

void
Perl_sv_setsv_mg(pTHX_ SV *const dstr, register SV *const sstr)
{
    PERL_ARGS_ASSERT_SV_SETSV_MG;

    sv_setsv(dstr,sstr);
    SvSETMAGIC(dstr);
}

#ifdef PERL_OLD_COPY_ON_WRITE
SV *
Perl_sv_setsv_cow(pTHX_ SV *dstr, SV *sstr)
{
    STRLEN cur = SvCUR(sstr);
    STRLEN len = SvLEN(sstr);
    register char *new_pv;

    PERL_ARGS_ASSERT_SV_SETSV_COW;

    if (DEBUG_C_TEST) {
	PerlIO_printf(Perl_debug_log, "Fast copy on write: %p -> %p\n",
		      (void*)sstr, (void*)dstr);
	sv_dump(sstr);
	if (dstr)
		    sv_dump(dstr);
    }

    if (dstr) {
	if (SvTHINKFIRST(dstr))
	    sv_force_normal_flags(dstr, SV_COW_DROP_PV);
	else if (SvPVX_const(dstr))
	    Safefree(SvPVX_const(dstr));
    }
    else
	new_SV(dstr);
    SvUPGRADE(dstr, SVt_PVIV);

    assert (SvPOK(sstr));
    assert (SvPOKp(sstr));
    assert (!SvIOK(sstr));
    assert (!SvIOKp(sstr));
    assert (!SvNOK(sstr));
    assert (!SvNOKp(sstr));

    if (SvIsCOW(sstr)) {

	if (SvLEN(sstr) == 0) {
	    /* source is a COW shared hash key.  */
	    DEBUG_C(PerlIO_printf(Perl_debug_log,
				  "Fast copy on write: Sharing hash\n"));
	    new_pv = HEK_KEY(share_hek_hek(SvSHARED_HEK_FROM_PV(SvPVX_const(sstr))));
	    goto common_exit;
	}
	SV_COW_NEXT_SV_SET(dstr, SV_COW_NEXT_SV(sstr));
    } else {
	assert ((SvFLAGS(sstr) & CAN_COW_MASK) == CAN_COW_FLAGS);
	SvUPGRADE(sstr, SVt_PVIV);
	SvREADONLY_on(sstr);
	SvFAKE_on(sstr);
	DEBUG_C(PerlIO_printf(Perl_debug_log,
			      "Fast copy on write: Converting sstr to COW\n"));
	SV_COW_NEXT_SV_SET(dstr, sstr);
    }
    SV_COW_NEXT_SV_SET(sstr, dstr);
    new_pv = SvPVX_mutable(sstr);

  common_exit:
    SvPV_set(dstr, new_pv);
    SvFLAGS(dstr) = (SVt_PVIV|SVf_POK|SVp_POK|SVf_FAKE|SVf_READONLY);
    if (SvUTF8(sstr))
	SvUTF8_on(dstr);
    SvLEN_set(dstr, len);
    SvCUR_set(dstr, cur);
    if (DEBUG_C_TEST) {
	sv_dump(dstr);
    }
    return dstr;
}
#endif

/*
=for apidoc sv_setpvn

Copies a string into an SV.  The C<len> parameter indicates the number of
bytes to be copied.  If the C<ptr> argument is NULL the SV will become
undefined.  Does not handle 'set' magic.  See C<sv_setpvn_mg>.

=cut
*/

void
Perl_sv_setpvn(pTHX_ register SV *const sv, register const char *const ptr, register const STRLEN len)
{
    dVAR;
    register char *dptr;

    PERL_ARGS_ASSERT_SV_SETPVN;

    SV_CHECK_THINKFIRST_COW_DROP(sv);
    if (!ptr) {
	(void)SvOK_off(sv);
	return;
    }
    else {
        /* len is STRLEN which is unsigned, need to copy to signed */
	const IV iv = len;
	if (iv < 0)
	    Perl_croak(aTHX_ "panic: sv_setpvn called with negative strlen");
    }
    SvUPGRADE(sv, SVt_PV);

    dptr = SvGROW(sv, len + 1);
    Move(ptr,dptr,len,char);
    dptr[len] = '\0';
    SvCUR_set(sv, len);
    (void)SvPOK_only_UTF8(sv);		/* validate pointer */
    SvTAINT(sv);
}

/*
=for apidoc sv_setpvn_mg

Like C<sv_setpvn>, but also handles 'set' magic.

=cut
*/

void
Perl_sv_setpvn_mg(pTHX_ register SV *const sv, register const char *const ptr, register const STRLEN len)
{
    PERL_ARGS_ASSERT_SV_SETPVN_MG;

    sv_setpvn(sv,ptr,len);
    SvSETMAGIC(sv);
}

/*
=for apidoc sv_setpv

Copies a string into an SV.  The string must be null-terminated.  Does not
handle 'set' magic.  See C<sv_setpv_mg>.

=cut
*/

void
Perl_sv_setpv(pTHX_ register SV *const sv, register const char *const ptr)
{
    dVAR;
    register STRLEN len;

    PERL_ARGS_ASSERT_SV_SETPV;

    SV_CHECK_THINKFIRST_COW_DROP(sv);
    if (!ptr) {
	(void)SvOK_off(sv);
	return;
    }
    len = strlen(ptr);
    SvUPGRADE(sv, SVt_PV);

    SvGROW(sv, len + 1);
    Move(ptr,SvPVX(sv),len+1,char);
    SvCUR_set(sv, len);
    (void)SvPOK_only_UTF8(sv);		/* validate pointer */
    SvTAINT(sv);
}

/*
=for apidoc sv_setpv_mg

Like C<sv_setpv>, but also handles 'set' magic.

=cut
*/

void
Perl_sv_setpv_mg(pTHX_ register SV *const sv, register const char *const ptr)
{
    PERL_ARGS_ASSERT_SV_SETPV_MG;

    sv_setpv(sv,ptr);
    SvSETMAGIC(sv);
}

/*
=for apidoc sv_usepvn_flags

Tells an SV to use C<ptr> to find its string value.  Normally the
string is stored inside the SV but sv_usepvn allows the SV to use an
outside string.  The C<ptr> should point to memory that was allocated
by C<malloc>.  The string length, C<len>, must be supplied.  By default
this function will realloc (i.e. move) the memory pointed to by C<ptr>,
so that pointer should not be freed or used by the programmer after
giving it to sv_usepvn, and neither should any pointers from "behind"
that pointer (e.g. ptr + 1) be used.

If C<flags> & SV_SMAGIC is true, will call SvSETMAGIC. If C<flags> &
SV_HAS_TRAILING_NUL is true, then C<ptr[len]> must be NUL, and the realloc
will be skipped. (i.e. the buffer is actually at least 1 byte longer than
C<len>, and already meets the requirements for storing in C<SvPVX>)

=cut
*/

void
Perl_sv_usepvn_flags(pTHX_ SV *const sv, char *ptr, const STRLEN len, const U32 flags)
{
    dVAR;
    STRLEN allocate;

    PERL_ARGS_ASSERT_SV_USEPVN_FLAGS;

    SV_CHECK_THINKFIRST_COW_DROP(sv);
    SvUPGRADE(sv, SVt_PV);
    if (!ptr) {
	(void)SvOK_off(sv);
	if (flags & SV_SMAGIC)
	    SvSETMAGIC(sv);
	return;
    }
    if (SvPVX_const(sv))
	SvPV_free(sv);

#ifdef DEBUGGING
    if (flags & SV_HAS_TRAILING_NUL)
	assert(ptr[len] == '\0');
#endif

    allocate = (flags & SV_HAS_TRAILING_NUL)
	? len + 1 :
#ifdef Perl_safesysmalloc_size
	len + 1;
#else 
	PERL_STRLEN_ROUNDUP(len + 1);
#endif
    if (flags & SV_HAS_TRAILING_NUL) {
	/* It's long enough - do nothing.
	   Specfically Perl_newCONSTSUB is relying on this.  */
    } else {
#ifdef DEBUGGING
	/* Force a move to shake out bugs in callers.  */
	char *new_ptr = (char*)safemalloc(allocate);
	Copy(ptr, new_ptr, len, char);
	PoisonFree(ptr,len,char);
	Safefree(ptr);
	ptr = new_ptr;
#else
	ptr = (char*) saferealloc (ptr, allocate);
#endif
    }
#ifdef Perl_safesysmalloc_size
    SvLEN_set(sv, Perl_safesysmalloc_size(ptr));
#else
    SvLEN_set(sv, allocate);
#endif
    SvCUR_set(sv, len);
    SvPV_set(sv, ptr);
    if (!(flags & SV_HAS_TRAILING_NUL)) {
	ptr[len] = '\0';
    }
    (void)SvPOK_only_UTF8(sv);		/* validate pointer */
    SvTAINT(sv);
    if (flags & SV_SMAGIC)
	SvSETMAGIC(sv);
}

#ifdef PERL_OLD_COPY_ON_WRITE
/* Need to do this *after* making the SV normal, as we need the buffer
   pointer to remain valid until after we've copied it.  If we let go too early,
   another thread could invalidate it by unsharing last of the same hash key
   (which it can do by means other than releasing copy-on-write Svs)
   or by changing the other copy-on-write SVs in the loop.  */
STATIC void
S_sv_release_COW(pTHX_ register SV *sv, const char *pvx, SV *after)
{
    PERL_ARGS_ASSERT_SV_RELEASE_COW;

    { /* this SV was SvIsCOW_normal(sv) */
         /* we need to find the SV pointing to us.  */
        SV *current = SV_COW_NEXT_SV(after);

        if (current == sv) {
            /* The SV we point to points back to us (there were only two of us
               in the loop.)
               Hence other SV is no longer copy on write either.  */
            SvFAKE_off(after);
            SvREADONLY_off(after);
        } else {
            /* We need to follow the pointers around the loop.  */
            SV *next;
            while ((next = SV_COW_NEXT_SV(current)) != sv) {
                assert (next);
                current = next;
                 /* don't loop forever if the structure is bust, and we have
                    a pointer into a closed loop.  */
                assert (current != after);
                assert (SvPVX_const(current) == pvx);
            }
            /* Make the SV before us point to the SV after us.  */
            SV_COW_NEXT_SV_SET(current, after);
        }
    }
}
#endif
/*
=for apidoc sv_force_normal_flags

Undo various types of fakery on an SV: if the PV is a shared string, make
a private copy; if we're a ref, stop refing; if we're a glob, downgrade to
an xpvmg; if we're a copy-on-write scalar, this is the on-write time when
we do the copy, and is also used locally. If C<SV_COW_DROP_PV> is set
then a copy-on-write scalar drops its PV buffer (if any) and becomes
SvPOK_off rather than making a copy. (Used where this scalar is about to be
set to some other value.) In addition, the C<flags> parameter gets passed to
C<sv_unref_flags()> when unrefing. C<sv_force_normal> calls this function
with flags set to 0.

=cut
*/

void
Perl_sv_force_normal_flags(pTHX_ register SV *const sv, const U32 flags)
{
    dVAR;

    PERL_ARGS_ASSERT_SV_FORCE_NORMAL_FLAGS;

#ifdef PERL_OLD_COPY_ON_WRITE
    if (SvREADONLY(sv)) {
	if (SvFAKE(sv)) {
	    const char * const pvx = SvPVX_const(sv);
	    const STRLEN len = SvLEN(sv);
	    const STRLEN cur = SvCUR(sv);
	    /* next COW sv in the loop.  If len is 0 then this is a shared-hash
	       key scalar, so we mustn't attempt to call SV_COW_NEXT_SV(), as
	       we'll fail an assertion.  */
	    SV * const next = len ? SV_COW_NEXT_SV(sv) : 0;

            if (DEBUG_C_TEST) {
                PerlIO_printf(Perl_debug_log,
                              "Copy on write: Force normal %ld\n",
                              (long) flags);
                sv_dump(sv);
            }
            SvFAKE_off(sv);
            SvREADONLY_off(sv);
            /* This SV doesn't own the buffer, so need to Newx() a new one:  */
            SvPV_set(sv, NULL);
            SvLEN_set(sv, 0);
            if (flags & SV_COW_DROP_PV) {
                /* OK, so we don't need to copy our buffer.  */
                SvPOK_off(sv);
            } else {
                SvGROW(sv, cur + 1);
                Move(pvx,SvPVX(sv),cur,char);
                SvCUR_set(sv, cur);
                *SvEND(sv) = '\0';
            }
	    if (len) {
		sv_release_COW(sv, pvx, next);
	    } else {
		unshare_hek(SvSHARED_HEK_FROM_PV(pvx));
	    }
            if (DEBUG_C_TEST) {
                sv_dump(sv);
            }
	}
	else if (IN_PERL_RUNTIME)
	    Perl_croak(aTHX_ "%s", PL_no_modify);
    }
#else
    if (SvREADONLY(sv)) {
	if (SvFAKE(sv)) {
	    const char * const pvx = SvPVX_const(sv);
	    const STRLEN len = SvCUR(sv);
	    SvFAKE_off(sv);
	    SvREADONLY_off(sv);
	    SvPV_set(sv, NULL);
	    SvLEN_set(sv, 0);
	    SvGROW(sv, len + 1);
	    Move(pvx,SvPVX(sv),len,char);
	    *SvEND(sv) = '\0';
	    unshare_hek(SvSHARED_HEK_FROM_PV(pvx));
	}
	else if (IN_PERL_RUNTIME)
	    Perl_croak(aTHX_ "%s", PL_no_modify);
    }
#endif
    if (SvROK(sv))
	sv_unref_flags(sv, flags);
    else if (SvFAKE(sv) && SvTYPE(sv) == SVt_PVGV)
	sv_unglob(sv);
    else if (SvFAKE(sv) && SvTYPE(sv) == SVt_REGEXP) {
	/* Need to downgrade the REGEXP to a simple(r) scalar. This is analagous
	   to sv_unglob. We only need it here, so inline it.  */
	const svtype new_type = SvMAGIC(sv) || SvSTASH(sv) ? SVt_PVMG : SVt_PV;
	SV *const temp = newSV_type(new_type);
	void *const temp_p = SvANY(sv);

	if (new_type == SVt_PVMG) {
	    SvMAGIC_set(temp, SvMAGIC(sv));
	    SvMAGIC_set(sv, NULL);
	    SvSTASH_set(temp, SvSTASH(sv));
	    SvSTASH_set(sv, NULL);
	}
	SvCUR_set(temp, SvCUR(sv));
	/* Remember that SvPVX is in the head, not the body. */
	if (SvLEN(temp)) {
	    SvLEN_set(temp, SvLEN(sv));
	    /* This signals "buffer is owned by someone else" in sv_clear,
	       which is the least effort way to stop it freeing the buffer.
	    */
	    SvLEN_set(sv, SvLEN(sv)+1);
	} else {
	    /* Their buffer is already owned by someone else. */
	    SvPVX(sv) = savepvn(SvPVX(sv), SvCUR(sv));
	    SvLEN_set(temp, SvCUR(sv)+1);
	}

	/* Now swap the rest of the bodies. */

	SvFLAGS(sv) &= ~(SVf_FAKE|SVTYPEMASK);
	SvFLAGS(sv) |= new_type;
	SvANY(sv) = SvANY(temp);

	SvFLAGS(temp) &= ~(SVTYPEMASK);
	SvFLAGS(temp) |= SVt_REGEXP|SVf_FAKE;
	SvANY(temp) = temp_p;

	SvREFCNT_dec(temp);
    }
}

/*
=for apidoc sv_chop

Efficient removal of characters from the beginning of the string buffer.
SvPOK(sv) must be true and the C<ptr> must be a pointer to somewhere inside
the string buffer.  The C<ptr> becomes the first character of the adjusted
string. Uses the "OOK hack".
Beware: after this function returns, C<ptr> and SvPVX_const(sv) may no longer
refer to the same chunk of data.

=cut
*/

void
Perl_sv_chop(pTHX_ register SV *const sv, register const char *const ptr)
{
    STRLEN delta;
    STRLEN old_delta;
    U8 *p;
#ifdef DEBUGGING
    const U8 *real_start;
#endif
    STRLEN max_delta;

    PERL_ARGS_ASSERT_SV_CHOP;

    if (!ptr || !SvPOKp(sv))
	return;
    delta = ptr - SvPVX_const(sv);
    if (!delta) {
	/* Nothing to do.  */
	return;
    }
    /* SvPVX(sv) may move in SV_CHECK_THINKFIRST(sv), but after this line,
       nothing uses the value of ptr any more.  */
    max_delta = SvLEN(sv) ? SvLEN(sv) : SvCUR(sv);
    if (ptr <= SvPVX_const(sv))
	Perl_croak(aTHX_ "panic: sv_chop ptr=%p, start=%p, end=%p",
		   ptr, SvPVX_const(sv), SvPVX_const(sv) + max_delta);
    SV_CHECK_THINKFIRST(sv);
    if (delta > max_delta)
	Perl_croak(aTHX_ "panic: sv_chop ptr=%p (was %p), start=%p, end=%p",
		   SvPVX_const(sv) + delta, ptr, SvPVX_const(sv),
		   SvPVX_const(sv) + max_delta);

    if (!SvOOK(sv)) {
	if (!SvLEN(sv)) { /* make copy of shared string */
	    const char *pvx = SvPVX_const(sv);
	    const STRLEN len = SvCUR(sv);
	    SvGROW(sv, len + 1);
	    Move(pvx,SvPVX(sv),len,char);
	    *SvEND(sv) = '\0';
	}
	SvFLAGS(sv) |= SVf_OOK;
	old_delta = 0;
    } else {
	SvOOK_offset(sv, old_delta);
    }
    SvLEN_set(sv, SvLEN(sv) - delta);
    SvCUR_set(sv, SvCUR(sv) - delta);
    SvPV_set(sv, SvPVX(sv) + delta);

    p = (U8 *)SvPVX_const(sv);

    delta += old_delta;

#ifdef DEBUGGING
    real_start = p - delta;
#endif

    assert(delta);
    if (delta < 0x100) {
	*--p = (U8) delta;
    } else {
	*--p = 0;
	p -= sizeof(STRLEN);
	Copy((U8*)&delta, p, sizeof(STRLEN), U8);
    }

#ifdef DEBUGGING
    /* Fill the preceding buffer with sentinals to verify that no-one is
       using it.  */
    while (p > real_start) {
	--p;
	*p = (U8)PTR2UV(p);
    }
#endif
}

/*
=for apidoc sv_catpvn

Concatenates the string onto the end of the string which is in the SV.  The
C<len> indicates number of bytes to copy.  If the SV has the UTF-8
status set, then the bytes appended should be valid UTF-8.
Handles 'get' magic, but not 'set' magic.  See C<sv_catpvn_mg>.

=for apidoc sv_catpvn_flags

Concatenates the string onto the end of the string which is in the SV.  The
C<len> indicates number of bytes to copy.  If the SV has the UTF-8
status set, then the bytes appended should be valid UTF-8.
If C<flags> has C<SV_GMAGIC> bit set, will C<mg_get> on C<dsv> if
appropriate, else not. C<sv_catpvn> and C<sv_catpvn_nomg> are implemented
in terms of this function.

=cut
*/

void
Perl_sv_catpvn_flags(pTHX_ register SV *const dsv, register const char *sstr, register const STRLEN slen, const I32 flags)
{
    dVAR;
    STRLEN dlen;
    const char * const dstr = SvPV_force_flags(dsv, dlen, flags);

    PERL_ARGS_ASSERT_SV_CATPVN_FLAGS;

    SvGROW(dsv, dlen + slen + 1);
    if (sstr == dstr)
	sstr = SvPVX_const(dsv);
    Move(sstr, SvPVX(dsv) + dlen, slen, char);
    SvCUR_set(dsv, SvCUR(dsv) + slen);
    *SvEND(dsv) = '\0';
    (void)SvPOK_only_UTF8(dsv);		/* validate pointer */
    SvTAINT(dsv);
    if (flags & SV_SMAGIC)
	SvSETMAGIC(dsv);
}

/*
=for apidoc sv_catsv

Concatenates the string from SV C<ssv> onto the end of the string in
SV C<dsv>.  Modifies C<dsv> but not C<ssv>.  Handles 'get' magic, but
not 'set' magic.  See C<sv_catsv_mg>.

=for apidoc sv_catsv_flags

Concatenates the string from SV C<ssv> onto the end of the string in
SV C<dsv>.  Modifies C<dsv> but not C<ssv>.  If C<flags> has C<SV_GMAGIC>
bit set, will C<mg_get> on the SVs if appropriate, else not. C<sv_catsv>
and C<sv_catsv_nomg> are implemented in terms of this function.

=cut */

void
Perl_sv_catsv_flags(pTHX_ SV *const dsv, register SV *const ssv, const I32 flags)
{
    dVAR;
 
    PERL_ARGS_ASSERT_SV_CATSV_FLAGS;

   if (ssv) {
	STRLEN slen;
	const char *spv = SvPV_const(ssv, slen);
	if (spv) {
	    /*  sutf8 and dutf8 were type bool, but under USE_ITHREADS,
		gcc version 2.95.2 20000220 (Debian GNU/Linux) for
		Linux xxx 2.2.17 on sparc64 with gcc -O2, we erroneously
		get dutf8 = 0x20000000, (i.e.  SVf_UTF8) even though
		dsv->sv_flags doesn't have that bit set.
		Andy Dougherty  12 Oct 2001
	    */
	    const I32 sutf8 = DO_UTF8(ssv);
	    I32 dutf8;

	    if (SvGMAGICAL(dsv) && (flags & SV_GMAGIC))
		mg_get(dsv);
	    dutf8 = DO_UTF8(dsv);

	    if (dutf8 != sutf8) {
		if (dutf8) {
		    /* Not modifying source SV, so taking a temporary copy. */
		    SV* const csv = newSVpvn_flags(spv, slen, SVs_TEMP);

		    sv_utf8_upgrade(csv);
		    spv = SvPV_const(csv, slen);
		}
		else
		    /* Leave enough space for the cat that's about to happen */
		    sv_utf8_upgrade_flags_grow(dsv, 0, slen);
	    }
	    sv_catpvn_nomg(dsv, spv, slen);
	}
    }
    if (flags & SV_SMAGIC)
	SvSETMAGIC(dsv);
}

/*
=for apidoc sv_catpv

Concatenates the string onto the end of the string which is in the SV.
If the SV has the UTF-8 status set, then the bytes appended should be
valid UTF-8.  Handles 'get' magic, but not 'set' magic.  See C<sv_catpv_mg>.

=cut */

void
Perl_sv_catpv(pTHX_ register SV *const sv, register const char *ptr)
{
    dVAR;
    register STRLEN len;
    STRLEN tlen;
    char *junk;

    PERL_ARGS_ASSERT_SV_CATPV;

    if (!ptr)
	return;
    junk = SvPV_force(sv, tlen);
    len = strlen(ptr);
    SvGROW(sv, tlen + len + 1);
    if (ptr == junk)
	ptr = SvPVX_const(sv);
    Move(ptr,SvPVX(sv)+tlen,len+1,char);
    SvCUR_set(sv, SvCUR(sv) + len);
    (void)SvPOK_only_UTF8(sv);		/* validate pointer */
    SvTAINT(sv);
}

/*
=for apidoc sv_catpv_mg

Like C<sv_catpv>, but also handles 'set' magic.

=cut
*/

void
Perl_sv_catpv_mg(pTHX_ register SV *const sv, register const char *const ptr)
{
    PERL_ARGS_ASSERT_SV_CATPV_MG;

    sv_catpv(sv,ptr);
    SvSETMAGIC(sv);
}

/*
=for apidoc newSV

Creates a new SV.  A non-zero C<len> parameter indicates the number of
bytes of preallocated string space the SV should have.  An extra byte for a
trailing NUL is also reserved.  (SvPOK is not set for the SV even if string
space is allocated.)  The reference count for the new SV is set to 1.

In 5.9.3, newSV() replaces the older NEWSV() API, and drops the first
parameter, I<x>, a debug aid which allowed callers to identify themselves.
This aid has been superseded by a new build option, PERL_MEM_LOG (see
L<perlhack/PERL_MEM_LOG>).  The older API is still there for use in XS
modules supporting older perls.

=cut
*/

SV *
Perl_newSV(pTHX_ const STRLEN len)
{
    dVAR;
    register SV *sv;

    new_SV(sv);
    if (len) {
	sv_upgrade(sv, SVt_PV);
	SvGROW(sv, len + 1);
    }
    return sv;
}
/*
=for apidoc sv_magicext

Adds magic to an SV, upgrading it if necessary. Applies the
supplied vtable and returns a pointer to the magic added.

Note that C<sv_magicext> will allow things that C<sv_magic> will not.
In particular, you can add magic to SvREADONLY SVs, and add more than
one instance of the same 'how'.

If C<namlen> is greater than zero then a C<savepvn> I<copy> of C<name> is
stored, if C<namlen> is zero then C<name> is stored as-is and - as another
special case - if C<(name && namlen == HEf_SVKEY)> then C<name> is assumed
to contain an C<SV*> and is stored as-is with its REFCNT incremented.

(This is now used as a subroutine by C<sv_magic>.)

=cut
*/
MAGIC *	
Perl_sv_magicext(pTHX_ SV *const sv, SV *const obj, const int how, 
                const MGVTBL *const vtable, const char *const name, const I32 namlen)
{
    dVAR;
    MAGIC* mg;

    PERL_ARGS_ASSERT_SV_MAGICEXT;

    SvUPGRADE(sv, SVt_PVMG);
    Newxz(mg, 1, MAGIC);
    mg->mg_moremagic = SvMAGIC(sv);
    SvMAGIC_set(sv, mg);

    /* Sometimes a magic contains a reference loop, where the sv and
       object refer to each other.  To prevent a reference loop that
       would prevent such objects being freed, we look for such loops
       and if we find one we avoid incrementing the object refcount.

       Note we cannot do this to avoid self-tie loops as intervening RV must
       have its REFCNT incremented to keep it in existence.

    */
    if (!obj || obj == sv ||
	how == PERL_MAGIC_arylen ||
	how == PERL_MAGIC_symtab ||
	(SvTYPE(obj) == SVt_PVGV &&
	    (GvSV(obj) == sv || GvHV(obj) == (const HV *)sv
	     || GvAV(obj) == (const AV *)sv || GvCV(obj) == (const CV *)sv
	     || GvIOp(obj) == (const IO *)sv || GvFORM(obj) == (const CV *)sv)))
    {
	mg->mg_obj = obj;
    }
    else {
	mg->mg_obj = SvREFCNT_inc_simple(obj);
	mg->mg_flags |= MGf_REFCOUNTED;
    }

    /* Normal self-ties simply pass a null object, and instead of
       using mg_obj directly, use the SvTIED_obj macro to produce a
       new RV as needed.  For glob "self-ties", we are tieing the PVIO
       with an RV obj pointing to the glob containing the PVIO.  In
       this case, to avoid a reference loop, we need to weaken the
       reference.
    */

    if (how == PERL_MAGIC_tiedscalar && SvTYPE(sv) == SVt_PVIO &&
        obj && SvROK(obj) && GvIO(SvRV(obj)) == (const IO *)sv)
    {
      sv_rvweaken(obj);
    }

    mg->mg_type = how;
    mg->mg_len = namlen;
    if (name) {
	if (namlen > 0)
	    mg->mg_ptr = savepvn(name, namlen);
	else if (namlen == HEf_SVKEY) {
	    /* Yes, this is casting away const. This is only for the case of
	       HEf_SVKEY. I think we need to document this abberation of the
	       constness of the API, rather than making name non-const, as
	       that change propagating outwards a long way.  */
	    mg->mg_ptr = (char*)SvREFCNT_inc_simple_NN((SV *)name);
	} else
	    mg->mg_ptr = (char *) name;
    }
    mg->mg_virtual = (MGVTBL *) vtable;

    mg_magical(sv);
    if (SvGMAGICAL(sv))
	SvFLAGS(sv) &= ~(SVf_IOK|SVf_NOK|SVf_POK);
    return mg;
}

/*
=for apidoc sv_magic

Adds magic to an SV. First upgrades C<sv> to type C<SVt_PVMG> if necessary,
then adds a new magic item of type C<how> to the head of the magic list.

See C<sv_magicext> (which C<sv_magic> now calls) for a description of the
handling of the C<name> and C<namlen> arguments.

You need to use C<sv_magicext> to add magic to SvREADONLY SVs and also
to add more than one instance of the same 'how'.

=cut
*/

void
Perl_sv_magic(pTHX_ register SV *const sv, SV *const obj, const int how, 
             const char *const name, const I32 namlen)
{
    dVAR;
    const MGVTBL *vtable;
    MAGIC* mg;

    PERL_ARGS_ASSERT_SV_MAGIC;

#ifdef PERL_OLD_COPY_ON_WRITE
    if (SvIsCOW(sv))
        sv_force_normal_flags(sv, 0);
#endif
    if (SvREADONLY(sv)) {
	if (
	    /* its okay to attach magic to shared strings; the subsequent
	     * upgrade to PVMG will unshare the string */
	    !(SvFAKE(sv) && SvTYPE(sv) < SVt_PVMG)

	    && IN_PERL_RUNTIME
	    && how != PERL_MAGIC_regex_global
	    && how != PERL_MAGIC_bm
	    && how != PERL_MAGIC_fm
	    && how != PERL_MAGIC_sv
	    && how != PERL_MAGIC_backref
	   )
	{
	    Perl_croak(aTHX_ "%s", PL_no_modify);
	}
    }
    if (SvMAGICAL(sv) || (how == PERL_MAGIC_taint && SvTYPE(sv) >= SVt_PVMG)) {
	if (SvMAGIC(sv) && (mg = mg_find(sv, how))) {
	    /* sv_magic() refuses to add a magic of the same 'how' as an
	       existing one
	     */
	    if (how == PERL_MAGIC_taint) {
		mg->mg_len |= 1;
		/* Any scalar which already had taint magic on which someone
		   (erroneously?) did SvIOK_on() or similar will now be
		   incorrectly sporting public "OK" flags.  */
		SvFLAGS(sv) &= ~(SVf_IOK|SVf_NOK|SVf_POK);
	    }
	    return;
	}
    }

    switch (how) {
    case PERL_MAGIC_sv:
	vtable = &PL_vtbl_sv;
	break;
    case PERL_MAGIC_overload:
        vtable = &PL_vtbl_amagic;
        break;
    case PERL_MAGIC_overload_elem:
        vtable = &PL_vtbl_amagicelem;
        break;
    case PERL_MAGIC_overload_table:
        vtable = &PL_vtbl_ovrld;
        break;
    case PERL_MAGIC_bm:
	vtable = &PL_vtbl_bm;
	break;
    case PERL_MAGIC_regdata:
	vtable = &PL_vtbl_regdata;
	break;
    case PERL_MAGIC_regdatum:
	vtable = &PL_vtbl_regdatum;
	break;
    case PERL_MAGIC_env:
	vtable = &PL_vtbl_env;
	break;
    case PERL_MAGIC_fm:
	vtable = &PL_vtbl_fm;
	break;
    case PERL_MAGIC_envelem:
	vtable = &PL_vtbl_envelem;
	break;
    case PERL_MAGIC_regex_global:
	vtable = &PL_vtbl_mglob;
	break;
    case PERL_MAGIC_isa:
	vtable = &PL_vtbl_isa;
	break;
    case PERL_MAGIC_isaelem:
	vtable = &PL_vtbl_isaelem;
	break;
    case PERL_MAGIC_nkeys:
	vtable = &PL_vtbl_nkeys;
	break;
    case PERL_MAGIC_dbfile:
	vtable = NULL;
	break;
    case PERL_MAGIC_dbline:
	vtable = &PL_vtbl_dbline;
	break;
#ifdef USE_LOCALE_COLLATE
    case PERL_MAGIC_collxfrm:
        vtable = &PL_vtbl_collxfrm;
        break;
#endif /* USE_LOCALE_COLLATE */
    case PERL_MAGIC_tied:
	vtable = &PL_vtbl_pack;
	break;
    case PERL_MAGIC_tiedelem:
    case PERL_MAGIC_tiedscalar:
	vtable = &PL_vtbl_packelem;
	break;
    case PERL_MAGIC_qr:
	vtable = &PL_vtbl_regexp;
	break;
    case PERL_MAGIC_sig:
	vtable = &PL_vtbl_sig;
	break;
    case PERL_MAGIC_sigelem:
	vtable = &PL_vtbl_sigelem;
	break;
    case PERL_MAGIC_taint:
	vtable = &PL_vtbl_taint;
	break;
    case PERL_MAGIC_uvar:
	vtable = &PL_vtbl_uvar;
	break;
    case PERL_MAGIC_vec:
	vtable = &PL_vtbl_vec;
	break;
    case PERL_MAGIC_arylen_p:
    case PERL_MAGIC_rhash:
    case PERL_MAGIC_symtab:
    case PERL_MAGIC_vstring:
	vtable = NULL;
	break;
    case PERL_MAGIC_utf8:
	vtable = &PL_vtbl_utf8;
	break;
    case PERL_MAGIC_substr:
	vtable = &PL_vtbl_substr;
	break;
    case PERL_MAGIC_defelem:
	vtable = &PL_vtbl_defelem;
	break;
    case PERL_MAGIC_arylen:
	vtable = &PL_vtbl_arylen;
	break;
    case PERL_MAGIC_pos:
	vtable = &PL_vtbl_pos;
	break;
    case PERL_MAGIC_backref:
	vtable = &PL_vtbl_backref;
	break;
    case PERL_MAGIC_hintselem:
	vtable = &PL_vtbl_hintselem;
	break;
    case PERL_MAGIC_hints:
	vtable = &PL_vtbl_hints;
	break;
    case PERL_MAGIC_ext:
	/* Reserved for use by extensions not perl internals.	        */
	/* Useful for attaching extension internal data to perl vars.	*/
	/* Note that multiple extensions may clash if magical scalars	*/
	/* etc holding private data from one are passed to another.	*/
	vtable = NULL;
	break;
    default:
	Perl_croak(aTHX_ "Don't know how to handle magic of type \\%o", how);
    }

    /* Rest of work is done else where */
    mg = sv_magicext(sv,obj,how,vtable,name,namlen);

    switch (how) {
    case PERL_MAGIC_taint:
	mg->mg_len = 1;
	break;
    case PERL_MAGIC_ext:
    case PERL_MAGIC_dbfile:
	SvRMAGICAL_on(sv);
	break;
    }
}

/*
=for apidoc sv_unmagic

Removes all magic of type C<type> from an SV.

=cut
*/

int
Perl_sv_unmagic(pTHX_ SV *const sv, const int type)
{
    MAGIC* mg;
    MAGIC** mgp;

    PERL_ARGS_ASSERT_SV_UNMAGIC;

    if (SvTYPE(sv) < SVt_PVMG || !SvMAGIC(sv))
	return 0;
    mgp = &(((XPVMG*) SvANY(sv))->xmg_u.xmg_magic);
    for (mg = *mgp; mg; mg = *mgp) {
	if (mg->mg_type == type) {
            const MGVTBL* const vtbl = mg->mg_virtual;
	    *mgp = mg->mg_moremagic;
	    if (vtbl && vtbl->svt_free)
		CALL_FPTR(vtbl->svt_free)(aTHX_ sv, mg);
	    if (mg->mg_ptr && mg->mg_type != PERL_MAGIC_regex_global) {
		if (mg->mg_len > 0)
		    Safefree(mg->mg_ptr);
		else if (mg->mg_len == HEf_SVKEY)
		    SvREFCNT_dec(MUTABLE_SV(mg->mg_ptr));
		else if (mg->mg_type == PERL_MAGIC_utf8)
		    Safefree(mg->mg_ptr);
            }
	    if (mg->mg_flags & MGf_REFCOUNTED)
		SvREFCNT_dec(mg->mg_obj);
	    Safefree(mg);
	}
	else
	    mgp = &mg->mg_moremagic;
    }
    if (SvMAGIC(sv)) {
	if (SvMAGICAL(sv))	/* if we're under save_magic, wait for restore_magic; */
	    mg_magical(sv);	/*    else fix the flags now */
    }
    else {
	SvMAGICAL_off(sv);
	SvFLAGS(sv) |= (SvFLAGS(sv) & (SVp_IOK|SVp_NOK|SVp_POK)) >> PRIVSHIFT;
    }
    return 0;
}

/*
=for apidoc sv_rvweaken

Weaken a reference: set the C<SvWEAKREF> flag on this RV; give the
referred-to SV C<PERL_MAGIC_backref> magic if it hasn't already; and
push a back-reference to this RV onto the array of backreferences
associated with that magic. If the RV is magical, set magic will be
called after the RV is cleared.

=cut
*/

SV *
Perl_sv_rvweaken(pTHX_ SV *const sv)
{
    SV *tsv;

    PERL_ARGS_ASSERT_SV_RVWEAKEN;

    if (!SvOK(sv))  /* let undefs pass */
	return sv;
    if (!SvROK(sv))
	Perl_croak(aTHX_ "Can't weaken a nonreference");
    else if (SvWEAKREF(sv)) {
	Perl_ck_warner(aTHX_ packWARN(WARN_MISC), "Reference is already weak");
	return sv;
    }
    tsv = SvRV(sv);
    Perl_sv_add_backref(aTHX_ tsv, sv);
    SvWEAKREF_on(sv);
    SvREFCNT_dec(tsv);
    return sv;
}

/* Give tsv backref magic if it hasn't already got it, then push a
 * back-reference to sv onto the array associated with the backref magic.
 */

/* A discussion about the backreferences array and its refcount:
 *
 * The AV holding the backreferences is pointed to either as the mg_obj of
 * PERL_MAGIC_backref, or in the specific case of a HV that has the hv_aux
 * structure, from the xhv_backreferences field. (A HV without hv_aux will
 * have the standard magic instead.) The array is created with a refcount
 * of 2. This means that if during global destruction the array gets
 * picked on first to have its refcount decremented by the random zapper,
 * it won't actually be freed, meaning it's still theere for when its
 * parent gets freed.
 * When the parent SV is freed, in the case of magic, the magic is freed,
 * Perl_magic_killbackrefs is called which decrements one refcount, then
 * mg_obj is freed which kills the second count.
 * In the vase of a HV being freed, one ref is removed by
 * Perl_hv_kill_backrefs, the other by Perl_sv_kill_backrefs, which it
 * calls.
 */

void
Perl_sv_add_backref(pTHX_ SV *const tsv, SV *const sv)
{
    dVAR;
    AV *av;

    PERL_ARGS_ASSERT_SV_ADD_BACKREF;

    if (SvTYPE(tsv) == SVt_PVHV) {
	AV **const avp = Perl_hv_backreferences_p(aTHX_ MUTABLE_HV(tsv));

	av = *avp;
	if (!av) {
	    /* There is no AV in the offical place - try a fixup.  */
	    MAGIC *const mg = mg_find(tsv, PERL_MAGIC_backref);

	    if (mg) {
		/* Aha. They've got it stowed in magic.  Bring it back.  */
		av = MUTABLE_AV(mg->mg_obj);
		/* Stop mg_free decreasing the refernce count.  */
		mg->mg_obj = NULL;
		/* Stop mg_free even calling the destructor, given that
		   there's no AV to free up.  */
		mg->mg_virtual = 0;
		sv_unmagic(tsv, PERL_MAGIC_backref);
	    } else {
		av = newAV();
		AvREAL_off(av);
		SvREFCNT_inc_simple_void(av); /* see discussion above */
	    }
	    *avp = av;
	}
    } else {
	const MAGIC *const mg
	    = SvMAGICAL(tsv) ? mg_find(tsv, PERL_MAGIC_backref) : NULL;
	if (mg)
	    av = MUTABLE_AV(mg->mg_obj);
	else {
	    av = newAV();
	    AvREAL_off(av);
	    sv_magic(tsv, MUTABLE_SV(av), PERL_MAGIC_backref, NULL, 0);
	    /* av now has a refcnt of 2; see discussion above */
	}
    }
    if (AvFILLp(av) >= AvMAX(av)) {
        av_extend(av, AvFILLp(av)+1);
    }
    AvARRAY(av)[++AvFILLp(av)] = sv; /* av_push() */
}

/* delete a back-reference to ourselves from the backref magic associated
 * with the SV we point to.
 */

STATIC void
S_sv_del_backref(pTHX_ SV *const tsv, SV *const sv)
{
    dVAR;
    AV *av = NULL;
    SV **svp;
    I32 i;

    PERL_ARGS_ASSERT_SV_DEL_BACKREF;

    if (SvTYPE(tsv) == SVt_PVHV && SvOOK(tsv)) {
	av = *Perl_hv_backreferences_p(aTHX_ MUTABLE_HV(tsv));
	/* We mustn't attempt to "fix up" the hash here by moving the
	   backreference array back to the hv_aux structure, as that is stored
	   in the main HvARRAY(), and hfreentries assumes that no-one
	   reallocates HvARRAY() while it is running.  */
    }
    if (!av) {
	const MAGIC *const mg
	    = SvMAGICAL(tsv) ? mg_find(tsv, PERL_MAGIC_backref) : NULL;
	if (mg)
	    av = MUTABLE_AV(mg->mg_obj);
    }

    if (!av)
	Perl_croak(aTHX_ "panic: del_backref");

    assert(!SvIS_FREED(av));

    svp = AvARRAY(av);
    /* We shouldn't be in here more than once, but for paranoia reasons lets
       not assume this.  */
    for (i = AvFILLp(av); i >= 0; i--) {
	if (svp[i] == sv) {
	    const SSize_t fill = AvFILLp(av);
	    if (i != fill) {
		/* We weren't the last entry.
		   An unordered list has this property that you can take the
		   last element off the end to fill the hole, and it's still
		   an unordered list :-)
		*/
		svp[i] = svp[fill];
	    }
	    svp[fill] = NULL;
	    AvFILLp(av) = fill - 1;
	}
    }
}

int
Perl_sv_kill_backrefs(pTHX_ SV *const sv, AV *const av)
{
    SV **svp = AvARRAY(av);

    PERL_ARGS_ASSERT_SV_KILL_BACKREFS;
    PERL_UNUSED_ARG(sv);

    assert(!svp || !SvIS_FREED(av));
    if (svp) {
	SV *const *const last = svp + AvFILLp(av);

	while (svp <= last) {
	    if (*svp) {
		SV *const referrer = *svp;
		if (SvWEAKREF(referrer)) {
		    /* XXX Should we check that it hasn't changed? */
		    SvRV_set(referrer, 0);
		    SvOK_off(referrer);
		    SvWEAKREF_off(referrer);
		    SvSETMAGIC(referrer);
		} else if (SvTYPE(referrer) == SVt_PVGV ||
			   SvTYPE(referrer) == SVt_PVLV) {
		    /* You lookin' at me?  */
		    assert(GvSTASH(referrer));
		    assert(GvSTASH(referrer) == (const HV *)sv);
		    GvSTASH(referrer) = 0;
		} else {
		    Perl_croak(aTHX_
			       "panic: magic_killbackrefs (flags=%"UVxf")",
			       (UV)SvFLAGS(referrer));
		}

		*svp = NULL;
	    }
	    svp++;
	}
    }
    SvREFCNT_dec(av); /* remove extra count added by sv_add_backref() */
    return 0;
}

/*
=for apidoc sv_insert

Inserts a string at the specified offset/length within the SV. Similar to
the Perl substr() function. Handles get magic.

=for apidoc sv_insert_flags

Same as C<sv_insert>, but the extra C<flags> are passed the C<SvPV_force_flags> that applies to C<bigstr>.

=cut
*/

void
Perl_sv_insert_flags(pTHX_ SV *const bigstr, const STRLEN offset, const STRLEN len, const char *const little, const STRLEN littlelen, const U32 flags)
{
    dVAR;
    register char *big;
    register char *mid;
    register char *midend;
    register char *bigend;
    register I32 i;
    STRLEN curlen;

    PERL_ARGS_ASSERT_SV_INSERT_FLAGS;

    if (!bigstr)
	Perl_croak(aTHX_ "Can't modify non-existent substring");
    SvPV_force_flags(bigstr, curlen, flags);
    (void)SvPOK_only_UTF8(bigstr);
    if (offset + len > curlen) {
	SvGROW(bigstr, offset+len+1);
	Zero(SvPVX(bigstr)+curlen, offset+len-curlen, char);
	SvCUR_set(bigstr, offset+len);
    }

    SvTAINT(bigstr);
    i = littlelen - len;
    if (i > 0) {			/* string might grow */
	big = SvGROW(bigstr, SvCUR(bigstr) + i + 1);
	mid = big + offset + len;
	midend = bigend = big + SvCUR(bigstr);
	bigend += i;
	*bigend = '\0';
	while (midend > mid)		/* shove everything down */
	    *--bigend = *--midend;
	Move(little,big+offset,littlelen,char);
	SvCUR_set(bigstr, SvCUR(bigstr) + i);
	SvSETMAGIC(bigstr);
	return;
    }
    else if (i == 0) {
	Move(little,SvPVX(bigstr)+offset,len,char);
	SvSETMAGIC(bigstr);
	return;
    }

    big = SvPVX(bigstr);
    mid = big + offset;
    midend = mid + len;
    bigend = big + SvCUR(bigstr);

    if (midend > bigend)
	Perl_croak(aTHX_ "panic: sv_insert");

    if (mid - big > bigend - midend) {	/* faster to shorten from end */
	if (littlelen) {
	    Move(little, mid, littlelen,char);
	    mid += littlelen;
	}
	i = bigend - midend;
	if (i > 0) {
	    Move(midend, mid, i,char);
	    mid += i;
	}
	*mid = '\0';
	SvCUR_set(bigstr, mid - big);
    }
    else if ((i = mid - big)) {	/* faster from front */
	midend -= littlelen;
	mid = midend;
	Move(big, midend - i, i, char);
	sv_chop(bigstr,midend-i);
	if (littlelen)
	    Move(little, mid, littlelen,char);
    }
    else if (littlelen) {
	midend -= littlelen;
	sv_chop(bigstr,midend);
	Move(little,midend,littlelen,char);
    }
    else {
	sv_chop(bigstr,midend);
    }
    SvSETMAGIC(bigstr);
}

/*
=for apidoc sv_replace

Make the first argument a copy of the second, then delete the original.
The target SV physically takes over ownership of the body of the source SV
and inherits its flags; however, the target keeps any magic it owns,
and any magic in the source is discarded.
Note that this is a rather specialist SV copying operation; most of the
time you'll want to use C<sv_setsv> or one of its many macro front-ends.

=cut
*/

void
Perl_sv_replace(pTHX_ register SV *const sv, register SV *const nsv)
{
    dVAR;
    const U32 refcnt = SvREFCNT(sv);

    PERL_ARGS_ASSERT_SV_REPLACE;

    SV_CHECK_THINKFIRST_COW_DROP(sv);
    if (SvREFCNT(nsv) != 1) {
	Perl_croak(aTHX_ "panic: reference miscount on nsv in sv_replace()"
		   " (%" UVuf " != 1)", (UV) SvREFCNT(nsv));
    }
    if (SvMAGICAL(sv)) {
	if (SvMAGICAL(nsv))
	    mg_free(nsv);
	else
	    sv_upgrade(nsv, SVt_PVMG);
	SvMAGIC_set(nsv, SvMAGIC(sv));
	SvFLAGS(nsv) |= SvMAGICAL(sv);
	SvMAGICAL_off(sv);
	SvMAGIC_set(sv, NULL);
    }
    SvREFCNT(sv) = 0;
    sv_clear(sv);
    assert(!SvREFCNT(sv));
#ifdef DEBUG_LEAKING_SCALARS
    sv->sv_flags  = nsv->sv_flags;
    sv->sv_any    = nsv->sv_any;
    sv->sv_refcnt = nsv->sv_refcnt;
    sv->sv_u      = nsv->sv_u;
#else
    StructCopy(nsv,sv,SV);
#endif
    if(SvTYPE(sv) == SVt_IV) {
	SvANY(sv)
	    = (XPVIV*)((char*)&(sv->sv_u.svu_iv) - STRUCT_OFFSET(XPVIV, xiv_iv));
    }
	

#ifdef PERL_OLD_COPY_ON_WRITE
    if (SvIsCOW_normal(nsv)) {
	/* We need to follow the pointers around the loop to make the
	   previous SV point to sv, rather than nsv.  */
	SV *next;
	SV *current = nsv;
	while ((next = SV_COW_NEXT_SV(current)) != nsv) {
	    assert(next);
	    current = next;
	    assert(SvPVX_const(current) == SvPVX_const(nsv));
	}
	/* Make the SV before us point to the SV after us.  */
	if (DEBUG_C_TEST) {
	    PerlIO_printf(Perl_debug_log, "previous is\n");
	    sv_dump(current);
	    PerlIO_printf(Perl_debug_log,
                          "move it from 0x%"UVxf" to 0x%"UVxf"\n",
			  (UV) SV_COW_NEXT_SV(current), (UV) sv);
	}
	SV_COW_NEXT_SV_SET(current, sv);
    }
#endif
    SvREFCNT(sv) = refcnt;
    SvFLAGS(nsv) |= SVTYPEMASK;		/* Mark as freed */
    SvREFCNT(nsv) = 0;
    del_SV(nsv);
}

/*
=for apidoc sv_clear

Clear an SV: call any destructors, free up any memory used by the body,
and free the body itself. The SV's head is I<not> freed, although
its type is set to all 1's so that it won't inadvertently be assumed
to be live during global destruction etc.
This function should only be called when REFCNT is zero. Most of the time
you'll want to call C<sv_free()> (or its macro wrapper C<SvREFCNT_dec>)
instead.

=cut
*/

void
Perl_sv_clear(pTHX_ register SV *const sv)
{
    dVAR;
    const U32 type = SvTYPE(sv);
    const struct body_details *const sv_type_details
	= bodies_by_type + type;
    HV *stash;

    PERL_ARGS_ASSERT_SV_CLEAR;
    assert(SvREFCNT(sv) == 0);
    assert(SvTYPE(sv) != SVTYPEMASK);

    if (type <= SVt_IV) {
	/* See the comment in sv.h about the collusion between this early
	   return and the overloading of the NULL and IV slots in the size
	   table.  */
	if (SvROK(sv)) {
	    SV * const target = SvRV(sv);
	    if (SvWEAKREF(sv))
	        sv_del_backref(target, sv);
	    else
	        SvREFCNT_dec(target);
	}
	SvFLAGS(sv) &= SVf_BREAK;
	SvFLAGS(sv) |= SVTYPEMASK;
	return;
    }

    if (SvOBJECT(sv)) {
	if (PL_defstash &&	/* Still have a symbol table? */
	    SvDESTROYABLE(sv))
	{
	    dSP;
	    HV* stash;
	    do {	
		CV* destructor;
		stash = SvSTASH(sv);
		destructor = StashHANDLER(stash,DESTROY);
		if (destructor
			/* A constant subroutine can have no side effects, so
			   don't bother calling it.  */
			&& !CvCONST(destructor)
			/* Don't bother calling an empty destructor */
			&& (CvISXSUB(destructor)
			|| (CvSTART(destructor)
			    && (CvSTART(destructor)->op_next->op_type != OP_LEAVESUB))))
		{
		    SV* const tmpref = newRV(sv);
	            SvREADONLY_on(tmpref);   /* DESTROY() could be naughty */
		    ENTER;
		    PUSHSTACKi(PERLSI_DESTROY);
		    EXTEND(SP, 2);
		    PUSHMARK(SP);
		    PUSHs(tmpref);
		    PUTBACK;
		    call_sv(MUTABLE_SV(destructor), G_DISCARD|G_EVAL|G_KEEPERR|G_VOID);
		
		
		    POPSTACK;
		    SPAGAIN;
		    LEAVE;
		    if(SvREFCNT(tmpref) < 2) {
		        /* tmpref is not kept alive! */
		        SvREFCNT(sv)--;
			SvRV_set(tmpref, NULL);
			SvROK_off(tmpref);
		    }
		    SvREFCNT_dec(tmpref);
		}
	    } while (SvOBJECT(sv) && SvSTASH(sv) != stash);


	    if (SvREFCNT(sv)) {
		if (PL_in_clean_objs)
		    Perl_croak(aTHX_ "DESTROY created new reference to dead object '%s'",
			  HvNAME_get(stash));
		/* DESTROY gave object new lease on life */
		return;
	    }
	}

	if (SvOBJECT(sv)) {
	    SvREFCNT_dec(SvSTASH(sv));	/* possibly of changed persuasion */
	    SvOBJECT_off(sv);	/* Curse the object. */
	    if (type != SVt_PVIO)
		--PL_sv_objcount;	/* XXX Might want something more general */
	}
    }
    if (type >= SVt_PVMG) {
	if (type == SVt_PVMG && SvPAD_OUR(sv)) {
	    SvREFCNT_dec(SvOURSTASH(sv));
	} else if (SvMAGIC(sv))
	    mg_free(sv);
	if (type == SVt_PVMG && SvPAD_TYPED(sv))
	    SvREFCNT_dec(SvSTASH(sv));
    }
    switch (type) {
	/* case SVt_BIND: */
    case SVt_PVIO:
	if (IoIFP(sv) &&
	    IoIFP(sv) != PerlIO_stdin() &&
	    IoIFP(sv) != PerlIO_stdout() &&
	    IoIFP(sv) != PerlIO_stderr())
	{
	    io_close(MUTABLE_IO(sv), FALSE);
	}
	if (IoDIRP(sv) && !(IoFLAGS(sv) & IOf_FAKE_DIRP))
	    PerlDir_close(IoDIRP(sv));
	IoDIRP(sv) = (DIR*)NULL;
	Safefree(IoTOP_NAME(sv));
	Safefree(IoFMT_NAME(sv));
	Safefree(IoBOTTOM_NAME(sv));
	goto freescalar;
    case SVt_REGEXP:
	/* FIXME for plugins */
	pregfree2((REGEXP*) sv);
	goto freescalar;
    case SVt_PVCV:
    case SVt_PVFM:
	cv_undef(MUTABLE_CV(sv));
	goto freescalar;
    case SVt_PVHV:
	if (PL_last_swash_hv == (const HV *)sv) {
	    PL_last_swash_hv = NULL;
	}
	Perl_hv_kill_backrefs(aTHX_ MUTABLE_HV(sv));
	hv_undef(MUTABLE_HV(sv));
	break;
    case SVt_PVAV:
	if (PL_comppad == MUTABLE_AV(sv)) {
	    PL_comppad = NULL;
	    PL_curpad = NULL;
	}
	av_undef(MUTABLE_AV(sv));
	break;
    case SVt_PVLV:
	if (LvTYPE(sv) == 'T') { /* for tie: return HE to pool */
	    SvREFCNT_dec(HeKEY_sv((HE*)LvTARG(sv)));
	    HeNEXT((HE*)LvTARG(sv)) = PL_hv_fetch_ent_mh;
	    PL_hv_fetch_ent_mh = (HE*)LvTARG(sv);
	}
	else if (LvTYPE(sv) != 't') /* unless tie: unrefcnted fake SV**  */
	    SvREFCNT_dec(LvTARG(sv));
    case SVt_PVGV:
	if (isGV_with_GP(sv)) {
            if(GvCVu((const GV *)sv) && (stash = GvSTASH(MUTABLE_GV(sv)))
	       && HvNAME_get(stash))
                mro_method_changed_in(stash);
	    gp_free(MUTABLE_GV(sv));
	    if (GvNAME_HEK(sv))
		unshare_hek(GvNAME_HEK(sv));
	    /* If we're in a stash, we don't own a reference to it. However it does
	       have a back reference to us, which needs to be cleared.  */
	    if (!SvVALID(sv) && (stash = GvSTASH(sv)))
		    sv_del_backref(MUTABLE_SV(stash), sv);
	}
	/* FIXME. There are probably more unreferenced pointers to SVs in the
	   interpreter struct that we should check and tidy in a similar
	   fashion to this:  */
	if ((const GV *)sv == PL_last_in_gv)
	    PL_last_in_gv = NULL;
    case SVt_PVMG:
    case SVt_PVNV:
    case SVt_PVIV:
    case SVt_PV:
      freescalar:
	/* Don't bother with SvOOK_off(sv); as we're only going to free it.  */
	if (SvOOK(sv)) {
	    STRLEN offset;
	    SvOOK_offset(sv, offset);
	    SvPV_set(sv, SvPVX_mutable(sv) - offset);
	    /* Don't even bother with turning off the OOK flag.  */
	}
	if (SvROK(sv)) {
	    SV * const target = SvRV(sv);
	    if (SvWEAKREF(sv))
	        sv_del_backref(target, sv);
	    else
	        SvREFCNT_dec(target);
	}
#ifdef PERL_OLD_COPY_ON_WRITE
	else if (SvPVX_const(sv)) {
            if (SvIsCOW(sv)) {
                if (DEBUG_C_TEST) {
                    PerlIO_printf(Perl_debug_log, "Copy on write: clear\n");
                    sv_dump(sv);
                }
		if (SvLEN(sv)) {
		    sv_release_COW(sv, SvPVX_const(sv), SV_COW_NEXT_SV(sv));
		} else {
		    unshare_hek(SvSHARED_HEK_FROM_PV(SvPVX_const(sv)));
		}

                SvFAKE_off(sv);
            } else if (SvLEN(sv)) {
                Safefree(SvPVX_const(sv));
            }
	}
#else
	else if (SvPVX_const(sv) && SvLEN(sv))
	    Safefree(SvPVX_mutable(sv));
	else if (SvPVX_const(sv) && SvREADONLY(sv) && SvFAKE(sv)) {
	    unshare_hek(SvSHARED_HEK_FROM_PV(SvPVX_const(sv)));
	    SvFAKE_off(sv);
	}
#endif
	break;
    case SVt_NV:
	break;
    }

    SvFLAGS(sv) &= SVf_BREAK;
    SvFLAGS(sv) |= SVTYPEMASK;

    if (sv_type_details->arena) {
	del_body(((char *)SvANY(sv) + sv_type_details->offset),
		 &PL_body_roots[type]);
    }
    else if (sv_type_details->body_size) {
	my_safefree(SvANY(sv));
    }
}

/*
=for apidoc sv_newref

Increment an SV's reference count. Use the C<SvREFCNT_inc()> wrapper
instead.

=cut
*/

SV *
Perl_sv_newref(pTHX_ SV *const sv)
{
    PERL_UNUSED_CONTEXT;
    if (sv)
	(SvREFCNT(sv))++;
    return sv;
}

/*
=for apidoc sv_free

Decrement an SV's reference count, and if it drops to zero, call
C<sv_clear> to invoke destructors and free up any memory used by
the body; finally, deallocate the SV's head itself.
Normally called via a wrapper macro C<SvREFCNT_dec>.

=cut
*/

void
Perl_sv_free(pTHX_ SV *const sv)
{
    dVAR;
    if (!sv)
	return;
    if (SvREFCNT(sv) == 0) {
	if (SvFLAGS(sv) & SVf_BREAK)
	    /* this SV's refcnt has been artificially decremented to
	     * trigger cleanup */
	    return;
	if (PL_in_clean_all) /* All is fair */
	    return;
	if (SvREADONLY(sv) && SvIMMORTAL(sv)) {
	    /* make sure SvREFCNT(sv)==0 happens very seldom */
	    SvREFCNT(sv) = (~(U32)0)/2;
	    return;
	}
	if (ckWARN_d(WARN_INTERNAL)) {
#ifdef DEBUG_LEAKING_SCALARS_FORK_DUMP
	    Perl_dump_sv_child(aTHX_ sv);
#else
  #ifdef DEBUG_LEAKING_SCALARS
	    sv_dump(sv);
  #endif
#ifdef DEBUG_LEAKING_SCALARS_ABORT
	    if (PL_warnhook == PERL_WARNHOOK_FATAL
		|| ckDEAD(packWARN(WARN_INTERNAL))) {
		/* Don't let Perl_warner cause us to escape our fate:  */
		abort();
	    }
#endif
	    /* This may not return:  */
	    Perl_warner(aTHX_ packWARN(WARN_INTERNAL),
                        "Attempt to free unreferenced scalar: SV 0x%"UVxf
                        pTHX__FORMAT, PTR2UV(sv) pTHX__VALUE);
#endif
	}
#ifdef DEBUG_LEAKING_SCALARS_ABORT
	abort();
#endif
	return;
    }
    if (--(SvREFCNT(sv)) > 0)
	return;
    Perl_sv_free2(aTHX_ sv);
}

void
Perl_sv_free2(pTHX_ SV *const sv)
{
    dVAR;

    PERL_ARGS_ASSERT_SV_FREE2;

#ifdef DEBUGGING
    if (SvTEMP(sv)) {
	Perl_ck_warner_d(aTHX_ packWARN(WARN_DEBUGGING),
			 "Attempt to free temp prematurely: SV 0x%"UVxf
			 pTHX__FORMAT, PTR2UV(sv) pTHX__VALUE);
	return;
    }
#endif
    if (SvREADONLY(sv) && SvIMMORTAL(sv)) {
	/* make sure SvREFCNT(sv)==0 happens very seldom */
	SvREFCNT(sv) = (~(U32)0)/2;
	return;
    }
    sv_clear(sv);
    if (! SvREFCNT(sv))
	del_SV(sv);
}

/*
=for apidoc sv_len

Returns the length of the string in the SV. Handles magic and type
coercion.  See also C<SvCUR>, which gives raw access to the xpv_cur slot.

=cut
*/

STRLEN
Perl_sv_len(pTHX_ register SV *const sv)
{
    STRLEN len;

    if (!sv)
	return 0;

    if (SvGMAGICAL(sv))
	len = mg_length(sv);
    else
        (void)SvPV_const(sv, len);
    return len;
}

/*
=for apidoc sv_len_utf8

Returns the number of characters in the string in an SV, counting wide
UTF-8 bytes as a single character. Handles magic and type coercion.

=cut
*/

/*
 * The length is cached in PERL_MAGIC_utf8, in the mg_len field.  Also the
 * mg_ptr is used, by sv_pos_u2b() and sv_pos_b2u() - see the comments below.
 * (Note that the mg_len is not the length of the mg_ptr field.
 * This allows the cache to store the character length of the string without
 * needing to malloc() extra storage to attach to the mg_ptr.)
 *
 */

STRLEN
Perl_sv_len_utf8(pTHX_ register SV *const sv)
{
    if (!sv)
	return 0;

    if (SvGMAGICAL(sv))
	return mg_length(sv);
    else
    {
	STRLEN len;
	const U8 *s = (U8*)SvPV_const(sv, len);

	if (PL_utf8cache) {
	    STRLEN ulen;
	    MAGIC *mg = SvMAGICAL(sv) ? mg_find(sv, PERL_MAGIC_utf8) : NULL;

	    if (mg && mg->mg_len != -1) {
		ulen = mg->mg_len;
		if (PL_utf8cache < 0) {
		    const STRLEN real = Perl_utf8_length(aTHX_ s, s + len);
		    if (real != ulen) {
			/* Need to turn the assertions off otherwise we may
			   recurse infinitely while printing error messages.
			*/
			SAVEI8(PL_utf8cache);
			PL_utf8cache = 0;
			Perl_croak(aTHX_ "panic: sv_len_utf8 cache %"UVuf
				   " real %"UVuf" for %"SVf,
				   (UV) ulen, (UV) real, SVfARG(sv));
		    }
		}
	    }
	    else {
		ulen = Perl_utf8_length(aTHX_ s, s + len);
		if (!SvREADONLY(sv)) {
		    if (!mg && (SvTYPE(sv) < SVt_PVMG ||
				!(mg = mg_find(sv, PERL_MAGIC_utf8)))) {
			mg = sv_magicext(sv, 0, PERL_MAGIC_utf8,
					 &PL_vtbl_utf8, 0, 0);
		    }
		    assert(mg);
		    mg->mg_len = ulen;
		    /* For now, treat "overflowed" as "still unknown".
		       See RT #72924.  */
		    if (ulen != (STRLEN) mg->mg_len)
			mg->mg_len = -1;
		}
	    }
	    return ulen;
	}
	return Perl_utf8_length(aTHX_ s, s + len);
    }
}

/* Walk forwards to find the byte corresponding to the passed in UTF-8
   offset.  */
static STRLEN
S_sv_pos_u2b_forwards(const U8 *const start, const U8 *const send,
		      STRLEN uoffset)
{
    const U8 *s = start;

    PERL_ARGS_ASSERT_SV_POS_U2B_FORWARDS;

    while (s < send && uoffset--)
	s += UTF8SKIP(s);
    if (s > send) {
	/* This is the existing behaviour. Possibly it should be a croak, as
	   it's actually a bounds error  */
	s = send;
    }
    return s - start;
}

/* Given the length of the string in both bytes and UTF-8 characters, decide
   whether to walk forwards or backwards to find the byte corresponding to
   the passed in UTF-8 offset.  */
static STRLEN
S_sv_pos_u2b_midway(const U8 *const start, const U8 *send,
		      const STRLEN uoffset, const STRLEN uend)
{
    STRLEN backw = uend - uoffset;

    PERL_ARGS_ASSERT_SV_POS_U2B_MIDWAY;

    if (uoffset < 2 * backw) {
	/* The assumption is that going forwards is twice the speed of going
	   forward (that's where the 2 * backw comes from).
	   (The real figure of course depends on the UTF-8 data.)  */
	return sv_pos_u2b_forwards(start, send, uoffset);
    }

    while (backw--) {
	send--;
	while (UTF8_IS_CONTINUATION(*send))
	    send--;
    }
    return send - start;
}

/* For the string representation of the given scalar, find the byte
   corresponding to the passed in UTF-8 offset.  uoffset0 and boffset0
   give another position in the string, *before* the sought offset, which
   (which is always true, as 0, 0 is a valid pair of positions), which should
   help reduce the amount of linear searching.
   If *mgp is non-NULL, it should point to the UTF-8 cache magic, which
   will be used to reduce the amount of linear searching. The cache will be
   created if necessary, and the found value offered to it for update.  */
static STRLEN
S_sv_pos_u2b_cached(pTHX_ SV *const sv, MAGIC **const mgp, const U8 *const start,
		    const U8 *const send, const STRLEN uoffset,
		    STRLEN uoffset0, STRLEN boffset0)
{
    STRLEN boffset = 0; /* Actually always set, but let's keep gcc happy.  */
    bool found = FALSE;

    PERL_ARGS_ASSERT_SV_POS_U2B_CACHED;

    assert (uoffset >= uoffset0);

    if (!SvREADONLY(sv)
	&& PL_utf8cache
	&& (*mgp || (SvTYPE(sv) >= SVt_PVMG &&
		     (*mgp = mg_find(sv, PERL_MAGIC_utf8))))) {
	if ((*mgp)->mg_ptr) {
	    STRLEN *cache = (STRLEN *) (*mgp)->mg_ptr;
	    if (cache[0] == uoffset) {
		/* An exact match. */
		return cache[1];
	    }
	    if (cache[2] == uoffset) {
		/* An exact match. */
		return cache[3];
	    }

	    if (cache[0] < uoffset) {
		/* The cache already knows part of the way.   */
		if (cache[0] > uoffset0) {
		    /* The cache knows more than the passed in pair  */
		    uoffset0 = cache[0];
		    boffset0 = cache[1];
		}
		if ((*mgp)->mg_len != -1) {
		    /* And we know the end too.  */
		    boffset = boffset0
			+ sv_pos_u2b_midway(start + boffset0, send,
					      uoffset - uoffset0,
					      (*mgp)->mg_len - uoffset0);
		} else {
		    boffset = boffset0
			+ sv_pos_u2b_forwards(start + boffset0,
						send, uoffset - uoffset0);
		}
	    }
	    else if (cache[2] < uoffset) {
		/* We're between the two cache entries.  */
		if (cache[2] > uoffset0) {
		    /* and the cache knows more than the passed in pair  */
		    uoffset0 = cache[2];
		    boffset0 = cache[3];
		}

		boffset = boffset0
		    + sv_pos_u2b_midway(start + boffset0,
					  start + cache[1],
					  uoffset - uoffset0,
					  cache[0] - uoffset0);
	    } else {
		boffset = boffset0
		    + sv_pos_u2b_midway(start + boffset0,
					  start + cache[3],
					  uoffset - uoffset0,
					  cache[2] - uoffset0);
	    }
	    found = TRUE;
	}
	else if ((*mgp)->mg_len != -1) {
	    /* If we can take advantage of a passed in offset, do so.  */
	    /* In fact, offset0 is either 0, or less than offset, so don't
	       need to worry about the other possibility.  */
	    boffset = boffset0
		+ sv_pos_u2b_midway(start + boffset0, send,
				      uoffset - uoffset0,
				      (*mgp)->mg_len - uoffset0);
	    found = TRUE;
	}
    }

    if (!found || PL_utf8cache < 0) {
	const STRLEN real_boffset
	    = boffset0 + sv_pos_u2b_forwards(start + boffset0,
					       send, uoffset - uoffset0);

	if (found && PL_utf8cache < 0) {
	    if (real_boffset != boffset) {
		/* Need to turn the assertions off otherwise we may recurse
		   infinitely while printing error messages.  */
		SAVEI8(PL_utf8cache);
		PL_utf8cache = 0;
		Perl_croak(aTHX_ "panic: sv_pos_u2b_cache cache %"UVuf
			   " real %"UVuf" for %"SVf,
			   (UV) boffset, (UV) real_boffset, SVfARG(sv));
	    }
	}
	boffset = real_boffset;
    }

    if (PL_utf8cache)
	utf8_mg_pos_cache_update(sv, mgp, boffset, uoffset, send - start);
    return boffset;
}


/*
=for apidoc sv_pos_u2b_flags

Converts the value pointed to by offsetp from a count of UTF-8 chars from
the start of the string, to a count of the equivalent number of bytes; if
lenp is non-zero, it does the same to lenp, but this time starting from
the offset, rather than from the start of the string. Handles type coercion.
I<flags> is passed to C<SvPV_flags>, and usually should be
C<SV_GMAGIC|SV_CONST_RETURN> to handle magic.

=cut
*/

/*
 * sv_pos_u2b_flags() uses, like sv_pos_b2u(), the mg_ptr of the potential
 * PERL_MAGIC_utf8 of the sv to store the mapping between UTF-8 and
 * byte offsets.  See also the comments of S_utf8_mg_pos_cache_update().
 *
 */

STRLEN
Perl_sv_pos_u2b_flags(pTHX_ SV *const sv, STRLEN uoffset, STRLEN *const lenp,
		      U32 flags)
{
    const U8 *start;
    STRLEN len;
    STRLEN boffset;

    PERL_ARGS_ASSERT_SV_POS_U2B_FLAGS;

    start = (U8*)SvPV_flags(sv, len, flags);
    if (len) {
	const U8 * const send = start + len;
	MAGIC *mg = NULL;
	boffset = sv_pos_u2b_cached(sv, &mg, start, send, uoffset, 0, 0);

	if (lenp) {
	    /* Convert the relative offset to absolute.  */
	    const STRLEN uoffset2 = uoffset + *lenp;
	    const STRLEN boffset2
		= sv_pos_u2b_cached(sv, &mg, start, send, uoffset2,
				      uoffset, boffset) - boffset;

	    *lenp = boffset2;
	}
    } else {
	if (lenp)
	    *lenp = 0;
	boffset = 0;
    }

    return boffset;
}

/*
=for apidoc sv_pos_u2b

Converts the value pointed to by offsetp from a count of UTF-8 chars from
the start of the string, to a count of the equivalent number of bytes; if
lenp is non-zero, it does the same to lenp, but this time starting from
the offset, rather than from the start of the string. Handles magic and
type coercion.

Use C<sv_pos_u2b_flags> in preference, which correctly handles strings longer
than 2Gb.

=cut
*/

/*
 * sv_pos_u2b() uses, like sv_pos_b2u(), the mg_ptr of the potential
 * PERL_MAGIC_utf8 of the sv to store the mapping between UTF-8 and
 * byte offsets.  See also the comments of S_utf8_mg_pos_cache_update().
 *
 */

/* This function is subject to size and sign problems */

void
Perl_sv_pos_u2b(pTHX_ register SV *const sv, I32 *const offsetp, I32 *const lenp)
{
    PERL_ARGS_ASSERT_SV_POS_U2B;

    if (lenp) {
	STRLEN ulen = (STRLEN)*lenp;
	*offsetp = (I32)sv_pos_u2b_flags(sv, (STRLEN)*offsetp, &ulen,
					 SV_GMAGIC|SV_CONST_RETURN);
	*lenp = (I32)ulen;
    } else {
	*offsetp = (I32)sv_pos_u2b_flags(sv, (STRLEN)*offsetp, NULL,
					 SV_GMAGIC|SV_CONST_RETURN);
    }
}

/* Create and update the UTF8 magic offset cache, with the proffered utf8/
   byte length pairing. The (byte) length of the total SV is passed in too,
   as blen, because for some (more esoteric) SVs, the call to SvPV_const()
   may not have updated SvCUR, so we can't rely on reading it directly.

   The proffered utf8/byte length pairing isn't used if the cache already has
   two pairs, and swapping either for the proffered pair would increase the
   RMS of the intervals between known byte offsets.

   The cache itself consists of 4 STRLEN values
   0: larger UTF-8 offset
   1: corresponding byte offset
   2: smaller UTF-8 offset
   3: corresponding byte offset

   Unused cache pairs have the value 0, 0.
   Keeping the cache "backwards" means that the invariant of
   cache[0] >= cache[2] is maintained even with empty slots, which means that
   the code that uses it doesn't need to worry if only 1 entry has actually
   been set to non-zero.  It also makes the "position beyond the end of the
   cache" logic much simpler, as the first slot is always the one to start
   from.   
*/
static void
S_utf8_mg_pos_cache_update(pTHX_ SV *const sv, MAGIC **const mgp, const STRLEN byte,
                           const STRLEN utf8, const STRLEN blen)
{
    STRLEN *cache;

    PERL_ARGS_ASSERT_UTF8_MG_POS_CACHE_UPDATE;

    if (SvREADONLY(sv))
	return;

    if (!*mgp && (SvTYPE(sv) < SVt_PVMG ||
		  !(*mgp = mg_find(sv, PERL_MAGIC_utf8)))) {
	*mgp = sv_magicext(sv, 0, PERL_MAGIC_utf8, (MGVTBL*)&PL_vtbl_utf8, 0,
			   0);
	(*mgp)->mg_len = -1;
    }
    assert(*mgp);

    if (!(cache = (STRLEN *)(*mgp)->mg_ptr)) {
	Newxz(cache, PERL_MAGIC_UTF8_CACHESIZE * 2, STRLEN);
	(*mgp)->mg_ptr = (char *) cache;
    }
    assert(cache);

    if (PL_utf8cache < 0 && SvPOKp(sv)) {
	/* SvPOKp() because it's possible that sv has string overloading, and
	   therefore is a reference, hence SvPVX() is actually a pointer.
	   This cures the (very real) symptoms of RT 69422, but I'm not actually
	   sure whether we should even be caching the results of UTF-8
	   operations on overloading, given that nothing stops overloading
	   returning a different value every time it's called.  */
	const U8 *start = (const U8 *) SvPVX_const(sv);
	const STRLEN realutf8 = utf8_length(start, start + byte);

	if (realutf8 != utf8) {
	    /* Need to turn the assertions off otherwise we may recurse
	       infinitely while printing error messages.  */
	    SAVEI8(PL_utf8cache);
	    PL_utf8cache = 0;
	    Perl_croak(aTHX_ "panic: utf8_mg_pos_cache_update cache %"UVuf
		       " real %"UVuf" for %"SVf, (UV) utf8, (UV) realutf8, SVfARG(sv));
	}
    }

    /* Cache is held with the later position first, to simplify the code
       that deals with unbounded ends.  */
       
    ASSERT_UTF8_CACHE(cache);
    if (cache[1] == 0) {
	/* Cache is totally empty  */
	cache[0] = utf8;
	cache[1] = byte;
    } else if (cache[3] == 0) {
	if (byte > cache[1]) {
	    /* New one is larger, so goes first.  */
	    cache[2] = cache[0];
	    cache[3] = cache[1];
	    cache[0] = utf8;
	    cache[1] = byte;
	} else {
	    cache[2] = utf8;
	    cache[3] = byte;
	}
    } else {
#define THREEWAY_SQUARE(a,b,c,d) \
	    ((float)((d) - (c))) * ((float)((d) - (c))) \
	    + ((float)((c) - (b))) * ((float)((c) - (b))) \
	       + ((float)((b) - (a))) * ((float)((b) - (a)))

	/* Cache has 2 slots in use, and we know three potential pairs.
	   Keep the two that give the lowest RMS distance. Do the
	   calcualation in bytes simply because we always know the byte
	   length.  squareroot has the same ordering as the positive value,
	   so don't bother with the actual square root.  */
	const float existing = THREEWAY_SQUARE(0, cache[3], cache[1], blen);
	if (byte > cache[1]) {
	    /* New position is after the existing pair of pairs.  */
	    const float keep_earlier
		= THREEWAY_SQUARE(0, cache[3], byte, blen);
	    const float keep_later
		= THREEWAY_SQUARE(0, cache[1], byte, blen);

	    if (keep_later < keep_earlier) {
		if (keep_later < existing) {
		    cache[2] = cache[0];
		    cache[3] = cache[1];
		    cache[0] = utf8;
		    cache[1] = byte;
		}
	    }
	    else {
		if (keep_earlier < existing) {
		    cache[0] = utf8;
		    cache[1] = byte;
		}
	    }
	}
	else if (byte > cache[3]) {
	    /* New position is between the existing pair of pairs.  */
	    const float keep_earlier
		= THREEWAY_SQUARE(0, cache[3], byte, blen);
	    const float keep_later
		= THREEWAY_SQUARE(0, byte, cache[1], blen);

	    if (keep_later < keep_earlier) {
		if (keep_later < existing) {
		    cache[2] = utf8;
		    cache[3] = byte;
		}
	    }
	    else {
		if (keep_earlier < existing) {
		    cache[0] = utf8;
		    cache[1] = byte;
		}
	    }
	}
	else {
 	    /* New position is before the existing pair of pairs.  */
	    const float keep_earlier
		= THREEWAY_SQUARE(0, byte, cache[3], blen);
	    const float keep_later
		= THREEWAY_SQUARE(0, byte, cache[1], blen);

	    if (keep_later < keep_earlier) {
		if (keep_later < existing) {
		    cache[2] = utf8;
		    cache[3] = byte;
		}
	    }
	    else {
		if (keep_earlier < existing) {
		    cache[0] = cache[2];
		    cache[1] = cache[3];
		    cache[2] = utf8;
		    cache[3] = byte;
		}
	    }
	}
    }
    ASSERT_UTF8_CACHE(cache);
}

/* We already know all of the way, now we may be able to walk back.  The same
   assumption is made as in S_sv_pos_u2b_midway(), namely that walking
   backward is half the speed of walking forward. */
static STRLEN
S_sv_pos_b2u_midway(pTHX_ const U8 *const s, const U8 *const target,
                    const U8 *end, STRLEN endu)
{
    const STRLEN forw = target - s;
    STRLEN backw = end - target;

    PERL_ARGS_ASSERT_SV_POS_B2U_MIDWAY;

    if (forw < 2 * backw) {
	return utf8_length(s, target);
    }

    while (end > target) {
	end--;
	while (UTF8_IS_CONTINUATION(*end)) {
	    end--;
	}
	endu--;
    }
    return endu;
}

/*
=for apidoc sv_pos_b2u

Converts the value pointed to by offsetp from a count of bytes from the
start of the string, to a count of the equivalent number of UTF-8 chars.
Handles magic and type coercion.

=cut
*/

/*
 * sv_pos_b2u() uses, like sv_pos_u2b(), the mg_ptr of the potential
 * PERL_MAGIC_utf8 of the sv to store the mapping between UTF-8 and
 * byte offsets.
 *
 */
void
Perl_sv_pos_b2u(pTHX_ register SV *const sv, I32 *const offsetp)
{
    const U8* s;
    const STRLEN byte = *offsetp;
    STRLEN len = 0; /* Actually always set, but let's keep gcc happy.  */
    STRLEN blen;
    MAGIC* mg = NULL;
    const U8* send;
    bool found = FALSE;

    PERL_ARGS_ASSERT_SV_POS_B2U;

    if (!sv)
	return;

    s = (const U8*)SvPV_const(sv, blen);

    if (blen < byte)
	Perl_croak(aTHX_ "panic: sv_pos_b2u: bad byte offset");

    send = s + byte;

    if (!SvREADONLY(sv)
	&& PL_utf8cache
	&& SvTYPE(sv) >= SVt_PVMG
	&& (mg = mg_find(sv, PERL_MAGIC_utf8)))
    {
	if (mg->mg_ptr) {
	    STRLEN * const cache = (STRLEN *) mg->mg_ptr;
	    if (cache[1] == byte) {
		/* An exact match. */
		*offsetp = cache[0];
		return;
	    }
	    if (cache[3] == byte) {
		/* An exact match. */
		*offsetp = cache[2];
		return;
	    }

	    if (cache[1] < byte) {
		/* We already know part of the way. */
		if (mg->mg_len != -1) {
		    /* Actually, we know the end too.  */
		    len = cache[0]
			+ S_sv_pos_b2u_midway(aTHX_ s + cache[1], send,
					      s + blen, mg->mg_len - cache[0]);
		} else {
		    len = cache[0] + utf8_length(s + cache[1], send);
		}
	    }
	    else if (cache[3] < byte) {
		/* We're between the two cached pairs, so we do the calculation
		   offset by the byte/utf-8 positions for the earlier pair,
		   then add the utf-8 characters from the string start to
		   there.  */
		len = S_sv_pos_b2u_midway(aTHX_ s + cache[3], send,
					  s + cache[1], cache[0] - cache[2])
		    + cache[2];

	    }
	    else { /* cache[3] > byte */
		len = S_sv_pos_b2u_midway(aTHX_ s, send, s + cache[3],
					  cache[2]);

	    }
	    ASSERT_UTF8_CACHE(cache);
	    found = TRUE;
	} else if (mg->mg_len != -1) {
	    len = S_sv_pos_b2u_midway(aTHX_ s, send, s + blen, mg->mg_len);
	    found = TRUE;
	}
    }
    if (!found || PL_utf8cache < 0) {
	const STRLEN real_len = utf8_length(s, send);

	if (found && PL_utf8cache < 0) {
	    if (len != real_len) {
		/* Need to turn the assertions off otherwise we may recurse
		   infinitely while printing error messages.  */
		SAVEI8(PL_utf8cache);
		PL_utf8cache = 0;
		Perl_croak(aTHX_ "panic: sv_pos_b2u cache %"UVuf
			   " real %"UVuf" for %"SVf,
			   (UV) len, (UV) real_len, SVfARG(sv));
	    }
	}
	len = real_len;
    }
    *offsetp = len;

    if (PL_utf8cache)
	utf8_mg_pos_cache_update(sv, &mg, byte, len, blen);
}

/*
=for apidoc sv_eq

Returns a boolean indicating whether the strings in the two SVs are
identical. Is UTF-8 and 'use bytes' aware, handles get magic, and will
coerce its args to strings if necessary.

=cut
*/

I32
Perl_sv_eq(pTHX_ register SV *sv1, register SV *sv2)
{
    dVAR;
    const char *pv1;
    STRLEN cur1;
    const char *pv2;
    STRLEN cur2;
    I32  eq     = 0;
    char *tpv   = NULL;
    SV* svrecode = NULL;

    if (!sv1) {
	pv1 = "";
	cur1 = 0;
    }
    else {
	/* if pv1 and pv2 are the same, second SvPV_const call may
	 * invalidate pv1, so we may need to make a copy */
	if (sv1 == sv2 && (SvTHINKFIRST(sv1) || SvGMAGICAL(sv1))) {
	    pv1 = SvPV_const(sv1, cur1);
	    sv1 = newSVpvn_flags(pv1, cur1, SVs_TEMP | SvUTF8(sv2));
	}
	pv1 = SvPV_const(sv1, cur1);
    }

    if (!sv2){
	pv2 = "";
	cur2 = 0;
    }
    else
	pv2 = SvPV_const(sv2, cur2);

    if (cur1 && cur2 && SvUTF8(sv1) != SvUTF8(sv2) && !IN_BYTES) {
        /* Differing utf8ness.
	 * Do not UTF8size the comparands as a side-effect. */
	 if (PL_encoding) {
	      if (SvUTF8(sv1)) {
		   svrecode = newSVpvn(pv2, cur2);
		   sv_recode_to_utf8(svrecode, PL_encoding);
		   pv2 = SvPV_const(svrecode, cur2);
	      }
	      else {
		   svrecode = newSVpvn(pv1, cur1);
		   sv_recode_to_utf8(svrecode, PL_encoding);
		   pv1 = SvPV_const(svrecode, cur1);
	      }
	      /* Now both are in UTF-8. */
	      if (cur1 != cur2) {
		   SvREFCNT_dec(svrecode);
		   return FALSE;
	      }
	 }
	 else {
	      bool is_utf8 = TRUE;

	      if (SvUTF8(sv1)) {
		   /* sv1 is the UTF-8 one,
		    * if is equal it must be downgrade-able */
		   char * const pv = (char*)bytes_from_utf8((const U8*)pv1,
						     &cur1, &is_utf8);
		   if (pv != pv1)
			pv1 = tpv = pv;
	      }
	      else {
		   /* sv2 is the UTF-8 one,
		    * if is equal it must be downgrade-able */
		   char * const pv = (char *)bytes_from_utf8((const U8*)pv2,
						      &cur2, &is_utf8);
		   if (pv != pv2)
			pv2 = tpv = pv;
	      }
	      if (is_utf8) {
		   /* Downgrade not possible - cannot be eq */
		   assert (tpv == 0);
		   return FALSE;
	      }
	 }
    }

    if (cur1 == cur2)
	eq = (pv1 == pv2) || memEQ(pv1, pv2, cur1);
	
    SvREFCNT_dec(svrecode);
    if (tpv)
	Safefree(tpv);

    return eq;
}

/*
=for apidoc sv_cmp

Compares the strings in two SVs.  Returns -1, 0, or 1 indicating whether the
string in C<sv1> is less than, equal to, or greater than the string in
C<sv2>. Is UTF-8 and 'use bytes' aware, handles get magic, and will
coerce its args to strings if necessary.  See also C<sv_cmp_locale>.

=cut
*/

I32
Perl_sv_cmp(pTHX_ register SV *const sv1, register SV *const sv2)
{
    dVAR;
    STRLEN cur1, cur2;
    const char *pv1, *pv2;
    char *tpv = NULL;
    I32  cmp;
    SV *svrecode = NULL;

    if (!sv1) {
	pv1 = "";
	cur1 = 0;
    }
    else
	pv1 = SvPV_const(sv1, cur1);

    if (!sv2) {
	pv2 = "";
	cur2 = 0;
    }
    else
	pv2 = SvPV_const(sv2, cur2);

    if (cur1 && cur2 && SvUTF8(sv1) != SvUTF8(sv2) && !IN_BYTES) {
        /* Differing utf8ness.
	 * Do not UTF8size the comparands as a side-effect. */
	if (SvUTF8(sv1)) {
	    if (PL_encoding) {
		 svrecode = newSVpvn(pv2, cur2);
		 sv_recode_to_utf8(svrecode, PL_encoding);
		 pv2 = SvPV_const(svrecode, cur2);
	    }
	    else {
		 pv2 = tpv = (char*)bytes_to_utf8((const U8*)pv2, &cur2);
	    }
	}
	else {
	    if (PL_encoding) {
		 svrecode = newSVpvn(pv1, cur1);
		 sv_recode_to_utf8(svrecode, PL_encoding);
		 pv1 = SvPV_const(svrecode, cur1);
	    }
	    else {
		 pv1 = tpv = (char*)bytes_to_utf8((const U8*)pv1, &cur1);
	    }
	}
    }

    if (!cur1) {
	cmp = cur2 ? -1 : 0;
    } else if (!cur2) {
	cmp = 1;
    } else {
        const I32 retval = memcmp((const void*)pv1, (const void*)pv2, cur1 < cur2 ? cur1 : cur2);

	if (retval) {
	    cmp = retval < 0 ? -1 : 1;
	} else if (cur1 == cur2) {
	    cmp = 0;
        } else {
	    cmp = cur1 < cur2 ? -1 : 1;
	}
    }

    SvREFCNT_dec(svrecode);
    if (tpv)
	Safefree(tpv);

    return cmp;
}

/*
=for apidoc sv_cmp_locale

Compares the strings in two SVs in a locale-aware manner. Is UTF-8 and
'use bytes' aware, handles get magic, and will coerce its args to strings
if necessary.  See also C<sv_cmp>.

=cut
*/

I32
Perl_sv_cmp_locale(pTHX_ register SV *const sv1, register SV *const sv2)
{
    dVAR;
#ifdef USE_LOCALE_COLLATE

    char *pv1, *pv2;
    STRLEN len1, len2;
    I32 retval;

    if (PL_collation_standard)
	goto raw_compare;

    len1 = 0;
    pv1 = sv1 ? sv_collxfrm(sv1, &len1) : (char *) NULL;
    len2 = 0;
    pv2 = sv2 ? sv_collxfrm(sv2, &len2) : (char *) NULL;

    if (!pv1 || !len1) {
	if (pv2 && len2)
	    return -1;
	else
	    goto raw_compare;
    }
    else {
	if (!pv2 || !len2)
	    return 1;
    }

    retval = memcmp((void*)pv1, (void*)pv2, len1 < len2 ? len1 : len2);

    if (retval)
	return retval < 0 ? -1 : 1;

    /*
     * When the result of collation is equality, that doesn't mean
     * that there are no differences -- some locales exclude some
     * characters from consideration.  So to avoid false equalities,
     * we use the raw string as a tiebreaker.
     */

  raw_compare:
    /*FALLTHROUGH*/

#endif /* USE_LOCALE_COLLATE */

    return sv_cmp(sv1, sv2);
}


#ifdef USE_LOCALE_COLLATE

/*
=for apidoc sv_collxfrm

Add Collate Transform magic to an SV if it doesn't already have it.

Any scalar variable may carry PERL_MAGIC_collxfrm magic that contains the
scalar data of the variable, but transformed to such a format that a normal
memory comparison can be used to compare the data according to the locale
settings.

=cut
*/

char *
Perl_sv_collxfrm(pTHX_ SV *const sv, STRLEN *const nxp)
{
    dVAR;
    MAGIC *mg;

    PERL_ARGS_ASSERT_SV_COLLXFRM;

    mg = SvMAGICAL(sv) ? mg_find(sv, PERL_MAGIC_collxfrm) : (MAGIC *) NULL;
    if (!mg || !mg->mg_ptr || *(U32*)mg->mg_ptr != PL_collation_ix) {
	const char *s;
	char *xf;
	STRLEN len, xlen;

	if (mg)
	    Safefree(mg->mg_ptr);
	s = SvPV_const(sv, len);
	if ((xf = mem_collxfrm(s, len, &xlen))) {
	    if (! mg) {
#ifdef PERL_OLD_COPY_ON_WRITE
		if (SvIsCOW(sv))
		    sv_force_normal_flags(sv, 0);
#endif
		mg = sv_magicext(sv, 0, PERL_MAGIC_collxfrm, &PL_vtbl_collxfrm,
				 0, 0);
		assert(mg);
	    }
	    mg->mg_ptr = xf;
	    mg->mg_len = xlen;
	}
	else {
	    if (mg) {
		mg->mg_ptr = NULL;
		mg->mg_len = -1;
	    }
	}
    }
    if (mg && mg->mg_ptr) {
	*nxp = mg->mg_len;
	return mg->mg_ptr + sizeof(PL_collation_ix);
    }
    else {
	*nxp = 0;
	return NULL;
    }
}

#endif /* USE_LOCALE_COLLATE */

/*
=for apidoc sv_gets

Get a li