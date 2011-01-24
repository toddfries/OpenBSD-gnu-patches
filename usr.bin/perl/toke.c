/*    toke.c
 *
 *    Copyright (C) 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
 *    2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008 by Larry Wall and others
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 */

/*
 *  'It all comes from here, the stench and the peril.'    --Frodo
 *
 *     [p.719 of _The Lord of the Rings_, IV/ix: "Shelob's Lair"]
 */

/*
 * This file is the lexer for Perl.  It's closely linked to the
 * parser, perly.y.
 *
 * The main routine is yylex(), which returns the next token.
 */

/*
=head1 Lexer interface

This is the lower layer of the Perl parser, managing characters and tokens.

=for apidoc AmU|yy_parser *|PL_parser

Pointer to a structure encapsulating the state of the parsing operation
currently in progress.  The pointer can be locally changed to perform
a nested parse without interfering with the state of an outer parse.
Individual members of C<PL_parser> have their own documentation.

=cut
*/

#include "EXTERN.h"
#define PERL_IN_TOKE_C
#include "perl.h"

#define new_constant(a,b,c,d,e,f,g)	\
	S_new_constant(aTHX_ a,b,STR_WITH_LEN(c),d,e,f, g)

#define pl_yylval	(PL_parser->yylval)

/* YYINITDEPTH -- initial size of the parser's stacks.  */
#define YYINITDEPTH 200

/* XXX temporary backwards compatibility */
#define PL_lex_brackets		(PL_parser->lex_brackets)
#define PL_lex_brackstack	(PL_parser->lex_brackstack)
#define PL_lex_casemods		(PL_parser->lex_casemods)
#define PL_lex_casestack        (PL_parser->lex_casestack)
#define PL_lex_defer		(PL_parser->lex_defer)
#define PL_lex_dojoin		(PL_parser->lex_dojoin)
#define PL_lex_expect		(PL_parser->lex_expect)
#define PL_lex_formbrack        (PL_parser->lex_formbrack)
#define PL_lex_inpat		(PL_parser->lex_inpat)
#define PL_lex_inwhat		(PL_parser->lex_inwhat)
#define PL_lex_op		(PL_parser->lex_op)
#define PL_lex_repl		(PL_parser->lex_repl)
#define PL_lex_starts		(PL_parser->lex_starts)
#define PL_lex_stuff		(PL_parser->lex_stuff)
#define PL_multi_start		(PL_parser->multi_start)
#define PL_multi_open		(PL_parser->multi_open)
#define PL_multi_close		(PL_parser->multi_close)
#define PL_pending_ident        (PL_parser->pending_ident)
#define PL_preambled		(PL_parser->preambled)
#define PL_sublex_info		(PL_parser->sublex_info)
#define PL_linestr		(PL_parser->linestr)
#define PL_expect		(PL_parser->expect)
#define PL_copline		(PL_parser->copline)
#define PL_bufptr		(PL_parser->bufptr)
#define PL_oldbufptr		(PL_parser->oldbufptr)
#define PL_oldoldbufptr		(PL_parser->oldoldbufptr)
#define PL_linestart		(PL_parser->linestart)
#define PL_bufend		(PL_parser->bufend)
#define PL_last_uni		(PL_parser->last_uni)
#define PL_last_lop		(PL_parser->last_lop)
#define PL_last_lop_op		(PL_parser->last_lop_op)
#define PL_lex_state		(PL_parser->lex_state)
#define PL_rsfp			(PL_parser->rsfp)
#define PL_rsfp_filters		(PL_parser->rsfp_filters)
#define PL_in_my		(PL_parser->in_my)
#define PL_in_my_stash		(PL_parser->in_my_stash)
#define PL_tokenbuf		(PL_parser->tokenbuf)
#define PL_multi_end		(PL_parser->multi_end)
#define PL_error_count		(PL_parser->error_count)

#ifdef PERL_MAD
#  define PL_endwhite		(PL_parser->endwhite)
#  define PL_faketokens		(PL_parser->faketokens)
#  define PL_lasttoke		(PL_parser->lasttoke)
#  define PL_nextwhite		(PL_parser->nextwhite)
#  define PL_realtokenstart	(PL_parser->realtokenstart)
#  define PL_skipwhite		(PL_parser->skipwhite)
#  define PL_thisclose		(PL_parser->thisclose)
#  define PL_thismad		(PL_parser->thismad)
#  define PL_thisopen		(PL_parser->thisopen)
#  define PL_thisstuff		(PL_parser->thisstuff)
#  define PL_thistoken		(PL_parser->thistoken)
#  define PL_thiswhite		(PL_parser->thiswhite)
#  define PL_thiswhite		(PL_parser->thiswhite)
#  define PL_nexttoke		(PL_parser->nexttoke)
#  define PL_curforce		(PL_parser->curforce)
#else
#  define PL_nexttoke		(PL_parser->nexttoke)
#  define PL_nexttype		(PL_parser->nexttype)
#  define PL_nextval		(PL_parser->nextval)
#endif

/* This can't be done with embed.fnc, because struct yy_parser contains a
   member named pending_ident, which clashes with the generated #define  */
static int
S_pending_ident(pTHX);

static const char ident_too_long[] = "Identifier too long";

#ifdef PERL_MAD
#  define CURMAD(slot,sv) if (PL_madskills) { curmad(slot,sv); sv = 0; }
#  define NEXTVAL_NEXTTOKE PL_nexttoke[PL_curforce].next_val
#else
#  define CURMAD(slot,sv)
#  define NEXTVAL_NEXTTOKE PL_nextval[PL_nexttoke]
#endif

#define XFAKEBRACK 128
#define XENUMMASK 127

#ifdef USE_UTF8_SCRIPTS
#   define UTF (!IN_BYTES)
#else
#   define UTF ((PL_linestr && DO_UTF8(PL_linestr)) || (PL_hints & HINT_UTF8))
#endif

/* The maximum number of characters preceding the unrecognized one to display */
#define UNRECOGNIZED_PRECEDE_COUNT 10

/* In variables named $^X, these are the legal values for X.
 * 1999-02-27 mjd-perl-patch@plover.com */
#define isCONTROLVAR(x) (isUPPER(x) || strchr("[\\]^_?", (x)))

#define SPACE_OR_TAB(c) ((c)==' '||(c)=='\t')

/* LEX_* are values for PL_lex_state, the state of the lexer.
 * They are arranged oddly so that the guard on the switch statement
 * can get by with a single comparison (if the compiler is smart enough).
 */

/* #define LEX_NOTPARSING		11 is done in perl.h. */

#define LEX_NORMAL		10 /* normal code (ie not within "...")     */
#define LEX_INTERPNORMAL	 9 /* code within a string, eg "$foo[$x+1]" */
#define LEX_INTERPCASEMOD	 8 /* expecting a \U, \Q or \E etc          */
#define LEX_INTERPPUSH		 7 /* starting a new sublex parse level     */
#define LEX_INTERPSTART		 6 /* expecting the start of a $var         */

				   /* at end of code, eg "$x" followed by:  */
#define LEX_INTERPEND		 5 /* ... eg not one of [, { or ->          */
#define LEX_INTERPENDMAYBE	 4 /* ... eg one of [, { or ->              */

#define LEX_INTERPCONCAT	 3 /* expecting anything, eg at start of
				        string or after \E, $foo, etc       */
#define LEX_INTERPCONST		 2 /* NOT USED */
#define LEX_FORMLINE		 1 /* expecting a format line               */
#define LEX_KNOWNEXT		 0 /* next token known; just return it      */


#ifdef DEBUGGING
static const char* const lex_state_names[] = {
    "KNOWNEXT",
    "FORMLINE",
    "INTERPCONST",
    "INTERPCONCAT",
    "INTERPENDMAYBE",
    "INTERPEND",
    "INTERPSTART",
    "INTERPPUSH",
    "INTERPCASEMOD",
    "INTERPNORMAL",
    "NORMAL"
};
#endif

#ifdef ff_next
#undef ff_next
#endif

#include "keywords.h"

/* CLINE is a macro that ensures PL_copline has a sane value */

#ifdef CLINE
#undef CLINE
#endif
#define CLINE (PL_copline = (CopLINE(PL_curcop) < PL_copline ? CopLINE(PL_curcop) : PL_copline))

#ifdef PERL_MAD
#  define SKIPSPACE0(s) skipspace0(s)
#  define SKIPSPACE1(s) skipspace1(s)
#  define SKIPSPACE2(s,tsv) skipspace2(s,&tsv)
#  define PEEKSPACE(s) skipspace2(s,0)
#else
#  define SKIPSPACE0(s) skipspace(s)
#  define SKIPSPACE1(s) skipspace(s)
#  define SKIPSPACE2(s,tsv) skipspace(s)
#  define PEEKSPACE(s) skipspace(s)
#endif

/*
 * Convenience functions to return different tokens and prime the
 * lexer for the next token.  They all take an argument.
 *
 * TOKEN        : generic token (used for '(', DOLSHARP, etc)
 * OPERATOR     : generic operator
 * AOPERATOR    : assignment operator
 * PREBLOCK     : beginning the block after an if, while, foreach, ...
 * PRETERMBLOCK : beginning a non-code-defining {} block (eg, hash ref)
 * PREREF       : *EXPR where EXPR is not a simple identifier
 * TERM         : expression term
 * LOOPX        : loop exiting command (goto, last, dump, etc)
 * FTST         : file test operator
 * FUN0         : zero-argument function
 * FUN1         : not used, except for not, which isn't a UNIOP
 * BOop         : bitwise or or xor
 * BAop         : bitwise and
 * SHop         : shift operator
 * PWop         : power operator
 * PMop         : pattern-matching operator
 * Aop          : addition-level operator
 * Mop          : multiplication-level operator
 * Eop          : equality-testing operator
 * Rop          : relational operator <= != gt
 *
 * Also see LOP and lop() below.
 */

#ifdef DEBUGGING /* Serve -DT. */
#   define REPORT(retval) tokereport((I32)retval, &pl_yylval)
#else
#   define REPORT(retval) (retval)
#endif

#define TOKEN(retval) return ( PL_bufptr = s, REPORT(retval))
#define OPERATOR(retval) return (PL_expect = XTERM, PL_bufptr = s, REPORT(retval))
#define AOPERATOR(retval) return ao((PL_expect = XTERM, PL_bufptr = s, REPORT(retval)))
#define PREBLOCK(retval) return (PL_expect = XBLOCK,PL_bufptr = s, REPORT(retval))
#define PRETERMBLOCK(retval) return (PL_expect = XTERMBLOCK,PL_bufptr = s, REPORT(retval))
#define PREREF(retval) return (PL_expect = XREF,PL_bufptr = s, REPORT(retval))
#define TERM(retval) return (CLINE, PL_expect = XOPERATOR, PL_bufptr = s, REPORT(retval))
#define LOOPX(f) return (pl_yylval.ival=f, PL_expect=XTERM, PL_bufptr=s, REPORT((int)LOOPEX))
#define FTST(f)  return (pl_yylval.ival=f, PL_expect=XTERMORDORDOR, PL_bufptr=s, REPORT((int)UNIOP))
#define FUN0(f)  return (pl_yylval.ival=f, PL_expect=XOPERATOR, PL_bufptr=s, REPORT((int)FUNC0))
#define FUN1(f)  return (pl_yylval.ival=f, PL_expect=XOPERATOR, PL_bufptr=s, REPORT((int)FUNC1))
#define BOop(f)  return ao((pl_yylval.ival=f, PL_expect=XTERM, PL_bufptr=s, REPORT((int)BITOROP)))
#define BAop(f)  return ao((pl_yylval.ival=f, PL_expect=XTERM, PL_bufptr=s, REPORT((int)BITANDOP)))
#define SHop(f)  return ao((pl_yylval.ival=f, PL_expect=XTERM, PL_bufptr=s, REPORT((int)SHIFTOP)))
#define PWop(f)  return ao((pl_yylval.ival=f, PL_expect=XTERM, PL_bufptr=s, REPORT((int)POWOP)))
#define PMop(f)  return(pl_yylval.ival=f, PL_expect=XTERM, PL_bufptr=s, REPORT((int)MATCHOP))
#define Aop(f)   return ao((pl_yylval.ival=f, PL_expect=XTERM, PL_bufptr=s, REPORT((int)ADDOP)))
#define Mop(f)   return ao((pl_yylval.ival=f, PL_expect=XTERM, PL_bufptr=s, REPORT((int)MULOP)))
#define Eop(f)   return (pl_yylval.ival=f, PL_expect=XTERM, PL_bufptr=s, REPORT((int)EQOP))
#define Rop(f)   return (pl_yylval.ival=f, PL_expect=XTERM, PL_bufptr=s, REPORT((int)RELOP))

/* This bit of chicanery makes a unary function followed by
 * a parenthesis into a function with one argument, highest precedence.
 * The UNIDOR macro is for unary functions that can be followed by the //
 * operator (such as C<shift // 0>).
 */
#define UNI2(f,x) { \
	pl_yylval.ival = f; \
	PL_expect = x; \
	PL_bufptr = s; \
	PL_last_uni = PL_oldbufptr; \
	PL_last_lop_op = f; \
	if (*s == '(') \
	    return REPORT( (int)FUNC1 ); \
	s = PEEKSPACE(s); \
	return REPORT( *s=='(' ? (int)FUNC1 : (int)UNIOP ); \
	}
#define UNI(f)    UNI2(f,XTERM)
#define UNIDOR(f) UNI2(f,XTERMORDORDOR)

#define UNIBRACK(f) { \
	pl_yylval.ival = f; \
	PL_bufptr = s; \
	PL_last_uni = PL_oldbufptr; \
	if (*s == '(') \
	    return REPORT( (int)FUNC1 ); \
	s = PEEKSPACE(s); \
	return REPORT( (*s == '(') ? (int)FUNC1 : (int)UNIOP ); \
	}

/* grandfather return to old style */
#define OLDLOP(f) return(pl_yylval.ival=f,PL_expect = XTERM,PL_bufptr = s,(int)LSTOP)

#ifdef DEBUGGING

/* how to interpret the pl_yylval associated with the token */
enum token_type {
    TOKENTYPE_NONE,
    TOKENTYPE_IVAL,
    TOKENTYPE_OPNUM, /* pl_yylval.ival contains an opcode number */
    TOKENTYPE_PVAL,
    TOKENTYPE_OPVAL,
    TOKENTYPE_GVVAL
};

static struct debug_tokens {
    const int token;
    enum token_type type;
    const char *name;
} const debug_tokens[] =
{
    { ADDOP,		TOKENTYPE_OPNUM,	"ADDOP" },
    { ANDAND,		TOKENTYPE_NONE,		"ANDAND" },
    { ANDOP,		TOKENTYPE_NONE,		"ANDOP" },
    { ANONSUB,		TOKENTYPE_IVAL,		"ANONSUB" },
    { ARROW,		TOKENTYPE_NONE,		"ARROW" },
    { ASSIGNOP,		TOKENTYPE_OPNUM,	"ASSIGNOP" },
    { BITANDOP,		TOKENTYPE_OPNUM,	"BITANDOP" },
    { BITOROP,		TOKENTYPE_OPNUM,	"BITOROP" },
    { COLONATTR,	TOKENTYPE_NONE,		"COLONATTR" },
    { CONTINUE,		TOKENTYPE_NONE,		"CONTINUE" },
    { DEFAULT,		TOKENTYPE_NONE,		"DEFAULT" },
    { DO,		TOKENTYPE_NONE,		"DO" },
    { DOLSHARP,		TOKENTYPE_NONE,		"DOLSHARP" },
    { DORDOR,		TOKENTYPE_NONE,		"DORDOR" },
    { DOROP,		TOKENTYPE_OPNUM,	"DOROP" },
    { DOTDOT,		TOKENTYPE_IVAL,		"DOTDOT" },
    { ELSE,		TOKENTYPE_NONE,		"ELSE" },
    { ELSIF,		TOKENTYPE_IVAL,		"ELSIF" },
    { EQOP,		TOKENTYPE_OPNUM,	"EQOP" },
    { FOR,		TOKENTYPE_IVAL,		"FOR" },
    { FORMAT,		TOKENTYPE_NONE,		"FORMAT" },
    { FUNC,		TOKENTYPE_OPNUM,	"FUNC" },
    { FUNC0,		TOKENTYPE_OPNUM,	"FUNC0" },
    { FUNC0SUB,		TOKENTYPE_OPVAL,	"FUNC0SUB" },
    { FUNC1,		TOKENTYPE_OPNUM,	"FUNC1" },
    { FUNCMETH,		TOKENTYPE_OPVAL,	"FUNCMETH" },
    { GIVEN,		TOKENTYPE_IVAL,		"GIVEN" },
    { HASHBRACK,	TOKENTYPE_NONE,		"HASHBRACK" },
    { IF,		TOKENTYPE_IVAL,		"IF" },
    { LABEL,		TOKENTYPE_PVAL,		"LABEL" },
    { LOCAL,		TOKENTYPE_IVAL,		"LOCAL" },
    { LOOPEX,		TOKENTYPE_OPNUM,	"LOOPEX" },
    { LSTOP,		TOKENTYPE_OPNUM,	"LSTOP" },
    { LSTOPSUB,		TOKENTYPE_OPVAL,	"LSTOPSUB" },
    { MATCHOP,		TOKENTYPE_OPNUM,	"MATCHOP" },
    { METHOD,		TOKENTYPE_OPVAL,	"METHOD" },
    { MULOP,		TOKENTYPE_OPNUM,	"MULOP" },
    { MY,		TOKENTYPE_IVAL,		"MY" },
    { MYSUB,		TOKENTYPE_NONE,		"MYSUB" },
    { NOAMP,		TOKENTYPE_NONE,		"NOAMP" },
    { NOTOP,		TOKENTYPE_NONE,		"NOTOP" },
    { OROP,		TOKENTYPE_IVAL,		"OROP" },
    { OROR,		TOKENTYPE_NONE,		"OROR" },
    { PACKAGE,		TOKENTYPE_NONE,		"PACKAGE" },
    { PLUGEXPR,		TOKENTYPE_OPVAL,	"PLUGEXPR" },
    { PLUGSTMT,		TOKENTYPE_OPVAL,	"PLUGSTMT" },
    { PMFUNC,		TOKENTYPE_OPVAL,	"PMFUNC" },
    { POSTDEC,		TOKENTYPE_NONE,		"POSTDEC" },
    { POSTINC,		TOKENTYPE_NONE,		"POSTINC" },
    { POWOP,		TOKENTYPE_OPNUM,	"POWOP" },
    { PREDEC,		TOKENTYPE_NONE,		"PREDEC" },
    { PREINC,		TOKENTYPE_NONE,		"PREINC" },
    { PRIVATEREF,	TOKENTYPE_OPVAL,	"PRIVATEREF" },
    { REFGEN,		TOKENTYPE_NONE,		"REFGEN" },
    { RELOP,		TOKENTYPE_OPNUM,	"RELOP" },
    { SHIFTOP,		TOKENTYPE_OPNUM,	"SHIFTOP" },
    { SUB,		TOKENTYPE_NONE,		"SUB" },
    { THING,		TOKENTYPE_OPVAL,	"THING" },
    { UMINUS,		TOKENTYPE_NONE,		"UMINUS" },
    { UNIOP,		TOKENTYPE_OPNUM,	"UNIOP" },
    { UNIOPSUB,		TOKENTYPE_OPVAL,	"UNIOPSUB" },
    { UNLESS,		TOKENTYPE_IVAL,		"UNLESS" },
    { UNTIL,		TOKENTYPE_IVAL,		"UNTIL" },
    { USE,		TOKENTYPE_IVAL,		"USE" },
    { WHEN,		TOKENTYPE_IVAL,		"WHEN" },
    { WHILE,		TOKENTYPE_IVAL,		"WHILE" },
    { WORD,		TOKENTYPE_OPVAL,	"WORD" },
    { YADAYADA,		TOKENTYPE_IVAL,		"YADAYADA" },
    { 0,		TOKENTYPE_NONE,		NULL }
};

/* dump the returned token in rv, plus any optional arg in pl_yylval */

STATIC int
S_tokereport(pTHX_ I32 rv, const YYSTYPE* lvalp)
{
    dVAR;

    PERL_ARGS_ASSERT_TOKEREPORT;

    if (DEBUG_T_TEST) {
	const char *name = NULL;
	enum token_type type = TOKENTYPE_NONE;
	const struct debug_tokens *p;
	SV* const report = newSVpvs("<== ");

	for (p = debug_tokens; p->token; p++) {
	    if (p->token == (int)rv) {
		name = p->name;
		type = p->type;
		break;
	    }
	}
	if (name)
	    Perl_sv_catpv(aTHX_ report, name);
	else if ((char)rv > ' ' && (char)rv < '~')
	    Perl_sv_catpvf(aTHX_ report, "'%c'", (char)rv);
	else if (!rv)
	    sv_catpvs(report, "EOF");
	else
	    Perl_sv_catpvf(aTHX_ report, "?? %"IVdf, (IV)rv);
	switch (type) {
	case TOKENTYPE_NONE:
	case TOKENTYPE_GVVAL: /* doesn't appear to be used */
	    break;
	case TOKENTYPE_IVAL:
	    Perl_sv_catpvf(aTHX_ report, "(ival=%"IVdf")", (IV)lvalp->ival);
	    break;
	case TOKENTYPE_OPNUM:
	    Perl_sv_catpvf(aTHX_ report, "(ival=op_%s)",
				    PL_op_name[lvalp->ival]);
	    break;
	case TOKENTYPE_PVAL:
	    Perl_sv_catpvf(aTHX_ report, "(pval=\"%s\")", lvalp->pval);
	    break;
	case TOKENTYPE_OPVAL:
	    if (lvalp->opval) {
		Perl_sv_catpvf(aTHX_ report, "(opval=op_%s)",
				    PL_op_name[lvalp->opval->op_type]);
		if (lvalp->opval->op_type == OP_CONST) {
		    Perl_sv_catpvf(aTHX_ report, " %s",
			SvPEEK(cSVOPx_sv(lvalp->opval)));
		}

	    }
	    else
		sv_catpvs(report, "(opval=null)");
	    break;
	}
        PerlIO_printf(Perl_debug_log, "### %s\n\n", SvPV_nolen_const(report));
    };
    return (int)rv;
}


/* print the buffer with suitable escapes */

STATIC void
S_printbuf(pTHX_ const char *const fmt, const char *const s)
{
    SV* const tmp = newSVpvs("");

    PERL_ARGS_ASSERT_PRINTBUF;

    PerlIO_printf(Perl_debug_log, fmt, pv_display(tmp, s, strlen(s), 0, 60));
    SvREFCNT_dec(tmp);
}

#endif

static int
S_deprecate_commaless_var_list(pTHX) {
    PL_expect = XTERM;
    deprecate("comma-less variable list");
    return REPORT(','); /* grandfather non-comma-format format */
}

/*
 * S_ao
 *
 * This subroutine detects &&=, ||=, and //= and turns an ANDAND, OROR or DORDOR
 * into an OP_ANDASSIGN, OP_ORASSIGN, or OP_DORASSIGN
 */

STATIC int
S_ao(pTHX_ int toketype)
{
    dVAR;
    if (*PL_bufptr == '=') {
	PL_bufptr++;
	if (toketype == ANDAND)
	    pl_yylval.ival = OP_ANDASSIGN;
	else if (toketype == OROR)
	    pl_yylval.ival = OP_ORASSIGN;
	else if (toketype == DORDOR)
	    pl_yylval.ival = OP_DORASSIGN;
	toketype = ASSIGNOP;
    }
    return toketype;
}

/*
 * S_no_op
 * When Perl expects an operator and finds something else, no_op
 * prints the warning.  It always prints "<something> found where
 * operator expected.  It prints "Missing semicolon on previous line?"
 * if the surprise occurs at the start of the line.  "do you need to
 * predeclare ..." is printed out for code like "sub bar; foo bar $x"
 * where the compiler doesn't know if foo is a method call or a function.
 * It prints "Missing operator before end of line" if there's nothing
 * after the missing operator, or "... before <...>" if there is something
 * after the missing operator.
 */

STATIC void
S_no_op(pTHX_ const char *const what, char *s)
{
    dVAR;
    char * const oldbp = PL_bufptr;
    const bool is_first = (PL_oldbufptr == PL_linestart);

    PERL_ARGS_ASSERT_NO_OP;

    if (!s)
	s = oldbp;
    else
	PL_bufptr = s;
    yywarn(Perl_form(aTHX_ "%s found where operator expected", what));
    if (ckWARN_d(WARN_SYNTAX)) {
	if (is_first)
	    Perl_warner(aTHX_ packWARN(WARN_SYNTAX),
		    "\t(Missing semicolon on previous line?)\n");
	else if (PL_oldoldbufptr && isIDFIRST_lazy_if(PL_oldoldbufptr,UTF)) {
	    const char *t;
	    for (t = PL_oldoldbufptr; (isALNUM_lazy_if(t,UTF) || *t == ':'); t++)
		NOOP;
	    if (t < PL_bufptr && isSPACE(*t))
		Perl_warner(aTHX_ packWARN(WARN_SYNTAX),
			"\t(Do you need to predeclare %.*s?)\n",
		    (int)(t - PL_oldoldbufptr), PL_oldoldbufptr);
	}
	else {
	    assert(s >= oldbp);
	    Perl_warner(aTHX_ packWARN(WARN_SYNTAX),
		    "\t(Missing operator before %.*s?)\n", (int)(s - oldbp), oldbp);
	}
    }
    PL_bufptr = oldbp;
}

/*
 * S_missingterm
 * Complain about missing quote/regexp/heredoc terminator.
 * If it's called with NULL then it cauterizes the line buffer.
 * If we're in a delimited string and the delimiter is a control
 * character, it's reformatted into a two-char sequence like ^C.
 * This is fatal.
 */

STATIC void
S_missingterm(pTHX_ char *s)
{
    dVAR;
    char tmpbuf[3];
    char q;
    if (s) {
	char * const nl = strrchr(s,'\n');
	if (nl)
	    *nl = '\0';
    }
    else if (isCNTRL(PL_multi_close)) {
	*tmpbuf = '^';
	tmpbuf[1] = (char)toCTRL(PL_multi_close);
	tmpbuf[2] = '\0';
	s = tmpbuf;
    }
    else {
	*tmpbuf = (char)PL_multi_close;
	tmpbuf[1] = '\0';
	s = tmpbuf;
    }
    q = strchr(s,'"') ? '\'' : '"';
    Perl_croak(aTHX_ "Can't find string terminator %c%s%c anywhere before EOF",q,s,q);
}

#define FEATURE_IS_ENABLED(name)				        \
	((0 != (PL_hints & HINT_LOCALIZE_HH))				\
	    && S_feature_is_enabled(aTHX_ STR_WITH_LEN(name)))
/* The longest string we pass in.  */
#define MAX_FEATURE_LEN (sizeof("unicode_strings")-1)

/*
 * S_feature_is_enabled
 * Check whether the named feature is enabled.
 */
STATIC bool
S_feature_is_enabled(pTHX_ const char *const name, STRLEN namelen)
{
    dVAR;
    HV * const hinthv = GvHV(PL_hintgv);
    char he_name[8 + MAX_FEATURE_LEN] = "feature_";

    PERL_ARGS_ASSERT_FEATURE_IS_ENABLED;

    assert(namelen <= MAX_FEATURE_LEN);
    memcpy(&he_name[8], name, namelen);

    return (hinthv && hv_exists(hinthv, he_name, 8 + namelen));
}

/*
 * experimental text filters for win32 carriage-returns, utf16-to-utf8 and
 * utf16-to-utf8-reversed.
 */

#ifdef PERL_CR_FILTER
static void
strip_return(SV *sv)
{
    register const char *s = SvPVX_const(sv);
    register const char * const e = s + SvCUR(sv);

    PERL_ARGS_ASSERT_STRIP_RETURN;

    /* outer loop optimized to do nothing if there are no CR-LFs */
    while (s < e) {
	if (*s++ == '\r' && *s == '\n') {
	    /* hit a CR-LF, need to copy the rest */
	    register char *d = s - 1;
	    *d++ = *s++;
	    while (s < e) {
		if (*s == '\r' && s[1] == '\n')
		    s++;
		*d++ = *s++;
	    }
	    SvCUR(sv) -= s - d;
	    return;
	}
    }
}

STATIC I32
S_cr_textfilter(pTHX_ int idx, SV *sv, int maxlen)
{
    const I32 count = FILTER_READ(idx+1, sv, maxlen);
    if (count > 0 && !maxlen)
	strip_return(sv);
    return count;
}
#endif



/*
 * Perl_lex_start
 *
 * Create a parser object and initialise its parser and lexer fields
 *
 * rsfp       is the opened file handle to read from (if any),
 *
 * line       holds any initial content already read from the file (or in
 *            the case of no file, such as an eval, the whole contents);
 *
 * new_filter indicates that this is a new file and it shouldn't inherit
 *            the filters from the current parser (ie require).
 */

void
Perl_lex_start(pTHX_ SV *line, PerlIO *rsfp, bool new_filter)
{
    dVAR;
    const char *s = NULL;
    STRLEN len;
    yy_parser *parser, *oparser;

    /* create and initialise a parser */

    Newxz(parser, 1, yy_parser);
    parser->old_parser = oparser = PL_parser;
    PL_parser = parser;

    Newx(parser->stack, YYINITDEPTH, yy_stack_frame);
    parser->ps = parser->stack;
    parser->stack_size = YYINITDEPTH;

    parser->stack->state = 0;
    parser->yyerrstatus = 0;
    parser->yychar = YYEMPTY;		/* Cause a token to be read.  */

    /* on scope exit, free this parser and restore any outer one */
    SAVEPARSER(parser);
    parser->saved_curcop = PL_curcop;

    /* initialise lexer state */

#ifdef PERL_MAD
    parser->curforce = -1;
#else
    parser->nexttoke = 0;
#endif
    parser->error_count = oparser ? oparser->error_count : 0;
    parser->copline = NOLINE;
    parser->lex_state = LEX_NORMAL;
    parser->expect = XSTATE;
    parser->rsfp = rsfp;
    parser->rsfp_filters = (new_filter || !oparser) ? newAV()
		: MUTABLE_AV(SvREFCNT_inc(oparser->rsfp_filters));

    Newx(parser->lex_brackstack, 120, char);
    Newx(parser->lex_casestack, 12, char);
    *parser->lex_casestack = '\0';

    if (line) {
	s = SvPV_const(line, len);
    } else {
	len = 0;
    }

    if (!len) {
	parser->linestr = newSVpvs("\n;");
    } else if (SvREADONLY(line) || s[len-1] != ';') {
	parser->linestr = newSVsv(line);
	if (s[len-1] != ';')
	    sv_catpvs(parser->linestr, "\n;");
    } else {
	SvTEMP_off(line);
	SvREFCNT_inc_simple_void_NN(line);
	parser->linestr = line;
    }
    parser->oldoldbufptr =
	parser->oldbufptr =
	parser->bufptr =
	parser->linestart = SvPVX(parser->linestr);
    parser->bufend = parser->bufptr + SvCUR(parser->linestr);
    parser->last_lop = parser->last_uni = NULL;
}


/* delete a parser object */

void
Perl_parser_free(pTHX_  const yy_parser *parser)
{
    PERL_ARGS_ASSERT_PARSER_FREE;

    PL_curcop = parser->saved_curcop;
    SvREFCNT_dec(parser->linestr);

    if (parser->rsfp == PerlIO_stdin())
	PerlIO_clearerr(parser->rsfp);
    else if (parser->rsfp && (!parser->old_parser ||
		(parser->old_parser && parser->rsfp != parser->old_parser->rsfp)))
	PerlIO_close(parser->rsfp);
    SvREFCNT_dec(parser->rsfp_filters);

    Safefree(parser->stack);
    Safefree(parser->lex_brackstack);
    Safefree(parser->lex_casestack);
    PL_parser = parser->old_parser;
    Safefree(parser);
}


/*
 * Perl_lex_end
 * Finalizer for lexing operations.  Must be called when the parser is
 * done with the lexer.
 */

void
Perl_lex_end(pTHX)
{
    dVAR;
    PL_doextract = FALSE;
}

/*
=for apidoc AmxU|SV *|PL_parser-E<gt>linestr

Buffer scalar containing the chunk currently under consideration of the
text currently being lexed.  This is always a plain string scalar (for
which C<SvPOK> is true).  It is not intended to be used as a scalar by
normal scalar means; instead refer to the buffer directly by the pointer
variables described below.

The lexer maintains various C<char*> pointers to things in the
C<PL_parser-E<gt>linestr> buffer.  If C<PL_parser-E<gt>linestr> is ever
reallocated, all of these pointers must be updated.  Don't attempt to
do this manually, but rather use L</lex_grow_linestr> if you need to
reallocate the buffer.

The content of the text chunk in the buffer is commonly exactly one
complete line of input, up to and including a newline terminator,
but there are situations where it is otherwise.  The octets of the
buffer may be intended to be interpreted as either UTF-8 or Latin-1.
The function L</lex_bufutf8> tells you which.  Do not use the C<SvUTF8>
flag on this scalar, which may disagree with it.

For direct examination of the buffer, the variable
L</PL_parser-E<gt>bufend> points to the end of the buffer.  The current
lexing position is pointed to by L</PL_parser-E<gt>bufptr>.  Direct use
of these pointers is usually preferable to examination of the scalar
through normal scalar means.

=for apidoc AmxU|char *|PL_parser-E<gt>bufend

Direct pointer to the end of the chunk of text currently being lexed, the
end of the lexer buffer.  This is equal to C<SvPVX(PL_parser-E<gt>linestr)
+ SvCUR(PL_parser-E<gt>linestr)>.  A NUL character (zero octet) is
always located at the end of the buffer, and does not count as part of
the buffer's contents.

=for apidoc AmxU|char *|PL_parser-E<gt>bufptr

Points to the current position of lexing inside the lexer buffer.
Characters around this point may be freely examined, within
the range delimited by C<SvPVX(L</PL_parser-E<gt>linestr>)> and
L</PL_parser-E<gt>bufend>.  The octets of the buffer may be intended to be
interpreted as either UTF-8 or Latin-1, as indicated by L</lex_bufutf8>.

Lexing code (whether in the Perl core or not) moves this pointer past
the characters that it consumes.  It is also expected to perform some
bookkeeping whenever a newline character is consumed.  This movement
can be more conveniently performed by the function L</lex_read_to>,
which handles newlines appropriately.

Interpretation of the buffer's octets can be abstracted out by
using the slightly higher-level functions L</lex_peek_unichar> and
L</lex_read_unichar>.

=for apidoc AmxU|char *|PL_parser-E<gt>linestart

Points to the start of the current line inside the lexer buffer.
This is useful for indicating at which column an error occurred, and
not much else.  This must be updated by any lexing code that consumes
a newline; the function L</lex_read_to> handles this detail.

=cut
*/

/*
=for apidoc Amx|bool|lex_bufutf8

Indicates whether the octets in the lexer buffer
(L</PL_parser-E<gt>linestr>) should be interpreted as the UTF-8 encoding
of Unicode characters.  If not, they should be interpreted as Latin-1
characters.  This is analogous to the C<SvUTF8> flag for scalars.

In UTF-8 mode, it is not guaranteed that the lexer buffer actually
contains valid UTF-8.  Lexing code must be robust in the face of invalid
encoding.

The actual C<SvUTF8> flag of the L</PL_parser-E<gt>linestr> scalar
is significant, but not the whole story regarding the input character
encoding.  Normally, when a file is being read, the scalar contains octets
and its C<SvUTF8> flag is off, but the octets should be interpreted as
UTF-8 if the C<use utf8> pragma is in effect.  During a string eval,
however, the scalar may have the C<SvUTF8> flag on, and in this case its
octets should be interpreted as UTF-8 unless the C<use bytes> pragma
is in effect.  This logic may change in the future; use this function
instead of implementing the logic yourself.

=cut
*/

bool
Perl_lex_bufutf8(pTHX)
{
    return UTF;
}

/*
=for apidoc Amx|char *|lex_grow_linestr|STRLEN len

Reallocates the lexer buffer (L</PL_parser-E<gt>linestr>) to accommodate
at least I<len> octets (including terminating NUL).  Returns a
pointer to the reallocated buffer.  This is necessary before making
any direct modification of the buffer that would increase its length.
L</lex_stuff_pvn> provides a more convenient way to insert text into
the buffer.

Do not use C<SvGROW> or C<sv_grow> directly on C<PL_parser-E<gt>linestr>;
this function updates all of the lexer's variables that point directly
into the buffer.

=cut
*/

char *
Perl_lex_grow_linestr(pTHX_ STRLEN len)
{
    SV *linestr;
    char *buf;
    STRLEN bufend_pos, bufptr_pos, oldbufptr_pos, oldoldbufptr_pos;
    STRLEN linestart_pos, last_uni_pos, last_lop_pos;
    linestr = PL_parser->linestr;
    buf = SvPVX(linestr);
    if (len <= SvLEN(linestr))
	return buf;
    bufend_pos = PL_parser->bufend - buf;
    bufptr_pos = PL_parser->bufptr - buf;
    oldbufptr_pos = PL_parser->oldbufptr - buf;
    oldoldbufptr_pos = PL_parser->oldoldbufptr - buf;
    linestart_pos = PL_parser->linestart - buf;
    last_uni_pos = PL_parser->last_uni ? PL_parser->last_uni - buf : 0;
    last_lop_pos = PL_parser->last_lop ? PL_parser->last_lop - buf : 0;
    buf = sv_grow(linestr, len);
    PL_parser->bufend = buf + bufend_pos;
    PL_parser->bufptr = buf + bufptr_pos;
    PL_parser->oldbufptr = buf + oldbufptr_pos;
    PL_parser->oldoldbufptr = buf + oldoldbufptr_pos;
    PL_parser->linestart = buf + linestart_pos;
    if (PL_parser->last_uni)
	PL_parser->last_uni = buf + last_uni_pos;
    if (PL_parser->last_lop)
	PL_parser->last_lop = buf + last_lop_pos;
    return buf;
}

/*
=for apidoc Amx|void|lex_stuff_pvn|char *pv|STRLEN len|U32 flags

Insert characters into the lexer buffer (L</PL_parser-E<gt>linestr>),
immediately after the current lexing point (L</PL_parser-E<gt>bufptr>),
reallocating the buffer if necessary.  This means that lexing code that
runs later will see the characters as if they had appeared in the input.
It is not recommended to do this as part of normal parsing, and most
uses of this facility run the risk of the inserted characters being
interpreted in an unintended manner.

The string to be inserted is represented by I<len> octets starting
at I<pv>.  These octets are interpreted as either UTF-8 or Latin-1,
according to whether the C<LEX_STUFF_UTF8> flag is set in I<flags>.
The characters are recoded for the lexer buffer, according to how the
buffer is currently being interpreted (L</lex_bufutf8>).  If a string
to be interpreted is available as a Perl scalar, the L</lex_stuff_sv>
function is more convenient.

=cut
*/

void
Perl_lex_stuff_pvn(pTHX_ char *pv, STRLEN len, U32 flags)
{
    dVAR;
    char *bufptr;
    PERL_ARGS_ASSERT_LEX_STUFF_PVN;
    if (flags & ~(LEX_STUFF_UTF8))
	Perl_croak(aTHX_ "Lexing code internal error (%s)", "lex_stuff_pvn");
    if (UTF) {
	if (flags & LEX_STUFF_UTF8) {
	    goto plain_copy;
	} else {
	    STRLEN highhalf = 0;
	    char *p, *e = pv+len;
	    for (p = pv; p != e; p++)
		highhalf += !!(((U8)*p) & 0x80);
	    if (!highhalf)
		goto plain_copy;
	    lex_grow_linestr(SvCUR(PL_parser->linestr)+1+len+highhalf);
	    bufptr = PL_parser->bufptr;
	    Move(bufptr, bufptr+len+highhalf, PL_parser->bufend+1-bufptr, char);
	    SvCUR_set(PL_parser->linestr,
	    	SvCUR(PL_parser->linestr) + len+highhalf);
	    PL_parser->bufend += len+highhalf;
	    for (p = pv; p != e; p++) {
		U8 c = (U8)*p;
		if (c & 0x80) {
		    *bufptr++ = (char)(0xc0 | (c >> 6));
		    *bufptr++ = (char)(0x80 | (c & 0x3f));
		} else {
		    *bufptr++ = (char)c;
		}
	    }
	}
    } else {
	if (flags & LEX_STUFF_UTF8) {
	    STRLEN highhalf = 0;
	    char *p, *e = pv+len;
	    for (p = pv; p != e; p++) {
		U8 c = (U8)*p;
		if (c >= 0xc4) {
		    Perl_croak(aTHX_ "Lexing code attempted to stuff "
				"non-Latin-1 character into Latin-1 input");
		} else if (c >= 0xc2 && p+1 != e &&
			    (((U8)p[1]) & 0xc0) == 0x80) {
		    p++;
		    highhalf++;
		} else if (c >= 0x80) {
		    /* malformed UTF-8 */
		    ENTER;
		    SAVESPTR(PL_warnhook);
		    PL_warnhook = PERL_WARNHOOK_FATAL;
		    utf8n_to_uvuni((U8*)p, e-p, NULL, 0);
		    LEAVE;
		}
	    }
	    if (!highhalf)
		goto plain_copy;
	    lex_grow_linestr(SvCUR(PL_parser->linestr)+1+len-highhalf);
	    bufptr = PL_parser->bufptr;
	    Move(bufptr, bufptr+len-highhalf, PL_parser->bufend+1-bufptr, char);
	    SvCUR_set(PL_parser->linestr,
	    	SvCUR(PL_parser->linestr) + len-highhalf);
	    PL_parser->bufend += len-highhalf;
	    for (p = pv; p != e; p++) {
		U8 c = (U8)*p;
		if (c & 0x80) {
		    *bufptr++ = (char)(((c & 0x3) << 6) | (p[1] & 0x3f));
		    p++;
		} else {
		    *bufptr++ = (char)c;
		}
	    }
	} else {
	    plain_copy:
	    lex_grow_linestr(SvCUR(PL_parser->linestr)+1+len);
	    bufptr = PL_parser->bufptr;
	    Move(bufptr, bufptr+len, PL_parser->bufend+1-bufptr, char);
	    SvCUR_set(PL_parser->linestr, SvCUR(PL_parser->linestr) + len);
	    PL_parser->bufend += len;
	    Copy(pv, bufptr, len, char);
	}
    }
}

/*
=for apidoc Amx|void|lex_stuff_sv|SV *sv|U32 flags

Insert characters into the lexer buffer (L</PL_parser-E<gt>linestr>),
immediately after the current lexing point (L</PL_parser-E<gt>bufptr>),
reallocating the buffer if necessary.  This means that lexing code that
runs later will see the characters as if they had appeared in the input.
It is not recommended to do this as part of normal parsing, and most
uses of this facility run the risk of the inserted characters being
interpreted in an unintended manner.

The string to be inserted is the string value of I<sv>.  The characters
are recoded for the lexer buffer, according to how the buffer is currently
being interpreted (L</lex_bufutf8>).  If a string to be interpreted is
not already a Perl scalar, the L</lex_stuff_pvn> function avoids the
need to construct a scalar.

=cut
*/

void
Perl_lex_stuff_sv(pTHX_ SV *sv, U32 flags)
{
    char *pv;
    STRLEN len;
    PERL_ARGS_ASSERT_LEX_STUFF_SV;
    if (flags)
	Perl_croak(aTHX_ "Lexing code internal error (%s)", "lex_stuff_sv");
    pv = SvPV(sv, len);
    lex_stuff_pvn(pv, len, flags | (SvUTF8(sv) ? LEX_STUFF_UTF8 : 0));
}

/*
=for apidoc Amx|void|lex_unstuff|char *ptr

Discards text about to be lexed, from L</PL_parser-E<gt>bufptr> up to
I<ptr>.  Text following I<ptr> will be moved, and the buffer shortened.
This hides the discarded text from any lexing code that runs later,
as if the text had never appeared.

This is not the normal way to consume lexed text.  For that, use
L</lex_read_to>.

=cut
*/

void
Perl_lex_unstuff(pTHX_ char *ptr)
{
    char *buf, *bufend;
    STRLEN unstuff_len;
    PERL_ARGS_ASSERT_LEX_UNSTUFF;
    buf = PL_parser->bufptr;
    if (ptr < buf)
	Perl_croak(aTHX_ "Lexing code internal error (%s)", "lex_unstuff");
    if (ptr == buf)
	return;
    bufend = PL_parser->bufend;
    if (ptr > bufend)
	Perl_croak(aTHX_ "Lexing code internal error (%s)", "lex_unstuff");
    unstuff_len = ptr - buf;
    Move(ptr, buf, bufend+1-ptr, char);
    SvCUR_set(PL_parser->linestr, SvCUR(PL_parser->linestr) - unstuff_len);
    PL_parser->bufend = bufend - unstuff_len;
}

/*
=for apidoc Amx|void|lex_read_to|char *ptr

Consume text in the lexer buffer, from L</PL_parser-E<gt>bufptr> up
to I<ptr>.  This advances L</PL_parser-E<gt>bufptr> to match I<ptr>,
performing the correct bookkeeping whenever a newline character is passed.
This is the normal way to consume lexed text.

Interpretation of the buffer's octets can be abstracted out by
using the slightly higher-level functions L</lex_peek_unichar> and
L</lex_read_unichar>.

=cut
*/

void
Perl_lex_read_to(pTHX_ char *ptr)
{
    char *s;
    PERL_ARGS_ASSERT_LEX_READ_TO;
    s = PL_parser->bufptr;
    if (ptr < s || ptr > PL_parser->bufend)
	Perl_croak(aTHX_ "Lexing code internal error (%s)", "lex_read_to");
    for (; s != ptr; s++)
	if (*s == '\n') {
	    CopLINE_inc(PL_curcop);
	    PL_parser->linestart = s+1;
	}
    PL_parser->bufptr = ptr;
}

/*
=for apidoc Amx|void|lex_discard_to|char *ptr

Discards the first part of the L</PL_parser-E<gt>linestr> buffer,
up to I<ptr>.  The remaining content of the buffer will be moved, and
all pointers into the buffer updated appropriately.  I<ptr> must not
be later in the buffer than the position of L</PL_parser-E<gt>bufptr>:
it is not permitted to discard text that has yet to be lexed.

Normally it is not necessarily to do this directly, because it suffices to
use the implicit discarding behaviour of L</lex_next_chunk> and things
based on it.  However, if a token stretches across multiple lines,
and the lexing code has kept multiple lines of text in the buffer fof
that purpose, then after completion of the token it would be wise to
explicitly discard the now-unneeded earlier lines, to avoid future
multi-line tokens growing the buffer without bound.

=cut
*/

void
Perl_lex_discard_to(pTHX_ char *ptr)
{
    char *buf;
    STRLEN discard_len;
    PERL_ARGS_ASSERT_LEX_DISCARD_TO;
    buf = SvPVX(PL_parser->linestr);
    if (ptr < buf)
	Perl_croak(aTHX_ "Lexing code internal error (%s)", "lex_discard_to");
    if (ptr == buf)
	return;
    if (ptr > PL_parser->bufptr)
	Perl_croak(aTHX_ "Lexing code internal error (%s)", "lex_discard_to");
    discard_len = ptr - buf;
    if (PL_parser->oldbufptr < ptr)
	PL_parser->oldbufptr = ptr;
    if (PL_parser->oldoldbufptr < ptr)
	PL_parser->oldoldbufptr = ptr;
    if (PL_parser->last_uni && PL_parser->last_uni < ptr)
	PL_parser->last_uni = NULL;
    if (PL_parser->last_lop && PL_parser->last_lop < ptr)
	PL_parser->last_lop = NULL;
    Move(ptr, buf, PL_parser->bufend+1-ptr, char);
    SvCUR_set(PL_parser->linestr, SvCUR(PL_parser->linestr) - discard_len);
    PL_parser->bufend -= discard_len;
    PL_parser->bufptr -= discard_len;
    PL_parser->oldbufptr -= discard_len;
    PL_parser->oldoldbufptr -= discard_len;
    if (PL_parser->last_uni)
	PL_parser->last_uni -= discard_len;
    if (PL_parser->last_lop)
	PL_parser->last_lop -= discard_len;
}

/*
=for apidoc Amx|bool|lex_next_chunk|U32 flags

Reads in the next chunk of text to be lexed, appending it to
L</PL_parser-E<gt>linestr>.  This should be called when lexing code has
looked to the end of the current chunk and wants to know more.  It is
usual, but not necessary, for lexing to have consumed the entirety of
the current chunk at this time.

If L</PL_parser-E<gt>bufptr> is pointing to the very end of the current
chunk (i.e., the current chunk has been entirely consumed), normally the
current chunk will be discarded at the same time that the new chunk is
read in.  If I<flags> includes C<LEX_KEEP_PREVIOUS>, the current chunk
will not be discarded.  If the current chunk has not been entirely
consumed, then it will not be discarded regardless of the flag.

Returns true if some new text was added to the buffer, or false if the
buffer has reached the end of the input text.

=cut
*/

#define LEX_FAKE_EOF 0x80000000

bool
Perl_lex_next_chunk(pTHX_ U32 flags)
{
    SV *linestr;
    char *buf;
    STRLEN old_bufend_pos, new_bufend_pos;
    STRLEN bufptr_pos, oldbufptr_pos, oldoldbufptr_pos;
    STRLEN linestart_pos, last_uni_pos, last_lop_pos;
    bool got_some_for_debugger = 0;
    bool got_some;
    if (flags & ~(LEX_KEEP_PREVIOUS|LEX_FAKE_EOF))
	Perl_croak(aTHX_ "Lexing code internal error (%s)", "lex_next_chunk");
    linestr = PL_parser->linestr;
    buf = SvPVX(linestr);
    if (!(flags & LEX_KEEP_PREVIOUS) &&
	    PL_parser->bufptr == PL_parser->bufend) {
	old_bufend_pos = bufptr_pos = oldbufptr_pos = oldoldbufptr_pos = 0;
	linestart_pos = 0;
	if (PL_parser->last_uni != PL_parser->bufend)
	    PL_parser->last_uni = NULL;
	if (PL_parser->last_lop != PL_parser->bufend)
	    PL_parser->last_lop = NULL;
	last_uni_pos = last_lop_pos = 0;
	*buf = 0;
	SvCUR(linestr) = 0;
    } else {
	old_bufend_pos = PL_parser->bufend - buf;
	bufptr_pos = PL_parser->bufptr - buf;
	oldbufptr_pos = PL_parser->oldbufptr - buf;
	oldoldbufptr_pos = PL_parser->oldoldbufptr - buf;
	linestart_pos = PL_parser->linestart - buf;
	last_uni_pos = PL_parser->last_uni ? PL_parser->last_uni - buf : 0;
	last_lop_pos = PL_parser->last_lop ? PL_parser->last_lop - buf : 0;
    }
    if (flags & LEX_FAKE_EOF) {
	goto eof;
    } else if (!PL_parser->rsfp) {
	got_some = 0;
    } else if (filter_gets(linestr, old_bufend_pos)) {
	got_some = 1;
	got_some_for_debugger = 1;
    } else {
	if (!SvPOK(linestr))   /* can get undefined by filter_gets */
	    sv_setpvs(linestr, "");
	eof:
	/* End of real input.  Close filehandle (unless it was STDIN),
	 * then add implicit termination.
	 */
	if ((PerlIO*)PL_parser->rsfp == PerlIO_stdin())
	    PerlIO_clearerr(PL_parser->rsfp);
	else if (PL_parser->rsfp)
	    (void)PerlIO_close(PL_parser->rsfp);
	PL_parser->rsfp = NULL;
	PL_doextract = FALSE;
#ifdef PERL_MAD
	if (PL_madskills && !PL_in_eval && (PL_minus_p || PL_minus_n))
	    PL_faketokens = 1;
#endif
	if (!PL_in_eval && PL_minus_p) {
	    sv_catpvs(linestr,
		/*{*/";}continue{print or die qq(-p destination: $!\\n);}");
	    PL_minus_n = PL_minus_p = 0;
	} else if (!PL_in_eval && PL_minus_n) {
	    sv_catpvs(linestr, /*{*/";}");
	    PL_minus_n = 0;
	} else
	    sv_catpvs(linestr, ";");
	got_some = 1;
    }
    buf = SvPVX(linestr);
    new_bufend_pos = SvCUR(linestr);
    PL_parser->bufend = buf + new_bufend_pos;
    PL_parser->bufptr = buf + bufptr_pos;
    PL_parser->oldbufptr = buf + oldbufptr_pos;
    PL_parser->oldoldbufptr = buf + oldoldbufptr_pos;
    PL_parser->linestart = buf + linestart_pos;
    if (PL_parser->last_uni)
	PL_parser->last_uni = buf + last_uni_pos;
    if (PL_parser->last_lop)
	PL_parser->last_lop = buf + last_lop_pos;
    if (got_some_for_debugger && (PERLDB_LINE || PERLDB_SAVESRC) &&
	    PL_curstash != PL_debstash) {
	/* debugger active and we're not compiling the debugger code,
	 * so store the line into the debugger's array of lines
	 */
	update_debugger_info(NULL, buf+old_bufend_pos,
	    new_bufend_pos-old_bufend_pos);
    }
    return got_some;
}

/*
=for apidoc Amx|I32|lex_peek_unichar|U32 flags

Looks ahead one (Unicode) character in the text currently being lexed.
Returns the codepoint (unsigned integer value) of the next character,
or -1 if lexing has reached the end of the input text.  To consume the
peeked character, use L</lex_read_unichar>.

If the next character is in (or extends into) the next chunk of input
text, the next chunk will be read in.  Normally the current chunk will be
discarded at the same time, but if I<flags> includes C<LEX_KEEP_PREVIOUS>
then the current chunk will not be discarded.

If the input is being interpreted as UTF-8 and a UTF-8 encoding error
is encountered, an exception is generated.

=cut
*/

I32
Perl_lex_peek_unichar(pTHX_ U32 flags)
{
    dVAR;
    char *s, *bufend;
    if (flags & ~(LEX_KEEP_PREVIOUS))
	Perl_croak(aTHX_ "Lexing code internal error (%s)", "lex_peek_unichar");
    s = PL_parser->bufptr;
    bufend = PL_parser->bufend;
    if (UTF) {
	U8 head;
	I32 unichar;
	STRLEN len, retlen;
	if (s == bufend) {
	    if (!lex_next_chunk(flags))
		return -1;
	    s = PL_parser->bufptr;
	    bufend = PL_parser->bufend;
	}
	head = (U8)*s;
	if (!(head & 0x80))
	    return head;
	if (head & 0x40) {
	    len = PL_utf8skip[head];
	    while ((STRLEN)(bufend-s) < len) {
		if (!lex_next_chunk(flags | LEX_KEEP_PREVIOUS))
		    break;
		s = PL_parser->bufptr;
		bufend = PL_parser->bufend;
	    }
	}
	unichar = utf8n_to_uvuni((U8*)s, bufend-s, &retlen, UTF8_CHECK_ONLY);
	if (retlen == (STRLEN)-1) {
	    /* malformed UTF-8 */
	    ENTER;
	    SAVESPTR(PL_warnhook);
	    PL_warnhook = PERL_WARNHOOK_FATAL;
	    utf8n_to_uvuni((U8*)s, bufend-s, NULL, 0);
	    LEAVE;
	}
	return unichar;
    } else {
	if (s == bufend) {
	    if (!lex_next_chunk(flags))
		return -1;
	    s = PL_parser->bufptr;
	}
	return (U8)*s;
    }
}

/*
=for apidoc Amx|I32|lex_read_unichar|U32 flags

Reads the next (Unicode) character in the text currently being lexed.
Returns the codepoint (unsigned integer value) of the character read,
and moves L</PL_parser-E<gt>bufptr> past the character, or returns -1
if lexing has reached the end of the input text.  To non-destructively
examine the next character, use L</lex_peek_unichar> instead.

If the next character is in (or extends into) the next chunk of input
text, the next chunk will be read in.  Normally the current chunk will be
discarded at the same time, but if I<flags> includes C<LEX_KEEP_PREVIOUS>
then the current chunk will not be discarded.

If the input is being interpreted as UTF-8 and a UTF-8 encoding error
is encountered, an exception is generated.

=cut
*/

I32
Perl_lex_read_unichar(pTHX_ U32 flags)
{
    I32 c;
    if (flags & ~(LEX_KEEP_PREVIOUS))
	Perl_croak(aTHX_ "Lexing code internal error (%s)", "lex_read_unichar");
    c = lex_peek_unichar(flags);
    if (c != -1) {
	if (c == '\n')
	    CopLINE_inc(PL_curcop);
	PL_parser->bufptr += UTF8SKIP(PL_parser->bufptr);
    }
    return c;
}

/*
=for apidoc Amx|void|lex_read_space|U32 flags

Reads optional spaces, in Perl style, in the text currently being
lexed.  The spaces may include ordinary whitespace characters and
Perl-style comments.  C<#line> directives are processed if encountered.
L</PL_parser-E<gt>bufptr> is moved past the spaces, so that it points
at a non-space character (or the end of the input text).

If spaces extend into the next chunk of input text, the next chunk will
be read in.  Normally the current chunk will be discarded at the same
time, but if I<flags> includes C<LEX_KEEP_PREVIOUS> then the current
chunk will not be discarded.

=cut
*/

#define LEX_NO_NEXT_CHUNK 0x80000000

void
Perl_lex_read_space(pTHX_ U32 flags)
{
    char *s, *bufend;
    bool need_incline = 0;
    if (flags & ~(LEX_KEEP_PREVIOUS|LEX_NO_NEXT_CHUNK))
	Perl_croak(aTHX_ "Lexing code internal error (%s)", "lex_read_space");
#ifdef PERL_MAD
    if (PL_skipwhite) {
	sv_free(PL_skipwhite);
	PL_skipwhite = NULL;
    }
    if (PL_madskills)
	PL_skipwhite = newSVpvs("");
#endif /* PERL_MAD */
    s = PL_parser->bufptr;
    bufend = PL_parser->bufend;
    while (1) {
	char c = *s;
	if (c == '#') {
	    do {
		c = *++s;
	    } while (!(c == '\n' || (c == 0 && s == bufend)));
	} else if (c == '\n') {
	    s++;
	    PL_parser->linestart = s;
	    if (s == bufend)
		need_incline = 1;
	    else
		incline(s);
	} else if (isSPACE(c)) {
	    s++;
	} else if (c == 0 && s == bufend) {
	    bool got_more;
#ifdef PERL_MAD
	    if (PL_madskills)
		sv_catpvn(PL_skipwhite, PL_parser->bufptr, s-PL_parser->bufptr);
#endif /* PERL_MAD */
	    if (flags & LEX_NO_NEXT_CHUNK)
		break;
	    PL_parser->bufptr = s;
	    CopLINE_inc(PL_curcop);
	    got_more = lex_next_chunk(flags);
	    CopLINE_dec(PL_curcop);
	    s = PL_parser->bufptr;
	    bufend = PL_parser->bufend;
	    if (!got_more)
		break;
	    if (need_incline && PL_parser->rsfp) {
		incline(s);
		need_incline = 0;
	    }
	} else {
	    break;
	}
    }
#ifdef PERL_MAD
    if (PL_madskills)
	sv_catpvn(PL_skipwhite, PL_parser->bufptr, s-PL_parser->bufptr);
#endif /* PERL_MAD */
    PL_parser->bufptr = s;
}

/*
 * S_incline
 * This subroutine has nothing to do with tilting, whether at windmills
 * or pinball tables.  Its name is short for "increment line".  It
 * increments the current line number in CopLINE(PL_curcop) and checks
 * to see whether the line starts with a comment of the form
 *    # line 500 "foo.pm"
 * If so, it sets the current line number and file to the values in the comment.
 */

STATIC void
S_incline(pTHX_ const char *s)
{
    dVAR;
    const char *t;
    const char *n;
    const char *e;

    PERL_ARGS_ASSERT_INCLINE;

    CopLINE_inc(PL_curcop);
    if (*s++ != '#')
	return;
    while (SPACE_OR_TAB(*s))
	s++;
    if (strnEQ(s, "line", 4))
	s += 4;
    else
	return;
    if (SPACE_OR_TAB(*s))
	s++;
    else
	return;
    while (SPACE_OR_TAB(*s))
	s++;
    if (!isDIGIT(*s))
	return;

    n = s;
    while (isDIGIT(*s))
	s++;
    if (!SPACE_OR_TAB(*s) && *s != '\r' && *s != '\n' && *s != '\0')
	return;
    while (SPACE_OR_TAB(*s))
	s++;
    if (*s == '"' && (t = strchr(s+1, '"'))) {
	s++;
	e = t + 1;
    }
    else {
	t = s;
	while (!isSPACE(*t))
	    t++;
	e = t;
    }
    while (SPACE_OR_TAB(*e) || *e == '\r' || *e == '\f')
	e++;
    if (*e != '\n' && *e != '\0')
	return;		/* false alarm */

    if (t - s > 0) {
	const STRLEN len = t - s;
#ifndef USE_ITHREADS
	SV *const temp_sv = CopFILESV(PL_curcop);
	const char *cf;
	STRLEN tmplen;

	if (temp_sv) {
	    cf = SvPVX(temp_sv);
	    tmplen = SvCUR(temp_sv);
	} else {
	    cf = NULL;
	    tmplen = 0;
	}

	if (tmplen > 7 && strnEQ(cf, "(eval ", 6)) {
	    /* must copy *{"::_<(eval N)[oldfilename:L]"}
	     * to *{"::_<newfilename"} */
	    /* However, the long form of evals is only turned on by the
	       debugger - usually they're "(eval %lu)" */
	    char smallbuf[128];
	    char *tmpbuf;
	    GV **gvp;
	    STRLEN tmplen2 = len;
	    if (tmplen + 2 <= sizeof smallbuf)
		tmpbuf = smallbuf;
	    else
		Newx(tmpbuf, tmplen + 2, char);
	    tmpbuf[0] = '_';
	    tmpbuf[1] = '<';
	    memcpy(tmpbuf + 2, cf, tmplen);
	    tmplen += 2;
	    gvp = (GV**)hv_fetch(PL_defstash, tmpbuf, tmplen, FALSE);
	    if (gvp) {
		char *tmpbuf2;
		GV *gv2;

		if (tmplen2 + 2 <= sizeof smallbuf)
		    tmpbuf2 = smallbuf;
		else
		    Newx(tmpbuf2, tmplen2 + 2, char);

		if (tmpbuf2 != smallbuf || tmpbuf != smallbuf) {
		    /* Either they malloc'd it, or we malloc'd it,
		       so no prefix is present in ours.  */
		    tmpbuf2[0] = '_';
		    tmpbuf2[1] = '<';
		}

		memcpy(tmpbuf2 + 2, s, tmplen2);
		tmplen2 += 2;

		gv2 = *(GV**)hv_fetch(PL_defstash, tmpbuf2, tmplen2, TRUE);
		if (!isGV(gv2)) {
		    gv_init(gv2, PL_defstash, tmpbuf2, tmplen2, FALSE);
		    /* adjust ${"::_<newfilename"} to store the new file name */
		    GvSV(gv2) = newSVpvn(tmpbuf2 + 2, tmplen2 - 2);
		    GvHV(gv2) = MUTABLE_HV(SvREFCNT_inc(GvHV(*gvp)));
		    GvAV(gv2) = MUTABLE_AV(SvREFCNT_inc(GvAV(*gvp)));
		}

		if (tmpbuf2 != smallbuf) Safefree(tmpbuf2);
	    }
	    if (tmpbuf != smallbuf) Safefree(tmpbuf);
	}
#endif
	CopFILE_free(PL_curcop);
	CopFILE_setn(PL_curcop, s, len);
    }
    CopLINE_set(PL_curcop, atoi(n)-1);
}

#ifdef PERL_MAD
/* skip space before PL_thistoken */

STATIC char *
S_skipspace0(pTHX_ register char *s)
{
    PERL_ARGS_ASSERT_SKIPSPACE0;

    s = skipspace(s);
    if (!PL_madskills)
	return s;
    if (PL_skipwhite) {
	if (!PL_thiswhite)
	    PL_thiswhite = newSVpvs("");
	sv_catsv(PL_thiswhite, PL_skipwhite);
	sv_free(PL_skipwhite);
	PL_skipwhite = 0;
    }
    PL_realtokenstart = s - SvPVX(PL_linestr);
    return s;
}

/* skip space after PL_thistoken */

STATIC char *
S_skipspace1(pTHX_ register char *s)
{
    const char *start = s;
    I32 startoff = start - SvPVX(PL_linestr);

    PERL_ARGS_ASSERT_SKIPSPACE1;

    s = skipspace(s);
    if (!PL_madskills)
	return s;
    start = SvPVX(PL_linestr) + startoff;
    if (!PL_thistoken && PL_realtokenstart >= 0) {
	const char * const tstart = SvPVX(PL_linestr) + PL_realtokenstart;
	PL_thistoken = newSVpvn(tstart, start - tstart);
    }
    PL_realtokenstart = -1;
    if (PL_skipwhite) {
	if (!PL_nextwhite)
	    PL_nextwhite = newSVpvs("");
	sv_catsv(PL_nextwhite, PL_skipwhite);
	sv_free(PL_skipwhite);
	PL_skipwhite = 0;
    }
    return s;
}

STATIC char *
S_skipspace2(pTHX_ register char *s, SV **svp)
{
    char *start;
    const I32 bufptroff = PL_bufptr - SvPVX(PL_linestr);
    const I32 startoff = s - SvPVX(PL_linestr);

    PERL_ARGS_ASSERT_SKIPSPACE2;

    s = skipspace(s);
    PL_bufptr = SvPVX(PL_linestr) + bufptroff;
    if (!PL_madskills || !svp)
	return s;
    start = SvPVX(PL_linestr) + startoff;
    if (!PL_thistoken && PL_realtokenstart >= 0) {
	char * const tstart = SvPVX(PL_linestr) + PL_realtokenstart;
	PL_thistoken = newSVpvn(tstart, start - tstart);
	PL_realtokenstart = -1;
    }
    if (PL_skipwhite) {
	if (!*svp)
	    *svp = newSVpvs("");
	sv_setsv(*svp, PL_skipwhite);
	sv_free(PL_skipwhite);
	PL_skipwhite = 0;
    }
    
    return s;
}
#endif

STATIC void
S_update_debugger_info(pTHX_ SV *orig_sv, const char *const buf, STRLEN len)
{
    AV *av = CopFILEAVx(PL_curcop);
    if (av) {
	SV * const sv = newSV_type(SVt_PVMG);
	if (orig_sv)
	    sv_setsv(sv, orig_sv);
	else
	    sv_setpvn(sv, buf, len);
	(void)SvIOK_on(sv);
	SvIV_set(sv, 0);
	av_store(av, (I32)CopLINE(PL_curcop), sv);
    }
}

/*
 * S_skipspace
 * Called to gobble the appropriate amount and type of whitespace.
 * Skips comments as well.
 */

STATIC char *
S_skipspace(pTHX_ register char *s)
{
#ifdef PERL_MAD
    char *start = s;
#endif /* PERL_MAD */
    PERL_ARGS_ASSERT_SKIPSPACE;
#ifdef PERL_MAD
    if (PL_skipwhite) {
	sv_free(PL_skipwhite);
	PL_skipwhite = NULL;
    }
#endif /* PERL_MAD */
    if (PL_lex_formbrack && PL_lex_brackets <= PL_lex_formbrack) {
	while (s < PL_bufend && SPACE_OR_TAB(*s))
	    s++;
    } else {
	STRLEN bufptr_pos = PL_bufptr - SvPVX(PL_linestr);
	PL_bufptr = s;
	lex_read_space(LEX_KEEP_PREVIOUS |
		(PL_sublex_info.sub_inwhat || PL_lex_state == LEX_FORMLINE ?
		    LEX_NO_NEXT_CHUNK : 0));
	s = PL_bufptr;
	PL_bufptr = SvPVX(PL_linestr) + bufptr_pos;
	if (PL_linestart > PL_bufptr)
	    PL_bufptr = PL_linestart;
	return s;
    }
#ifdef PERL_MAD
    if (PL_madskills)
	PL_skipwhite = newSVpvn(start, s-start);
#endif /* PERL_MAD */
    return s;
}

/*
 * S_check_uni
 * Check the unary operators to ensure there's no ambiguity in how they're
 * used.  An ambiguous piece of code would be:
 *     rand + 5
 * This doesn't mean rand() + 5.  Because rand() is a unary operator,
 * the +5 is its argument.
 */

STATIC void
S_check_uni(pTHX)
{
    dVAR;
    const char *s;
    const char *t;

    if (PL_oldoldbufptr != PL_last_uni)
	return;
    while (isSPACE(*PL_last_uni))
	PL_last_uni++;
    s = PL_last_uni;
    while (isALNUM_lazy_if(s,UTF) || *s == '-')
	s++;
    if ((t = strchr(s, '(')) && t < PL_bufptr)
	return;

    Perl_ck_warner_d(aTHX_ packWARN(WARN_AMBIGUOUS),
		     "Warning: Use of \"%.*s\" without parentheses is ambiguous",
		     (int)(s - PL_last_uni), PL_last_uni);
}

/*
 * LOP : macro to build a list operator.  Its behaviour has been replaced
 * with a subroutine, S_lop() for which LOP is just another name.
 */

#define LOP(f,x) return lop(f,x,s)

/*
 * S_lop
 * Build a list operator (or something that might be one).  The rules:
 *  - if we have a next token, then it's a list operator [why?]
 *  - if the next thing is an opening paren, then it's a function
 *  - else it's a list operator
 */

STATIC I32
S_lop(pTHX_ I32 f, int x, char *s)
{
    dVAR;

    PERL_ARGS_ASSERT_LOP;

    pl_yylval.ival = f;
    CLINE;
    PL_expect = x;
    PL_bufptr = s;
    PL_last_lop = PL_oldbufptr;
    PL_last_lop_op = (OPCODE)f;
#ifdef PERL_MAD
    if (PL_lasttoke)
 	return REPORT(LSTOP);
#else
    if (PL_nexttoke)
	return REPORT(LSTOP);
#endif
    if (*s == '(')
	return REPORT(FUNC);
    s = PEEKSPACE(s);
    if (*s == '(')
	return REPORT(FUNC);
    else
	return REPORT(LSTOP);
}

#ifdef PERL_MAD
 /*
 * S_start_force
 * Sets up for an eventual force_next().  start_force(0) basically does
 * an unshift, while start_force(-1) does a push.  yylex removes items
 * on the "pop" end.
 */

STATIC void
S_start_force(pTHX_ int where)
{
    int i;

    if (where < 0)	/* so people can duplicate start_force(PL_curforce) */
	where = PL_lasttoke;
    assert(PL_curforce < 0 || PL_curforce == where);
    if (PL_curforce != where) {
	for (i = PL_lasttoke; i > where; --i) {
	    PL_nexttoke[i] = PL_nexttoke[i-1];
	}
	PL_lasttoke++;
    }
    if (PL_curforce < 0)	/* in case of duplicate start_force() */
	Zero(&PL_nexttoke[where], 1, NEXTTOKE);
    PL_curforce = where;
    if (PL_nextwhite) {
	if (PL_madskills)
	    curmad('^', newSVpvs(""));
	CURMAD('_', PL_nextwhite);
    }
}

STATIC void
S_curmad(pTHX_ char slot, SV *sv)
{
    MADPROP **where;

    if (!sv)
	return;
    if (PL_curforce < 0)
	where = &PL_thismad;
    else
	where = &PL_nexttoke[PL_curforce].next_mad;

    if (PL_faketokens)
	sv_setpvs(sv, "");
    else {
	if (!IN_BYTES) {
	    if (UTF && is_utf8_string((U8*)SvPVX(sv), SvCUR(sv)))
		SvUTF8_on(sv);
	    else if (PL_encoding) {
		sv_recode_to_utf8(sv, PL_encoding);
	    }
	}
    }

    /* keep a slot open for the head of the list? */
    if (slot != '_' && *where && (*where)->mad_key == '^') {
	(*where)->mad_key = slot;
	sv_free(MUTABLE_SV(((*where)->mad_val)));
	(*where)->mad_val = (void*)sv;
    }
    else
	addmad(newMADsv(slot, sv), where, 0);
}
#else
#  define start_force(where)    NOOP
#  define curmad(slot, sv)      NOOP
#endif

/*
 * S_force_next
 * When the lexer realizes it knows the next token (for instance,
 * it is reordering tokens for the parser) then it can call S_force_next
 * to know what token to return the next time the lexer is called.  Caller
 * will need to set PL_nextval[] (or PL_nexttoke[].next_val with PERL_MAD),
 * and possibly PL_expect to ensure the lexer handles the token correctly.
 */

STATIC void
S_force_next(pTHX_ I32 type)
{
    dVAR;
#ifdef DEBUGGING
    if (DEBUG_T_TEST) {
        PerlIO_printf(Perl_debug_log, "### forced token:\n");
	tokereport(type, &NEXTVAL_NEXTTOKE);
    }
#endif
#ifdef PERL_MAD
    if (PL_curforce < 0)
	start_force(PL_lasttoke);
    PL_nexttoke[PL_curforce].next_type = type;
    if (PL_lex_state != LEX_KNOWNEXT)
 	PL_lex_defer = PL_lex_state;
    PL_lex_state = LEX_KNOWNEXT;
    PL_lex_expect = PL_expect;
    PL_curforce = -1;
#else
    PL_nexttype[PL_nexttoke] = type;
    PL_nexttoke++;
    if (PL_lex_state != LEX_KNOWNEXT) {
	PL_lex_defer = PL_lex_state;
	PL_lex_expect = PL_expect;
	PL_lex_state = LEX_KNOWNEXT;
    }
#endif
}

STATIC SV *
S_newSV_maybe_utf8(pTHX_ const char *const start, STRLEN len)
{
    dVAR;
    SV * const sv = newSVpvn_utf8(start, len,
				  !IN_BYTES
				  && UTF
				  && !is_ascii_string((const U8*)start, len)
				  && is_utf8_string((const U8*)start, len));
    return sv;
}

/*
 * S_force_word
 * When the lexer knows the next thing is a word (for instance, it has
 * just seen -> and it knows that the next char is a word char, then
 * it calls S_force_word to stick the next word into the PL_nexttoke/val
 * lookahead.
 *
 * Arguments:
 *   char *start : buffer position (must be within PL_linestr)
 *   int token   : PL_next* will be this type of bare word (e.g., METHOD,WORD)
 *   int check_keyword : if true, Perl checks to make sure the word isn't
 *       a keyword (do this if the word is a label, e.g. goto FOO)
 *   int allow_pack : if true, : characters will also be allowed (require,
 *       use, etc. do this)
 *   int allow_initial_tick : used by the "sub" lexer only.
 */

STATIC char *
S_force_word(pTHX_ register char *start, int token, int check_keyword, int allow_pack, int allow_initial_tick)
{
    dVAR;
    register char *s;
    STRLEN len;

    PERL_ARGS_ASSERT_FORCE_WORD;

    start = SKIPSPACE1(start);
    s = start;
    if (isIDFIRST_lazy_if(s,UTF) ||
	(allow_pack && *s == ':') ||
	(allow_initial_tick && *s == '\'') )
    {
	s = scan_word(s, PL_tokenbuf, sizeof PL_tokenbuf, allow_pack, &len);
	if (check_keyword && keyword(PL_tokenbuf, len, 0))
	    return start;
	start_force(PL_curforce);
	if (PL_madskills)
	    curmad('X', newSVpvn(start,s-start));
	if (token == METHOD) {
	    s = SKIPSPACE1(s);
	    if (*s == '(')
		PL_expect = XTERM;
	    else {
		PL_expect = XOPERATOR;
	    }
	}
	if (PL_madskills)
	    curmad('g', newSVpvs( "forced" ));
	NEXTVAL_NEXTTOKE.opval
	    = (OP*)newSVOP(OP_CONST,0,
			   S_newSV_maybe_utf8(aTHX_ PL_tokenbuf, len));
	NEXTVAL_NEXTTOKE.opval->op_private |= OPpCONST_BARE;
	force_next(token);
    }
    return s;
}

/*
 * S_force_ident
 * Called when the lexer wants $foo *foo &foo etc, but the program
 * text only contains the "foo" portion.  The first argument is a pointer
 * to the "foo", and the second argument is the type symbol to prefix.
 * Forces the next token to be a "WORD".
 * Creates the symbol if it didn't already exist (via gv_fetchpv()).
 */

STATIC void
S_force_ident(pTHX_ register const char *s, int kind)
{
    dVAR;

    PERL_ARGS_ASSERT_FORCE_IDENT;

    if (*s) {
	const STRLEN len = strlen(s);
	OP* const o = (OP*)newSVOP(OP_CONST, 0, newSVpvn(s, len));
	start_force(PL_curforce);
	NEXTVAL_NEXTTOKE.opval = o;
	force_next(WORD);
	if (kind) {
	    o->op_private = OPpCONST_ENTERED;
	    /* XXX see note in pp_entereval() for why we forgo typo
	       warnings if the symbol must be introduced in an eval.
	       GSAR 96-10-12 */
	    gv_fetchpvn_flags(s, len,
			      PL_in_eval ? (GV_ADDMULTI | GV_ADDINEVAL)
			      : GV_ADD,
			      kind == '$' ? SVt_PV :
			      kind == '@' ? SVt_PVAV :
			      kind == '%' ? SVt_PVHV :
			      SVt_PVGV
			      );
	}
    }
}

NV
Perl_str_to_version(pTHX_ SV *sv)
{
    NV retval = 0.0;
    NV nshift = 1.0;
    STRLEN len;
    const char *start = SvPV_const(sv,len);
    const char * const end = start + len;
    const bool utf = SvUTF8(sv) ? TRUE : FALSE;

    PERL_ARGS_ASSERT_STR_TO_VERSION;

    while (start < end) {
	STRLEN skip;
	UV n;
	if (utf)
	    n = utf8n_to_uvchr((U8*)start, len, &skip, 0);
	else {
	    n = *(U8*)start;
	    skip = 1;
	}
	retval += ((NV)n)/nshift;
	start += skip;
	nshift *= 1000;
    }
    return retval;
}

/*
 * S_force_version
 * Forces the next token to be a version number.
 * If the next token appears to be an invalid version number, (e.g. "v2b"),
 * and if "guessing" is TRUE, then no new token is created (and the caller
 * must use an alternative parsing method).
 */

STATIC char *
S_force_version(pTHX_ char *s, int guessing)
{
    dVAR;
    OP *version = NULL;
    char *d;
#ifdef PERL_MAD
    I32 startoff = s - SvPVX(PL_linestr);
#endif

    PERL_ARGS_ASSERT_FORCE_VERSION;

    s = SKIPSPACE1(s);

    d = s;
    if (*d == 'v')
	d++;
    if (isDIGIT(*d)) {
	while (isDIGIT(*d) || *d == '_' || *d == '.')
	    d++;
#ifdef PERL_MAD
	if (PL_madskills) {
	    start_force(PL_curforce);
	    curmad('X', newSVpvn(s,d-s));
	}
#endif
        if (*d == ';' || isSPACE(*d) || *d == '}' || !*d) {
	    SV *ver;
#ifdef USE_LOCALE_NUMERIC
	    char *loc = setlocale(LC_NUMERIC, "C");
#endif
            s = scan_num(s, &pl_yylval);
#ifdef USE_LOCALE_NUMERIC
	    setlocale(LC_NUMERIC, loc);
#endif
            version = pl_yylval.opval;
	    ver = cSVOPx(version)->op_sv;
	    if (SvPOK(ver) && !SvNIOK(ver)) {
		SvUPGRADE(ver, SVt_PVNV);
		SvNV_set(ver, str_to_version(ver));
		SvNOK_on(ver);		/* hint that it is a version */
	    }
        }
	else if (guessing) {
#ifdef PERL_MAD
	    if (PL_madskills) {
		sv_free(PL_nextwhite);	/* let next token collect whitespace */
		PL_nextwhite = 0;
		s = SvPVX(PL_linestr) + startoff;
	    }
#endif
	    return s;
	}
    }

#ifdef PERL_MAD
    if (PL_madskills && !version) {
	sv_free(PL_nextwhite);	/* let next token collect whitespace */
	PL_nextwhite = 0;
	s = SvPVX(PL_linestr) + startoff;
    }
#endif
    /* NOTE: The parser sees the package name and the VERSION swapped */
    start_force(PL_curforce);
    NEXTVAL_NEXTTOKE.opval = version;
    force_next(WORD);

    return s;
}

/*
 * S_force_strict_version
 * Forces the next token to be a version number using strict syntax rules.
 */

STATIC char *
S_force_strict_version(pTHX_ char *s)
{
    dVAR;
    OP *version = NULL;
#ifdef PERL_MAD
    I32 startoff = s - SvPVX(PL_linestr);
#endif
    const char *errstr = NULL;

    PERL_ARGS_ASSERT_FORCE_STRICT_VERSION;

    while (isSPACE(*s)) /* leading whitespace */
	s++;

    if (is_STRICT_VERSION(s,&errstr)) {
	SV *ver = newSV(0);
	s = (char *)scan_version(s, ver, 0);
	version = newSVOP(OP_CONST, 0, ver);
    }
    else if ( (*s != ';' && *s != '}' ) && (s = SKIPSPACE1(s), (*s != ';' && *s !='}' ))) {
	PL_bufptr = s;
	if (errstr)
	    yyerror(errstr); /* version required */
	return s;
    }

#ifdef PERL_MAD
    if (PL_madskills && !version) {
	sv_free(PL_nextwhite);	/* let next token collect whitespace */
	PL_nextwhite = 0;
	s = SvPVX(PL_linestr) + startoff;
    }
#endif
    /* NOTE: The parser sees the package name and the VERSION swapped */
    start_force(PL_curforce);
    NEXTVAL_NEXTTOKE.opval = version;
    force_next(WORD);

    return s;
}

/*
 * S_tokeq
 * Tokenize a quoted string passed in as an SV.  It finds the next
 * chunk, up to end of string or a backslash.  It may make a new
 * SV containing that chunk (if HINT_NEW_STRING is on).  It also
 * turns \\ into \.
 */

STATIC SV *
S_tokeq(pTHX_ SV *sv)
{
    dVAR;
    register char *s;
    register char *send;
    register char *d;
    STRLEN len = 0;
    SV *pv = sv;

    PERL_ARGS_ASSERT_TOKEQ;

    if (!SvLEN(sv))
	goto finish;

    s = SvPV_force(sv, len);
    if (SvTYPE(sv) >= SVt_PVIV && SvIVX(sv) == -1)
	goto finish;
    send = s + len;
    while (s < send && *s != '\\')
	s++;
    if (s == send)
	goto finish;
    d = s;
    if ( PL_hints & HINT_NEW_STRING ) {
	pv = newSVpvn_flags(SvPVX_const(pv), len, SVs_TEMP | SvUTF8(sv));
    }
    while (s < send) {
	if (*s == '\\') {
	    if (s + 1 < send && (s[1] == '\\'))
		s++;		/* all that, just for this */
	}
	*d++ = *s++;
    }
    *d = '\0';
    SvCUR_set(sv, d - SvPVX_const(sv));
  finish:
    if ( PL_hints & HINT_NEW_STRING )
       return new_constant(NULL, 0, "q", sv, pv, "q", 1);
    return sv;
}

/*
 * Now come three functions related to double-quote context,
 * S_sublex_start, S_sublex_push, and S_sublex_done.  They're used when
 * converting things like "\u\Lgnat" into ucfirst(lc("gnat")).  They
 * interact with PL_lex_state, and create fake ( ... ) argument lists
 * to handle functions and concatenation.
 * They assume that whoever calls them will be setting up a fake
 * join call, because each subthing puts a ',' after it.  This lets
 *   "lower \luPpEr"
 * become
 *  join($, , 'lower ', lcfirst( 'uPpEr', ) ,)
 *
 * (I'm not sure whether the spurious commas at the end of lcfirst's
 * arguments and join's arguments are created or not).
 */

/*
 * S_sublex_start
 * Assumes that pl_yylval.ival is the op we're creating (e.g. OP_LCFIRST).
 *
 * Pattern matching will set PL_lex_op to the pattern-matching op to
 * make (we return THING if pl_yylval.ival is OP_NULL, PMFUNC otherwise).
 *
 * OP_CONST and OP_READLINE are easy--just make the new op and return.
 *
 * Everything else becomes a FUNC.
 *
 * Sets PL_lex_state to LEX_INTERPPUSH unless (ival was OP_NULL or we
 * had an OP_CONST or OP_READLINE).  This just sets us up for a
 * call to S_sublex_push().
 */

STATIC I32
S_sublex_start(pTHX)
{
    dVAR;
    register const I32 op_type = pl_yylval.ival;

    if (op_type == OP_NULL) {
	pl_yylval.opval = PL_lex_op;
	PL_lex_op = NULL;
	return THING;
    }
    if (op_type == OP_CONST || op_type == OP_READLINE) {
	SV *sv = tokeq(PL_lex_stuff);

	if (SvTYPE(sv) == SVt_PVIV) {
	    /* Overloaded constants, nothing fancy: Convert to SVt_PV: */
	    STRLEN len;
	    const char * const p = SvPV_const(sv, len);
	    SV * const nsv = newSVpvn_flags(p, len, SvUTF8(sv));
	    SvREFCNT_dec(sv);
	    sv = nsv;
	}
	pl_yylval.opval = (OP*)newSVOP(op_type, 0, sv);
	PL_lex_stuff = NULL;
	/* Allow <FH> // "foo" */
	if (op_type == OP_READLINE)
	    PL_expect = XTERMORDORDOR;
	return THING;
    }
    else if (op_type == OP_BACKTICK && PL_lex_op) {
	/* readpipe() vas overriden */
	cSVOPx(cLISTOPx(cUNOPx(PL_lex_op)->op_first)->op_first->op_sibling)->op_sv = tokeq(PL_lex_stuff);
	pl_yylval.opval = PL_lex_op;
	PL_lex_op = NULL;
	PL_lex_stuff = NULL;
	return THING;
    }

    PL_sublex_info.super_state = PL_lex_state;
    PL_sublex_info.sub_inwhat = (U16)op_type;
    PL_sublex_info.sub_op = PL_lex_op;
    PL_lex_state = LEX_INTERPPUSH;

    PL_expect = XTERM;
    if (PL_lex_op) {
	pl_yylval.opval = PL_lex_op;
	PL_lex_op = NULL;
	return PMFUNC;
    }
    else
	return FUNC;
}

/*
 * S_sublex_push
 * Create a new scope to save the lexing state.  The scope will be
 * ended in S_sublex_done.  Returns a '(', starting the function arguments
 * to the uc, lc, etc. found before.
 * Sets PL_lex_state to LEX_INTERPCONCAT.
 */

STATIC I32
S_sublex_push(pTHX)
{
    dVAR;
    ENTER;

    PL_lex_state = PL_sublex_info.super_state;
    SAVEBOOL(PL_lex_dojoin);
    SAVEI32(PL_lex_brackets);
    SAVEI32(PL_lex_casemods);
    SAVEI32(PL_lex_starts);
    SAVEI8(PL_lex_state);
    SAVEVPTR(PL_lex_inpat);
    SAVEI16(PL_lex_inwhat);
    SAVECOPLINE(PL_curcop);
    SAVEPPTR(PL_bufptr);
    SAVEPPTR(PL_bufend);
    SAVEPPTR(PL_oldbufptr);
    SAVEPPTR(PL_oldoldbufptr);
    SAVEPPTR(PL_last_lop);
    SAVEPPTR(PL_last_uni);
    SAVEPPTR(PL_linestart);
    SAVESPTR(PL_linestr);
    SAVEGENERICPV(PL_lex_brackstack);
    SAVEGENERICPV(PL_lex_casestack);

    PL_linestr = PL_lex_stuff;
    PL_lex_stuff = NULL;

    PL_bufend = PL_bufptr = PL_oldbufptr = PL_oldoldbufptr = PL_linestart
	= SvPVX(PL_linestr);
    PL_bufend += SvCUR(PL_linestr);
    PL_last_lop = PL_last_uni = NULL;
    SAVEFREESV(PL_linestr);

    PL_lex_dojoin = FALSE;
    PL_lex_brackets = 0;
    Newx(PL_lex_brackstack, 120, char);
    Newx(PL_lex_casestack, 12, char);
    PL_lex_casemods = 0;
    *PL_lex_casestack = '\0';
    PL_lex_starts = 0;
    PL_lex_state = LEX_INTERPCONCAT;
    CopLINE_set(PL_curcop, (line_t)PL_multi_start);

    PL_lex_inwhat = PL_sublex_info.sub_inwhat;
    if (PL_lex_inwhat == OP_MATCH || PL_lex_inwhat == OP_QR || PL_lex_inwhat == OP_SUBST)
	PL_lex_inpat = PL_sublex_info.sub_op;
    else
	PL_lex_inpat = NULL;

    return '(';
}

/*
 * S_sublex_done
 * Restores lexer state after a S_sublex_push.
 */

STATIC I32
S_sublex_done(pTHX)
{
    dVAR;
    if (!PL_lex_starts++) {
	SV * const sv = newSVpvs("");
	if (SvUTF8(PL_linestr))
	    SvUTF8_on(sv);
	PL_expect = XOPERATOR;
	pl_yylval.opval = (OP*)newSVOP(OP_CONST, 0, sv);
	return THING;
    }

    if (PL_lex_casemods) {		/* oops, we've got some unbalanced parens */
	PL_lex_state = LEX_INTERPCASEMOD;
	return yylex();
    }

    /* Is there a right-hand side to take care of? (s//RHS/ or tr//RHS/) */
    if (PL_lex_repl && (PL_lex_inwhat == OP_SUBST || PL_lex_inwhat == OP_TRANS)) {
	PL_linestr = PL_lex_repl;
	PL_lex_inpat = 0;
	PL_bufend = PL_bufptr = PL_oldbufptr = PL_oldoldbufptr = PL_linestart = SvPVX(PL_linestr);
	PL_bufend += SvCUR(PL_linestr);
	PL_last_lop = PL_last_uni = NULL;
	SAVEFREESV(PL_linestr);
	PL_lex_dojoin = FALSE;
	PL_lex_brackets = 0;
	PL_lex_casemods = 0;
	*PL_lex_casestack = '\0';
	PL_lex_starts = 0;
	if (SvEVALED(PL_lex_repl)) {
	    PL_lex_state = LEX_INTERPNORMAL;
	    PL_lex_starts++;
	    /*	we don't clear PL_lex_repl here, so that we can check later
		whether this is an evalled subst; that means we rely on the
		logic to ensure sublex_done() is called again only via the
		branch (in yylex()) that clears PL_lex_repl, else we'll loop */
	}
	else {
	    PL_lex_state = LEX_INTERPCONCAT;
	    PL_lex_repl = NULL;
	}
	return ',';
    }
    else {
#ifdef PERL_MAD
	if (PL_madskills) {
	    if (PL_thiswhite) {
		if (!PL_endwhite)
		    PL_endwhite = newSVpvs("");
		sv_catsv(PL_endwhite, PL_thiswhite);
		PL_thiswhite = 0;
	    }
	    if (PL_thistoken)
		sv_setpvs(PL_thistoken,"");
	    else
		PL_realtokenstart = -1;
	}
#endif
	LEAVE;
	PL_bufend = SvPVX(PL_linestr);
	PL_bufend += SvCUR(PL_linestr);
	PL_expect = XOPERATOR;
	PL_sublex_info.sub_inwhat = 0;
	return ')';
    }
}

/*
  scan_const

  Extracts a pattern, double-quoted string, or transliteration.  This
  is terrifying code.

  It looks at PL_lex_inwhat and PL_lex_inpat to find out whether it's
  processing a pattern (PL_lex_inpat is true), a transliteration
  (PL_lex_inwhat == OP_TRANS is true), or a double-quoted string.

  Returns a pointer to the character scanned up to. If this is
  advanced from the start pointer supplied (i.e. if anything was
  successfully parsed), will leave an OP for the substring scanned
  in pl_yylval. Caller must intuit reason for not parsing further
  by looking at the next characters herself.

  In patterns:
    backslashes:
      constants: \N{NAME} only
      case and quoting: \U \Q \E
    stops on @ and $, but not for $ as tail anchor

  In transliterations:
    characters are VERY literal, except for - not at the start or end
    of the string, which indicates a range. If the range is in bytes,
    scan_const expands the range to the full set of intermediate
    characters. If the range is in utf8, the hyphen is replaced with
    a certain range mark which will be handled by pmtrans() in op.c.

  In double-quoted strings:
    backslashes:
      double-quoted style: \r and \n
      constants: \x31, etc.
      deprecated backrefs: \1 (in substitution replacements)
      case and quoting: \U \Q \E
    stops on @ and $

  scan_const does *not* construct ops to handle interpolated strings.
  It stops processing as soon as it finds an embedded $ or @ variable
  and leaves it to the caller to work out what's going on.

  embedded arrays (whether in pattern or not) could be:
      @foo, @::foo, @'foo, @{foo}, @$foo, @+, @-.

  $ in double-quoted strings must be the symbol of an embedded scalar.

  $ in pattern could be $foo or could be tail anchor.  Assumption:
  it's a tail anchor if $ is the last thing in the string, or if it's
  followed by one of "()| \r\n\t"

  \1 (backreferences) are turned into $1

  The structure of the code is
      while (there's a character to process) {
	  handle transliteration ranges
	  skip regexp comments /(?#comment)/ and codes /(?{code})/
	  skip #-initiated comments in //x patterns
	  check for embedded arrays
	  check for embedded scalars
	  if (backslash) {
	      deprecate \1 in substitution replacements
	      handle string-changing backslashes \l \U \Q \E, etc.
	      switch (what was escaped) {
		  handle \- in a transliteration (becomes a literal -)
		  if a pattern and not \N{, go treat as regular character
		  handle \132 (octal characters)
		  handle \x15 and \x{1234} (hex characters)
		  handle \N{name} (named characters, also \N{3,5} in a pattern)
		  handle \cV (control characters)
		  handle printf-style backslashes (\f, \r, \n, etc)
	      } (end switch)
	      continue
	  } (end if backslash)
          handle regular character
    } (end while character to read)
		
*/

STATIC char *
S_scan_const(pTHX_ char *start)
{
    dVAR;
    register char *send = PL_bufend;		/* end of the constant */
    SV *sv = newSV(send - start);		/* sv for the constant.  See
						   note below on sizing. */
    register char *s = start;			/* start of the constant */
    register char *d = SvPVX(sv);		/* destination for copies */
    bool dorange = FALSE;			/* are we in a translit range? */
    bool didrange = FALSE;		        /* did we just finish a range? */
    I32  has_utf8 = FALSE;			/* Output constant is UTF8 */
    I32  this_utf8 = UTF;			/* Is the source string assumed
						   to be UTF8?  But, this can
						   show as true when the source
						   isn't utf8, as for example
						   when it is entirely composed
						   of hex constants */

    /* Note on sizing:  The scanned constant is placed into sv, which is
     * initialized by newSV() assuming one byte of output for every byte of
     * input.  This routine expects newSV() to allocate an extra byte for a
     * trailing NUL, which this routine will append if it gets to the end of
     * the input.  There may be more bytes of input than output (eg., \N{LATIN
     * CAPITAL LETTER A}), or more output than input if the constant ends up
     * recoded to utf8, but each time a construct is found that might increase
     * the needed size, SvGROW() is called.  Its size parameter each time is
     * based on the best guess estimate at the time, namely the length used so
     * far, plus the length the current construct will occupy, plus room for
     * the trailing NUL, plus one byte for every input byte still unscanned */ 

    UV uv;
#ifdef EBCDIC
    UV literal_endpoint = 0;
    bool native_range = TRUE; /* turned to FALSE if the first endpoint is Unicode. */
#endif

    PERL_ARGS_ASSERT_SCAN_CONST;

    if (PL_lex_inwhat == OP_TRANS && PL_sublex_info.sub_op) {
	/* If we are doing a trans and we know we want UTF8 set expectation */
	has_utf8   = PL_sublex_info.sub_op->op_private & (OPpTRANS_FROM_UTF|OPpTRANS_TO_UTF);
	this_utf8  = PL_sublex_info.sub_op->op_private & (PL_lex_repl ? OPpTRANS_FROM_UTF : OPpTRANS_TO_UTF);
    }


    while (s < send || dorange) {

        /* get transliterations out of the way (they're most literal) */
	if (PL_lex_inwhat == OP_TRANS) {
	    /* expand a range A-Z to the full set of characters.  AIE! */
	    if (dorange) {
		I32 i;				/* current expanded character */
		I32 min;			/* first character in range */
		I32 max;			/* last character in range */

#ifdef EBCDIC
		UV uvmax = 0;
#endif

		if (has_utf8
#ifdef EBCDIC
		    && !native_range
#endif
		    ) {
		    char * const c = (char*)utf8_hop((U8*)d, -1);
		    char *e = d++;
		    while (e-- > c)
			*(e + 1) = *e;
		    *c = (char)UTF_TO_NATIVE(0xff);
		    /* mark the range as done, and continue */
		    dorange = FALSE;
		    didrange = TRUE;
		    continue;
		}

		i = d - SvPVX_const(sv);		/* remember current offset */
#ifdef EBCDIC
                SvGROW(sv,
		       SvLEN(sv) + (has_utf8 ?
				    (512 - UTF_CONTINUATION_MARK +
				     UNISKIP(0x100))
				    : 256));
                /* How many two-byte within 0..255: 128 in UTF-8,
		 * 96 in UTF-8-mod. */
#else
		SvGROW(sv, SvLEN(sv) + 256);	/* never more than 256 chars in a range */
#endif
		d = SvPVX(sv) + i;		/* refresh d after realloc */
#ifdef EBCDIC
                if (has_utf8) {
                    int j;
                    for (j = 0; j <= 1; j++) {
                        char * const c = (char*)utf8_hop((U8*)d, -1);
                        const UV uv    = utf8n_to_uvchr((U8*)c, d - c, NULL, 0);
                        if (j)
                            min = (U8)uv;
                        else if (uv < 256)
                            max = (U8)uv;
                        else {
                            max = (U8)0xff; /* only to \xff */
                            uvmax = uv; /* \x{100} to uvmax */
                        }
                        d = c; /* eat endpoint chars */
                     }
                }
               else {
#endif
		   d -= 2;		/* eat the first char and the - */
		   min = (U8)*d;	/* first char in range */
		   max = (U8)d[1];	/* last char in range  */
#ifdef EBCDIC
	       }
#endif

                if (min > max) {
		    Perl_croak(aTHX_
			       "Invalid range \"%c-%c\" in transliteration operator",
			       (char)min, (char)max);
                }

#ifdef EBCDIC
		if (literal_endpoint == 2 &&
		    ((isLOWER(min) && isLOWER(max)) ||
		     (isUPPER(min) && isUPPER(max)))) {
		    if (isLOWER(min)) {
			for (i = min; i <= max; i++)
			    if (isLOWER(i))
				*d++ = NATIVE_TO_NEED(has_utf8,i);
		    } else {
			for (i = min; i <= max; i++)
			    if (isUPPER(i))
				*d++ = NATIVE_TO_NEED(has_utf8,i);
		    }
		}
		else
#endif
		    for (i = min; i <= max; i++)
#ifdef EBCDIC
                        if (has_utf8) {
                            const U8 ch = (U8)NATIVE_TO_UTF(i);
                            if (UNI_IS_INVARIANT(ch))
                                *d++ = (U8)i;
                            else {
                                *d++ = (U8)UTF8_EIGHT_BIT_HI(ch);
                                *d++ = (U8)UTF8_EIGHT_BIT_LO(ch);
                            }
                        }
                        else
#endif
                            *d++ = (char)i;
 
#ifdef EBCDIC
                if (uvmax) {
                    d = (char*)uvchr_to_utf8((U8*)d, 0x100);
                    if (uvmax > 0x101)
                        *d++ = (char)UTF_TO_NATIVE(0xff);
                    if (uvmax > 0x100)
                        d = (char*)uvchr_to_utf8((U8*)d, uvmax);
                }
#endif

		/* mark the range as done, and continue */
		dorange = FALSE;
		didrange = TRUE;
#ifdef EBCDIC
		literal_endpoint = 0;
#endif
		continue;
	    }

	    /* range begins (ignore - as first or last char) */
	    else if (*s == '-' && s+1 < send  && s != start) {
		if (didrange) {
		    Perl_croak(aTHX_ "Ambiguous range in transliteration operator");
		}
		if (has_utf8
#ifdef EBCDIC
		    && !native_range
#endif
		    ) {
		    *d++ = (char)UTF_TO_NATIVE(0xff);	/* use illegal utf8 byte--see pmtrans */
		    s++;
		    continue;
		}
		dorange = TRUE;
		s++;
	    }
	    else {
		didrange = FALSE;
#ifdef EBCDIC
		literal_endpoint = 0;
		native_range = TRUE;
#endif
	    }
	}

	/* if we get here, we're not doing a transliteration */

	/* skip for regexp comments /(?#comment)/ and code /(?{code})/,
	   except for the last char, which will be done separately. */
	else if (*s == '(' && PL_lex_inpat && s[1] == '?') {
	    if (s[2] == '#') {
		while (s+1 < send && *s != ')')
		    *d++ = NATIVE_TO_NEED(has_utf8,*s++);
	    }
	    else if (s[2] == '{' /* This should match regcomp.c */
		    || (s[2] == '?' && s[3] == '{'))
	    {
		I32 count = 1;
		char *regparse = s + (s[2] == '{' ? 3 : 4);
		char c;

		while (count && (c = *regparse)) {
		    if (c == '\\' && regparse[1])
			regparse++;
		    else if (c == '{')
			count++;
		    else if (c == '}')
			count--;
		    regparse++;
		}
		if (*regparse != ')')
		    regparse--;		/* Leave one char for continuation. */
		while (s < regparse)
		    *d++ = NATIVE_TO_NEED(has_utf8,*s++);
	    }
	}

	/* likewise skip #-initiated comments in //x patterns */
	else if (*s == '#' && PL_lex_inpat &&
	  ((PMOP*)PL_lex_inpat)->op_pmflags & PMf_EXTENDED) {
	    while (s+1 < send && *s != '\n')
		*d++ = NATIVE_TO_NEED(has_utf8,*s++);
	}

	/* check for embedded arrays
	   (@foo, @::foo, @'foo, @{foo}, @$foo, @+, @-)
	   */
	else if (*s == '@' && s[1]) {
	    if (isALNUM_lazy_if(s+1,UTF))
		break;
	    if (strchr(":'{$", s[1]))
		break;
	    if (!PL_lex_inpat && (s[1] == '+' || s[1] == '-'))
		break; /* in regexp, neither @+ nor @- are interpolated */
	}

	/* check for embedded scalars.  only stop if we're sure it's a
	   variable.
        */
	else if (*s == '$') {
	    if (!PL_lex_inpat)	/* not a regexp, so $ must be var */
		break;
	    if (s + 1 < send && !strchr("()| \r\n\t", s[1])) {
		if (s[1] == '\\') {
		    Perl_ck_warner(aTHX_ packWARN(WARN_AMBIGUOUS),
				   "Possible unintended interpolation of $\\ in regex");
		}
		break;		/* in regexp, $ might be tail anchor */
            }
	}

	/* End of else if chain - OP_TRANS rejoin rest */

	/* backslashes */
	if (*s == '\\' && s+1 < send) {
	    char* e;	/* Can be used for ending '}', etc. */

	    s++;

	    /* deprecate \1 in strings and substitution replacements */
	    if (PL_lex_inwhat == OP_SUBST && !PL_lex_inpat &&
		isDIGIT(*s) && *s != '0' && !isDIGIT(s[1]))
	    {
		Perl_ck_warner(aTHX_ packWARN(WARN_SYNTAX), "\\%c better written as $%c", *s, *s);
		*--s = '$';
		break;
	    }

	    /* string-change backslash escapes */
	    if (PL_lex_inwhat != OP_TRANS && *s && strchr("lLuUEQ", *s)) {
		--s;
		break;
	    }
	    /* In a pattern, process \N, but skip any other backslash escapes.
	     * This is because we don't want to translate an escape sequence
	     * into a meta symbol and have the regex compiler use the meta
	     * symbol meaning, e.g. \x{2E} would be confused with a dot.  But
	     * in spite of this, we do have to process \N here while the proper
	     * charnames handler is in scope.  See bugs #56444 and #62056.
	     * There is a complication because \N in a pattern may also stand
	     * for 'match a non-nl', and not mean a charname, in which case its
	     * processing should be deferred to the regex compiler.  To be a
	     * charname it must be followed immediately by a '{', and not look
	     * like \N followed by a curly quantifier, i.e., not something like
	     * \N{3,}.  regcurly returns a boolean indicating if it is a legal
	     * quantifier */
	    else if (PL_lex_inpat
		    && (*s != 'N'
			|| s[1] != '{'
			|| regcurly(s + 1)))
	    {
		*d++ = NATIVE_TO_NEED(has_utf8,'\\');
		goto default_action;
	    }

	    switch (*s) {

	    /* quoted - in transliterations */
	    case '-':
		if (PL_lex_inwhat == OP_TRANS) {
		    *d++ = *s++;
		    continue;
		}
		/* FALL THROUGH */
	    default:
	        {
		    if ((isALPHA(*s) || isDIGIT(*s)))
			Perl_ck_warner(aTHX_ packWARN(WARN_MISC),
				       "Unrecognized escape \\%c passed through",
				       *s);
		    /* default action is to copy the quoted character */
		    goto default_action;
		}

	    /* eg. \132 indicates the octal constant 0x132 */
	    case '0': case '1': case '2': case '3':
	    case '4': case '5': case '6': case '7':
		{
                    I32 flags = 0;
                    STRLEN len = 3;
		    uv = NATIVE_TO_UNI(grok_oct(s, &len, &flags, NULL));
		    s += len;
		}
		goto NUM_ESCAPE_INSERT;

	    /* eg. \x24 indicates the hex constant 0x24 */
	    case 'x':
		++s;
		if (*s == '{') {
		    char* const e = strchr(s, '}');
                    I32 flags = PERL_SCAN_ALLOW_UNDERSCORES |
                      PERL_SCAN_DISALLOW_PREFIX;
		    STRLEN len;

                    ++s;
		    if (!e) {
			yyerror("Missing right brace on \\x{}");
			continue;
		    }
                    len = e - s;
		    uv = NATIVE_TO_UNI(grok_hex(s, &len, &flags, NULL));
		    s = e + 1;
		}
		else {
		    {
			STRLEN len = 2;
                        I32 flags = PERL_SCAN_DISALLOW_PREFIX;
			uv = NATIVE_TO_UNI(grok_hex(s, &len, &flags, NULL));
			s += len;
		    }
		}

	      NUM_ESCAPE_INSERT:
		/* Insert oct or hex escaped character.  There will always be
		 * enough room in sv since such escapes will be longer than any
		 * UTF-8 sequence they can end up as, except if they force us
		 * to recode the rest of the string into utf8 */
		
		/* Here uv is the ordinal of the next character being added in
		 * unicode (converted from native). */
		if (!UNI_IS_INVARIANT(uv)) {
		    if (!has_utf8 && uv > 255) {
			/* Might need to recode whatever we have accumulated so
			 * far if it contains any chars variant in utf8 or
			 * utf-ebcdic. */
			  
			SvCUR_set(sv, d - SvPVX_const(sv));
			SvPOK_on(sv);
			*d = '\0';
			/* See Note on sizing above.  */
			sv_utf8_upgrade_flags_grow(sv,
					SV_GMAGIC|SV_FORCE_UTF8_UPGRADE,
					UNISKIP(uv) + (STRLEN)(send - s) + 1);
			d = SvPVX(sv) + SvCUR(sv);
			has_utf8 = TRUE;
                    }

                    if (has_utf8) {
		        d = (char*)uvuni_to_utf8((U8*)d, uv);
			if (PL_lex_inwhat == OP_TRANS &&
			    PL_sublex_info.sub_op) {
			    PL_sublex_info.sub_op->op_private |=
				(PL_lex_repl ? OPpTRANS_FROM_UTF
					     : OPpTRANS_TO_UTF);
			}
#ifdef EBCDIC
			if (uv > 255 && !dorange)
			    native_range = FALSE;
#endif
                    }
		    else {
		        *d++ = (char)uv;
		    }
		}
		else {
		    *d++ = (char) uv;
		}
		continue;

 	    case 'N':
		/* In a non-pattern \N must be a named character, like \N{LATIN
		 * SMALL LETTER A} or \N{U+0041}.  For patterns, it also can
		 * mean to match a non-newline.  For non-patterns, named
		 * characters are converted to their string equivalents. In
		 * patterns, named characters are not converted to their
		 * ultimate forms for the same reasons that other escapes
		 * aren't.  Instead, they are converted to the \N{U+...} form
		 * to get the value from the charnames that is in effect right
		 * now, while preserving the fact that it was a named character
		 * so that the regex compiler knows this */

		/* This section of code doesn't generally use the
		 * NATIVE_TO_NEED() macro to transform the input.  I (khw) did
		 * a close examination of this macro and determined it is a
		 * no-op except on utfebcdic variant characters.  Every
		 * character generated by this that would normally need to be
		 * enclosed by this macro is invariant, so the macro is not
		 * needed, and would complicate use of copy(). There are other
		 * parts of this file where the macro is used inconsistently,
		 * but are saved by it being a no-op */

		/* The structure of this section of code (besides checking for
		 * errors and upgrading to utf8) is:
		 *  Further disambiguate between the two meanings of \N, and if
		 *	not a charname, go process it elsewhere
		 *  If of form \N{U+...}, pass it through if a pattern;
		 *	otherwise convert to utf8
		 *  Otherwise must be \N{NAME}: convert to \N{U+c1.c2...} if a
		 *  pattern; otherwise convert to utf8 */

		/* Here, s points to the 'N'; the test below is guaranteed to
		 * succeed if we are being called on a pattern as we already
		 * know from a test above that the next character is a '{'.
		 * On a non-pattern \N must mean 'named sequence, which
		 * requires braces */
		s++;
		if (*s != '{') {
		    yyerror("Missing braces on \\N{}"); 
		    continue;
		}
		s++;

		/* If there is no matching '}', it is an error. */
		if (! (e = strchr(s, '}'))) {
		    if (! PL_lex_inpat) {
			yyerror("Missing right brace on \\N{}");
		    } else {
			yyerror("Missing right brace on \\N{} or unescaped left brace after \\N.");
		    }
		    continue;
		}

		/* Here it looks like a named character */

		if (PL_lex_inpat) {

		    /* XXX This block is temporary code.  \N{} implies that the
		     * pattern is to have Unicode semantics, and therefore
		     * currently has to be encoded in utf8.  By putting it in
		     * utf8 now, we save a whole pass in the regular expression
		     * compiler.  Once that code is changed so Unicode
		     * semantics doesn't necessarily have to be in utf8, this
		     * block should be removed */
		    if (!has_utf8) {
			SvCUR_set(sv, d - SvPVX_const(sv));
			SvPOK_on(sv);
			*d = '\0';
			/* See Note on sizing above.  */
			sv_utf8_upgrade_flags_grow(sv,
					SV_GMAGIC|SV_FORCE_UTF8_UPGRADE,
					/* 5 = '\N{' + cur char + NUL */
					(STRLEN)(send - s) + 5);
			d = SvPVX(sv) + SvCUR(sv);
			has_utf8 = TRUE;
		    }
		}

		if (*s == 'U' && s[1] == '+') { /* \N{U+...} */
		    I32 flags = PERL_SCAN_ALLOW_UNDERSCORES
				| PERL_SCAN_DISALLOW_PREFIX;
		    STRLEN len;

		    /* For \N{U+...}, the '...' is a unicode value even on
		     * EBCDIC machines */
		    s += 2;	    /* Skip to next char after the 'U+' */
		    len = e - s;
		    uv = grok_hex(s, &len, &flags, NULL);
		    if (len == 0 || len != (STRLEN)(e - s)) {
			yyerror("Invalid hexadecimal number in \\N{U+...}");
			s = e + 1;
			continue;
		    }

		    if (PL_lex_inpat) {

			/* Pass through to the regex compiler unchanged.  The
			 * reason we evaluated the number above is to make sure
			 * there wasn't a syntax error. */
			s -= 5;	    /* Include the '\N{U+' */
			Copy(s, d, e - s + 1, char);	/* 1 = include the } */
			d += e - s + 1;
		    }
		    else {  /* Not a pattern: convert the hex to string */

			 /* If destination is not in utf8, unconditionally
			  * recode it to be so.  This is because \N{} implies
			  * Unicode semantics, and scalars have to be in utf8
			  * to guarantee those semantics */
			if (! has_utf8) {
			    SvCUR_set(sv, d - SvPVX_const(sv));
			    SvPOK_on(sv);
			    *d = '\0';
			    /* See Note on sizing above.  */
			    sv_utf8_upgrade_flags_grow(
					sv,
					SV_GMAGIC|SV_FORCE_UTF8_UPGRADE,
					UNISKIP(uv) + (STRLEN)(send - e) + 1);
			    d = SvPVX(sv) + SvCUR(sv);
			    has_utf8 = TRUE;
			}

			/* Add the string to the output */
			if (UNI_IS_INVARIANT(uv)) {
			    *d++ = (char) uv;
			}
			else d = (char*)uvuni_to_utf8((U8*)d, uv);
		    }
		}
		else { /* Here is \N{NAME} but not \N{U+...}. */

		    SV *res;		/* result from charnames */
		    const char *str;    /* the string in 'res' */
		    STRLEN len;		/* its length */

		    /* Get the value for NAME */
		    res = newSVpvn(s, e - s);
		    res = new_constant( NULL, 0, "charnames",
					/* includes all of: \N{...} */
					res, NULL, s - 3, e - s + 4 );

		    /* Most likely res will be in utf8 already since the
		     * standard charnames uses pack U, but a custom translator
		     * can leave it otherwise, so make sure.  XXX This can be
		     * revisited to not have charnames use utf8 for characters
		     * that don't need it when regexes don't have to be in utf8
		     * for Unicode semantics.  If doing so, remember EBCDIC */
		    sv_utf8_upgrade(res);
		    str = SvPV_const(res, len);

		    /* Don't accept malformed input */
		    if (! is_utf8_string((U8 *) str, len)) {
			yyerror("Malformed UTF-8 returned by \\N");
		    }
		    else if (PL_lex_inpat) {

			if (! len) { /* The name resolved to an empty string */
			    Copy("\\N{}", d, 4, char);
			    d += 4;
			}
			else {
			    /* In order to not lose information for the regex
			    * compiler, pass the result in the specially made
			    * syntax: \N{U+c1.c2.c3...}, where c1 etc. are
			    * the code points in hex of each character
			    * returned by charnames */

			    const char *str_end = str + len;
			    STRLEN char_length;	    /* cur char's byte length */
			    STRLEN output_length;   /* and the number of bytes
						       after this is translated
						       into hex digits */
			    const STRLEN off = d - SvPVX_const(sv);

			    /* 2 hex per byte; 2 chars for '\N'; 2 chars for
			     * max('U+', '.'); and 1 for NUL */
			    char hex_string[2 * UTF8_MAXBYTES + 5];

			    /* Get the first character of the result. */
			    U32 uv = utf8n_to_uvuni((U8 *) str,
						    len,
						    &char_length,
						    UTF8_ALLOW_ANYUV);

			    /* The call to is_utf8_string() above hopefully
			     * guarantees that there won't be an error.  But
			     * it's easy here to make sure.  The function just
			     * above warns and returns 0 if invalid utf8, but
			     * it can also return 0 if the input is validly a
			     * NUL. Disambiguate */
			    if (uv == 0 && NATIVE_TO_ASCII(*str) != '\0') {
				uv = UNICODE_REPLACEMENT;
			    }

			    /* Convert first code point to hex, including the
			     * boiler plate before it */
			    snprintf(hex_string, sizeof(hex_string),
				     "\\N{U+%X", (unsigned int) uv);
			    output_length = strlen(hex_string);

			    /* Make sure there is enough space to hold it */
			    d = off + SvGROW(sv, off
						 + output_length
						 + (STRLEN)(send - e)
						 + 2);	/* '}' + NUL */
			    /* And output it */
			    Copy(hex_string, d, output_length, char);
			    d += output_length;

			    /* For each subsequent character, append dot and
			     * its ordinal in hex */
			    while ((str += char_length) < str_end) {
				const STRLEN off = d - SvPVX_const(sv);
				U32 uv = utf8n_to_uvuni((U8 *) str,
							str_end - str,
							&char_length,
							UTF8_ALLOW_ANYUV);
				if (uv == 0 && NATIVE_TO_ASCII(*str) != '\0') {
				    uv = UNICODE_REPLACEMENT;
				}

				snprintf(hex_string, sizeof(hex_string),
					".%X", (unsigned int) uv);
				output_length = strlen(hex_string);

				d = off + SvGROW(sv, off
						     + output_length
						     + (STRLEN)(send - e)
						     + 2);	/* '}' +  NUL */
				Copy(hex_string, d, output_length, char);
				d += output_length;
			    }

			    *d++ = '}';	/* Done.  Add the trailing brace */
			}
		    }
		    else { /* Here, not in a pattern.  Convert the name to a
			    * string. */

			 /* If destination is not in utf8, unconditionally
			  * recode it to be so.  This is because \N{} implies
			  * Unicode semantics, and scalars have to be in utf8
			  * to guarantee those semantics */
			if (! has_utf8) {
			    SvCUR_set(sv, d - SvPVX_const(sv));
			    SvPOK_on(sv);
			    *d = '\0';
			    /* See Note on sizing above.  */
			    sv_utf8_upgrade_flags_grow(sv,
						SV_GMAGIC|SV_FORCE_UTF8_UPGRADE,
						len + (STRLEN)(send - s) + 1);
			    d = SvPVX(sv) + SvCUR(sv);
			    has_utf8 = TRUE;
			} else if (len > (STRLEN)(e - s + 4)) { /* I _guess_ 4 is \N{} --jhi */

			    /* See Note on sizing above.  (NOTE: SvCUR() is not
			     * set correctly here). */
			    const STRLEN off = d - SvPVX_const(sv);
			    d = off + SvGROW(sv, off + len + (STRLEN)(send - s) + 1);
			}
			Copy(str, d, len, char);
			d += len;
		    }
		    SvREFCNT_dec(res);

		    /* Deprecate non-approved name syntax */
		    if (ckWARN_d(WARN_DEPRECATED)) {
			bool problematic = FALSE;
			char* i = s;

			/* For non-ut8 input, look to see that the first
			 * character is an alpha, then loop through the rest
			 * checking that each is a continuation */
			if (! this_utf8) {
			    if (! isALPHAU(*i)) problematic = TRUE;
			    else for (i = s + 1; i < e; i++) {
				if (isCHARNAME_CONT(*i)) continue;
				problematic = TRUE;
				break;
			    }
			}
			else {
			    /* Similarly for utf8.  For invariants can check
			     * directly.  We accept anything above the latin1
			     * range because it is immaterial to Perl if it is
			     * correct or not, and is expensive to check.  But
			     * it is fairly easy in the latin1 range to convert
			     * the variants into a single character and check
			     * those */
			    if (UTF8_IS_INVARIANT(*i)) {
				if (! isALPHAU(*i)) problematic = TRUE;
			    } else if (UTF8_IS_DOWNGRADEABLE_START(*i)) {
				if (! isALPHAU(UNI_TO_NATIVE(UTF8_ACCUMULATE(*i,
									    *(i+1)))))
				{
				    problematic = TRUE;
				}
			    }
			    if (! problematic) for (i = s + UTF8SKIP(s);
						    i < e;
						    i+= UTF8SKIP(i))
			    {
				if (UTF8_IS_INVARIANT(*i)) {
				    if (isCHARNAME_CONT(*i)) continue;
				} else if (! UTF8_IS_DOWNGRADEABLE_START(*i)) {
				    continue;
				} else if (isCHARNAME_CONT(
					    UNI_TO_NATIVE(
					    UTF8_ACCUMULATE(*i, *(i+1)))))
				{
				    continue;
				}
				problematic = TRUE;
				break;
			    }
			}
			if (problematic) {
			    char *string;
			    Newx(string, e - i + 1, char);
			    Copy(i, string, e - i, char);
			    string[e - i] = '\0';
			    Perl_warner(aTHX_ packWARN(WARN_DEPRECATED),
				"Deprecated character(s) in \\N{...} starting at '%s'",
				string);
			    Safefree(string);
			}
		    }
		} /* End \N{NAME} */
#ifdef EBCDIC
		if (!dorange) 
		    native_range = FALSE; /* \N{} is defined to be Unicode */
#endif
		s = e + 1;  /* Point to just after the '}' */
		continue;

	    /* \c is a control character */
	    case 'c':
		s++;
		if (s < send) {
		    U8 c = *s++;
#ifdef EBCDIC
		    if (isLOWER(c))
			c = toUPPER(c);
#endif
		    *d++ = NATIVE_TO_NEED(has_utf8,toCTRL(c));
		}
		else {
		    yyerror("Missing control char name in \\c");
		}
		continue;

	    /* printf-style backslashes, formfeeds, newlines, etc */
	    case 'b':
		*d++ = NATIVE_TO_NEED(has_utf8,'\b');
		break;
	    case 'n':
		*d++ = NATIVE_TO_NEED(has_utf8,'\n');
		break;
	    case 'r':
		*d++ = NATIVE_TO_NEED(has_utf8,'\r');
		break;
	    case 'f':
		*d++ = NATIVE_TO_NEED(has_utf8,'\f');
		break;
	    case 't':
		*d++ = NATIVE_TO_NEED(has_utf8,'\t');
		break;
	    case 'e':
		*d++ = ASCII_TO_NEED(has_utf8,'\033');
		break;
	    case 'a':
		*d++ = ASCII_TO_NEED(has_utf8,'\007');
		break;
	    } /* end switch */

	    s++;
	    continue;
	} /* end if (backslash) */
#ifdef EBCDIC
	else
	    literal_endpoint++;
#endif

    default_action:
	/* If we started with encoded form, or already know we want it,
	   then encode the next character */
	if (! NATIVE_IS_INVARIANT((U8)(*s)) && (this_utf8 || has_utf8)) {
	    STRLEN len  = 1;


	    /* One might think that it is wasted effort in the case of the
	     * source being utf8 (this_utf8 == TRUE) to take the next character
	     * in the source, convert it to an unsigned value, and then convert
	     * it back again.  But the source has not been validated here.  The
	     * routine that does the conversion checks for errors like
	     * malformed utf8 */

	    const UV nextuv   = (this_utf8) ? utf8n_to_uvchr((U8*)s, send - s, &len, 0) : (UV) ((U8) *s);
	    const STRLEN need = UNISKIP(NATIVE_TO_UNI(nextuv));
	    if (!has_utf8) {
		SvCUR_set(sv, d - SvPVX_const(sv));
		SvPOK_on(sv);
		*d = '\0';
		/* See Note on sizing above.  */
		sv_utf8_upgrade_flags_grow(sv,
					SV_GMAGIC|SV_FORCE_UTF8_UPGRADE,
					need + (STRLEN)(send - s) + 1);
		d = SvPVX(sv) + SvCUR(sv);
		has_utf8 = TRUE;
	    } else if (need > len) {
		/* encoded value larger than old, may need extra space (NOTE:
		 * SvCUR() is not set correctly here).   See Note on sizing
		 * above.  */
		const STRLEN off = d - SvPVX_const(sv);
		d = SvGROW(sv, off + need + (STRLEN)(send - s) + 1) + off;
	    }
	    s += len;

	    d = (char*)uvchr_to_utf8((U8*)d, nextuv);
#ifdef EBCDIC
	    if (uv > 255 && !dorange)
		native_range = FALSE;
#endif
	}
	else {
	    *d++ = NATIVE_TO_NEED(has_utf8,*s++);
	}
    } /* while loop to process each character */

    /* terminate the string and set up the sv */
    *d = '\0';
    SvCUR_set(sv, d - SvPVX_const(sv));
    if (SvCUR(sv) >= SvLEN(sv))
	Perl_croak(aTHX_ "panic: constant overflowed allocated space");

    SvPOK_on(sv);
    if (PL_encoding && !has_utf8) {
	sv_recode_to_utf8(sv, PL_encoding);
	if (SvUTF8(sv))
	    has_utf8 = TRUE;
    }
    if (has_utf8) {
	SvUTF8_on(sv);
	if (PL_lex_inwhat == OP_TRANS && PL_sublex_info.sub_op) {
	    PL_sublex_info.sub_op->op_private |=
		    (PL_lex_repl ? OPpTRANS_FROM_UTF : OPpTRANS_TO_UTF);
	}
    }

    /* shrink the sv if we allocated more than we used */
    if (SvCUR(sv) + 5 < SvLEN(sv)) {
	SvPV_shrink_to_cur(sv);
    }

    /* return the substring (via pl_yylval) only if we parsed anything */
    if (s > PL_bufptr) {
	if ( PL_hints & ( PL_lex_inpat ? HINT_NEW_RE : HINT_NEW_STRING ) ) {
	    const char *const key = PL_lex_inpat ? "qr" : "q";
	    const STRLEN keylen = PL_lex_inpat ? 2 : 1;
	    const char *type;
	    STRLEN typelen;

	    if (PL_lex_inwhat == OP_TRANS) {
		type = "tr";
		typelen = 2;
	    } else if (PL_lex_inwhat == OP_SUBST && !PL_lex_inpat) {
		type = "s";
		typelen = 1;
	    } else  {
		type = "qq";
		typelen = 2;
	    }

	    sv = S_new_constant(aTHX_ start, s - start, key, keylen, sv, NULL,
				type, typelen);
	}
	pl_yylval.opval = (OP*)newSVOP(OP_CONST, 0, sv);
    } else
	SvREFCNT_dec(sv);
    return s;
}

/* S_intuit_more
 * Returns TRUE if there's more to the expression (e.g., a subscript),
 * FALSE otherwise.
 *
 * It deals with "$foo[3]" and /$foo[3]/ and /$foo[0123456789$]+/
 *
 * ->[ and ->{ return TRUE
 * { and [ outside a pattern are always subscripts, so return TRUE
 * if we're outside a pattern and it's not { or [, then return FALSE
 * if we're in a pattern and the first char is a {
 *   {4,5} (any digits around the comma) returns FALSE
 * if we're in a pattern and the first char is a [
 *   [] returns FALSE
 *   [SOMETHING] has a funky algorithm to decide whether it's a
 *      character class or not.  It has to deal with things like
 *      /$foo[-3]/ and /$foo[$bar]/ as well as /$foo[$\d]+/
 * anything else returns TRUE
 */

/* This is the one truly awful dwimmer necessary to conflate C and sed. */

STATIC int
S_intuit_more(pTHX_ register char *s)
{
    dVAR;

    PERL_ARGS_ASSERT_INTUIT_MORE;

    if (PL_lex_brackets)
	return TRUE;
    if (*s == '-' && s[1] == '>' && (s[2] == '[' || s[2] == '{'))
	return TRUE;
    if (*s != '{' && *s != '[')
	return FALSE;
    if (!PL_lex_inpat)
	return TRUE;

    /* In a pattern, so maybe we have {n,m}. */
    if (*s == '{') {
	s++;
	if (!isDIGIT(*s))
	    return TRUE;
	while (isDIGIT(*s))
	    s++;
	if (*s == ',')
	    s++;
	while (isDIGIT(*s))
	    s++;
	if (*s == '}')
	    return FALSE;
	return TRUE;
	
    }

    /* On the other hand, maybe we have a character class */

    s++;
    if (*s == ']' || *s == '^')
	return FALSE;
    else {
        /* this is terrifying, and it works */
	int weight = 2;		/* let's weigh the evidence */
	char seen[256];
	unsigned char un_char = 255, last_un_char;
	const char * const send = strchr(s,']');
	char tmpbuf[sizeof PL_tokenbuf * 4];

	if (!send)		/* has to be an expression */
	    return TRUE;

	Zero(seen,256,char);
	if (*s == '$')
	    weight -= 3;
	else if (isDIGIT(*s)) {
	    if (s[1] != ']') {
		if (isDIGIT(s[1]) && s[2] == ']')
		    weight -= 10;
	    }
	    else
		weight -= 100;
	}
	for (; s < send; s++) {
	    last_un_char = un_char;
	    un_char = (unsigned char)*s;
	    switch (*s) {
	    case '@':
	    case '&':
	    case '$':
		weight -= seen[un_char] * 10;
		if (isALNUM_lazy_if(s+1,UTF)) {
		    int len;
		    scan_ident(s, send, tmpbuf, sizeof tmpbuf, FALSE);
		    len = (int)strlen(tmpbuf);
		    if (len > 1 && gv_fetchpvn_flags(tmpbuf, len, 0, SVt_PV))
			weight -= 100;
		    else
			weight -= 10;
		}
		else if (*s == '$' && s[1] &&
		  strchr("[#!%*<>()-=",s[1])) {
		    if (/*{*/ strchr("])} =",s[2]))
			weight -= 10;
		    else
			weight -= 1;
		}
		break;
	    case '\\':
		un_char = 254;
		if (s[1]) {
		    if (strchr("wds]",s[1]))
			weight += 100;
		    else if (seen[(U8)'\''] || seen[(U8)'"'])
			weight += 1;
		    else if (strchr("rnftbxcav",s[1]))
			weight += 40;
		    else if (isDIGIT(s[1])) {
			weight += 40;
			while (s[1] && isDIGIT(s[1]))
			    s++;
		    }
		}
		else
		    weight += 100;
		break;
	    case '-':
		if (s[1] == '\\')
		    weight += 50;
		if (strchr("aA01! ",last_un_char))
		    weight += 30;
		if (strchr("zZ79~",s[1]))
		    weight += 30;
		if (last_un_char == 255 && (isDIGIT(s[1]) || s[1] == '$'))
		    weight -= 5;	/* cope with negative subscript */
		break;
	    default:
		if (!isALNUM(last_un_char)
		    && !(last_un_char == '$' || last_un_char == '@'
			 || last_un_char == '&')
		    && isALPHA(*s) && s[1] && isALPHA(s[1])) {
		    char *d = tmpbuf;
		    while (isALPHA(*s))
			*d++ = *s++;
		    *d = '\0';
		    if (keyword(tmpbuf, d - tmpbuf, 0))
			weight -= 150;
		}
		if (un_char == last_un_char + 1)
		    weight += 5;
		weight -= seen[un_char];
		break;
	    }
	    seen[un_char]++;
	}
	if (weight >= 0)	/* probably a character class */
	    return FALSE;
    }

    return TRUE;
}

/*
 * S_intuit_method
 *
 * Does all the checking to disambiguate
 *   foo bar
 * between foo(bar) and bar->foo.  Returns 0 if not a method, otherwise
 * FUNCMETH (bar->foo(args)) or METHOD (bar->foo args).
 *
 * First argument is the stuff after the first token, e.g. "bar".
 *
 * Not a method if bar is a filehandle.
 * Not a method if foo is a subroutine prototyped to take a filehandle.
 * Not a method if it's really "Foo $bar"
 * Method if it's "foo $bar"
 * Not a method if it's really "print foo $bar"
 * Method if it's really "foo package::" (interpreted as package->foo)
 * Not a method if bar is known to be a subroutine ("sub bar; foo bar")
 * Not a method if bar is a filehandle or package, but is quoted with
 *   =>
 */

STATIC int
S_intuit_method(pTHX_ char *start, GV *gv, CV *cv)
{
    dVAR;
    char *s = start + (*start == '$');
    char tmpbuf[sizeof PL_tokenbuf];
    STRLEN len;
    GV* indirgv;
#ifdef PERL_MAD
    int soff;
#endif

    PERL_ARGS_ASSERT_INTUIT_METHOD;

    if (gv) {
	if (SvTYPE(gv) == SVt_PVGV && GvIO(gv))
	    return 0;
	if (cv) {
	    if (SvPOK(cv)) {
		const char *proto = SvPVX_const(cv);
		if (proto) {
		    if (*proto == ';')
			proto++;
		    if (*proto == '*')
			return 0;
		}
	    }
	} else
	    gv = NULL;
    }
    s = scan_word(s, tmpbuf, sizeof tmpbuf, TRUE, &len);
    /* start is the beginning of the possible filehandle/object,
     * and s is the end of it
     * tmpbuf is a copy of it
     */

    if (*start == '$') {
	if (gv || PL_last_lop_op == OP_PRINT || PL_last_lop_op == OP_SAY ||
		isUPPER(*PL_tokenbuf))
	    return 0;
#ifdef PERL_MAD
	len = start - SvPVX(PL_linestr);
#endif
	s = PEEKSPACE(s);
#ifdef PERL_MAD
	start = SvPVX(PL_linestr) + len;
#endif
	PL_bufptr = start;
	PL_expect = XREF;
	return *s == '(' ? FUNCMETH : METHOD;
    }
    if (!keyword(tmpbuf, len, 0)) {
	if (len > 2 && tmpbuf[len - 2] == ':' && tmpbuf[len - 1] == ':') {
	    len -= 2;
	    tmpbuf[len] = '\0';
#ifdef PERL_MAD
	    soff = s - SvPVX(PL_linestr);
#endif
	    goto bare_package;
	}
	indirgv = gv_fetchpvn_flags(tmpbuf, len, 0, SVt_PVCV);
	if (indirgv && GvCVu(indirgv))
	    return 0;
	/* filehandle or package name makes it a method */
	if (!gv || GvIO(indirgv) || gv_stashpvn(tmpbuf, len, 0)) {
#ifdef PERL_MAD
	    soff = s - SvPVX(PL_linestr);
#endif
	    s = PEEKSPACE(s);
	    if ((PL_bufend - s) >= 2 && *s == '=' && *(s+1) == '>')
		return 0;	/* no assumptions -- "=>" quotes bearword */
      bare_package:
	    start_force(PL_curforce);
	    NEXTVAL_NEXTTOKE.opval = (OP*)newSVOP(OP_CONST, 0,
						  S_newSV_maybe_utf8(aTHX_ tmpbuf, len));
	    NEXTVAL_NEXTTOKE.opval->op_private = OPpCONST_BARE;
	    if (PL_madskills)
		curmad('X', newSVpvn(start,SvPVX(PL_linestr) + soff - start));
	    PL_expect = XTERM;
	    force_next(WORD);
	    PL_bufptr = s;
#ifdef PERL_MAD
	    PL_bufptr = SvPVX(PL_linestr) + soff; /* restart before space */
#endif
	    return *s == '(' ? FUNCMETH : METHOD;
	}
    }
    return 0;
}

/* Encoded script support. filter_add() effectively inserts a
 * 'pre-processing' function into the current source input stream.
 * Note that the filter function only applies to the current source file
 * (e.g., it will not affect files 'require'd or 'use'd by this one).
 *
 * The datasv parameter (which may be NULL) can be used to pass
 * private data to this instance of the filter. The filter function
 * can recover the SV using the FILTER_DATA macro and use it to
 * store private buffers and state information.
 *
 * The supplied datasv parameter is upgraded to a PVIO type
 * and the IoDIRP/IoANY field is used to store the function pointer,
 * and IOf_FAKE_DIRP is enabled on datasv to mark this as such.
 * Note that IoTOP_NAME, IoFMT_NAME, IoBOTTOM_NAME, if set for
 * private use must be set using malloc'd pointers.
 */

SV *
Perl_filter_add(pTHX_ filter_t funcp, SV *datasv)
{
    dVAR;
    if (!funcp)
	return NULL;

    if (!PL_parser)
	return NULL;

    if (!PL_rsfp_filters)
	PL_rsfp_filters = newAV();
    if (!datasv)
	datasv = newSV(0);
    SvUPGRADE(datasv, SVt_PVIO);
    IoANY(datasv) = FPTR2DPTR(void *, funcp); /* stash funcp into spare field */
    IoFLAGS(datasv) |= IOf_FAKE_DIRP;
    DEBUG_P(PerlIO_printf(Perl_debug_log, "filter_add func %p (%s)\n",
			  FPTR2DPTR(void *, IoANY(datasv)),
			  SvPV_nolen(datasv)));
    av_unshift(PL_rsfp_filters, 1);
    av_store(PL_rsfp_filters, 0, datasv) ;
    return(datasv);
}


/* Delete most recently added instance of this filter function.	*/
void
Perl_filter_del(pTHX_ filter_t funcp)
{
    dVAR;
    SV *datasv;

    PERL_ARGS_ASSERT_FILTER_DEL;

#ifdef DEBUGGING
    DEBUG_P(PerlIO_printf(Perl_debug_log, "filter_del func %p",
			  FPTR2DPTR(void*, funcp)));
#endif
    if (!PL_parser || !PL_rsfp_filters || AvFILLp(PL_rsfp_filters)<0)
	return;
    /* if filter is on top of stack (usual case) just pop it off */
    datasv = FILTER_DATA(AvFILLp(PL_rsfp_filters));
    if (IoANY(datasv) == FPTR2DPTR(void *, funcp)) {
	IoFLAGS(datasv) &= ~IOf_FAKE_DIRP;
	IoANY(datasv) = (void *)NULL;
	sv_free(av_pop(PL_rsfp_filters));

        return;
    }
    /* we need to search for the correct entry and clear it	*/
    Perl_die(aTHX_ "filter_del can only delete in reverse order (currently)");
}


/* Invoke the idxth filter function for the current rsfp.	 */
/* maxlen 0 = read one text line */
I32
Perl_filter_read(pTHX_ int idx, SV *buf_sv, int maxlen)
{
    dVAR;
    filter_t funcp;
    SV *datasv = NULL;
    /* This API is bad. It should have been using unsigned int for maxlen.
       Not sure if we want to change the API, but if not we should sanity
       check the value here.  */
    const unsigned int correct_length
	= maxlen < 0 ?
#ifdef PERL_MICRO
	0x7FFFFFFF
#else
	INT_MAX
#endif
	: maxlen;

    PERL_ARGS_ASSERT_FILTER_READ;

    if (!PL_parser || !PL_rsfp_filters)
	return -1;
    if (idx > AvFILLp(PL_rsfp_filters)) {       /* Any more filters?	*/
	/* Provide a default input filter to make life easy.	*/
	/* Note that we append to the line. This is handy.	*/
	DEBUG_P(PerlIO_printf(Perl_debug_log,
			      "filter_read %d: from rsfp\n", idx));
	if (correct_length) {
 	    /* Want a block */
	    int len ;
	    const int old_len = SvCUR(buf_sv);

	    /* ensure buf_sv is large enough */
	    SvGROW(buf_sv, (STRLEN)(old_len + correct_length + 1)) ;
	    if ((len = PerlIO_read(PL_rsfp, SvPVX(buf_sv) + old_len,
				   correct_length)) <= 0) {
		if (PerlIO_error(PL_rsfp))
	            return -1;		/* error */
	        else
		    return 0 ;		/* end of file */
	    }
	    SvCUR_set(buf_sv, old_len + len) ;
	    SvPVX(buf_sv)[old_len + len] = '\0';
	} else {
	    /* Want a line */
            if (sv_gets(buf_sv, PL_rsfp, SvCUR(buf_sv)) == NULL) {
		if (PerlIO_error(PL_rsfp))
	            return -1;		/* error */
	        else
		    return 0 ;		/* end of file */
	    }
	}
	return SvCUR(buf_sv);
    }
    /* Skip this filter slot if filter has been deleted	*/
    if ( (datasv = FILTER_DATA(idx)) == &PL_sv_undef) {
	DEBUG_P(PerlIO_printf(Perl_debug_log,
			      "filter_read %d: skipped (filter deleted)\n",
			      idx));
	return FILTER_READ(idx+1, buf_sv, correct_length); /* recurse */
    }
    /* Get function pointer hidden within datasv	*/
    funcp = DPTR2FPTR(filter_t, IoANY(datasv));
    DEBUG_P(PerlIO_printf(Perl_debug_log,
			  "filter_read %d: via function %p (%s)\n",
			  idx, (void*)datasv, SvPV_nolen_const(datasv)));
    /* Call function. The function is expected to 	*/
    /* call "FILTER_READ(idx+1, buf_sv)" first.		*/
    /* Return: <0:error, =0:eof, >0:not eof 		*/
    return (*funcp)(aTHX_ idx, buf_sv, correct_length);
}

STATIC char *
S_filter_gets(pTHX_ register SV *sv, STRLEN append)
{
    dVAR;

    PERL_ARGS_ASSERT_FILTER_GETS;

#ifdef PERL_CR_FILTER
    if (!PL_rsfp_filters) {
	filter_add(S_cr_textfilter,NULL);
    }
#endif
    if (PL_rsfp_filters) {
	if (!append)
            SvCUR_set(sv, 0);	/* start with empty line	*/
        if (FILTER_READ(0, sv, 0) > 0)
            return ( SvPVX(sv) ) ;
        else
	    return NULL ;
    }
    else
        return (sv_gets(sv, PL_rsfp, append));
}

STATIC HV *
S_find_in_my_stash(pTHX_ const char *pkgname, STRLEN len)
{
    dVAR;
    GV *gv;

    PERL_ARGS_ASSERT_FIND_IN_MY_STASH;

    if (len == 11 && *pkgname == '_' && strEQ(pkgname, "__PACKAGE__"))
        return PL_curstash;

    if (len > 2 &&
        (pkgname[len - 2] == ':' && pkgname[len - 1] == ':') &&
        (gv = gv_fetchpvn_flags(pkgname, len, 0, SVt_PVHV)))
    {
        return GvHV(gv);			/* Foo:: */
    }

    /* use constant CLASS => 'MyClass' */
    gv = gv_fetchpvn_flags(pkgname, len, 0, SVt_PVCV);
    if (gv && GvCV(gv)) {
	SV * const sv = cv_const_sv(GvCV(gv));
	if (sv)
            pkgname = SvPV_const(sv, len);
    }

    return gv_stashpvn(pkgname, len, 0);
}

/*
 * S_readpipe_override
 * Check whether readpipe() is overriden, and generates the appropriate
 * optree, provided sublex_start() is called afterwards.
 */
STATIC void
S_readpipe_override(pTHX)
{
    GV **gvp;
    GV *gv_readpipe = gv_fetchpvs("readpipe", GV_NOTQUAL, SVt_PVCV);
    pl_yylval.ival = OP_BACKTICK;
    if ((gv_readpipe
		&& GvCVu(gv_readpipe) && GvIMPORTED_CV(gv_readpipe))
	    ||
	    ((gvp = (GV**)hv_fetchs(PL_globalstash, "readpipe", FALSE))
	     && (gv_readpipe = *gvp) && isGV_with_GP(gv_readpipe)
	     && GvCVu(gv_readpipe) && GvIMPORTED_CV(gv_readpipe)))
    {
	PL_lex_op = (OP*)newUNOP(OP_ENTERSUB, OPf_STACKED,
	    append_elem(OP_LIST,
		newSVOP(OP_CONST, 0, &PL_sv_undef), /* value will be read later */
		newCVREF(0, newGVOP(OP_GV, 0, gv_readpipe))));
    }
}

#ifdef PERL_MAD 
 /*
 * Perl_madlex
 * The intent of this yylex wrapper is to minimize the changes to the
 * tokener when we aren't interested in collecting madprops.  It remains
 * to be seen how successful this strategy will be...
 */

int
Perl_madlex(pTHX)
{
    int optype;
    char *s = PL_bufptr;

    /* make sure PL_thiswhite is initialized */
    PL_thiswhite = 0;
    PL_thismad = 0;

    /* just do what yylex would do on pending identifier; leave PL_thiswhite alone */
    if (PL_pending_ident)
        return S_pending_ident(aTHX);

    /* previous token ate up our whitespace? */
    if (!PL_lasttoke && PL_nextwhite) {
	PL_thiswhite = PL_nextwhite;
	PL_nextwhite = 0;
    }

    /* isolate the token, and figure out where it is without whitespace */
    PL_realtokenstart = -1;
    PL_thistoken = 0;
    optype = yylex();
    s = PL_bufptr;
    assert(PL_curforce < 0);

    if (!PL_thismad || PL_thismad->mad_key == '^') {	/* not forced already? */
	if (!PL_thistoken) {
	    if (PL_realtokenstart < 0 || !CopLINE(PL_curcop))
		PL_thistoken = newSVpvs("");
	    else {
		char * const tstart = SvPVX(PL_linestr) + PL_realtokenstart;
		PL_thistoken = newSVpvn(tstart, s - tstart);
	    }
	}
	if (PL_thismad)	/* install head */
	    CURMAD('X', PL_thistoken);
    }

    /* last whitespace of a sublex? */
    if (optype == ')' && PL_endwhite) {
	CURMAD('X', PL_endwhite);
    }

    if (!PL_thismad) {

	/* if no whitespace and we're at EOF, bail.  Otherwise fake EOF below. */
	if (!PL_thiswhite && !PL_endwhite && !optype) {
	    sv_free(PL_thistoken);
	    PL_thistoken = 0;
	    return 0;
	}

	/* put off final whitespace till peg */
	if (optype == ';' && !PL_rsfp) {
	    PL_nextwhite = PL_thiswhite;
	    PL_thiswhite = 0;
	}
	else if (PL_thisopen) {
	    CURMAD('q', PL_thisopen);
	    if (PL_thistoken)
		sv_free(PL_thistoken);
	    PL_thistoken = 0;
	}
	else {
	    /* Store actual token text as madprop X */
	    CURMAD('X', PL_thistoken);
	}

	if (PL_thiswhite) {
	    /* add preceding whitespace as madprop _ */
	    CURMAD('_', PL_thiswhite);
	}

	if (PL_thisstuff) {
	    /* add quoted material as madprop = */
	    CURMAD('=', PL_thisstuff);
	}

	if (PL_thisclose) {
	    /* add terminating quote as madprop Q */
	    CURMAD('Q', PL_thisclose);
	}
    }

    /* special processing based on optype */

    switch (optype) {

    /* opval doesn't need a TOKEN since it can already store mp */
    case WORD:
    case METHOD:
    case FUNCMETH:
    case THING:
    case PMFUNC:
    case PRIVATEREF:
    case FUNC0SUB:
    case UNIOPSUB:
    case LSTOPSUB:
	if (pl_yylval.opval)
	    append_madprops(PL_thismad, pl_yylval.opval, 0);
	PL_thismad = 0;
	return optype;

    /* fake EOF */
    case 0:
	optype = PEG;
	if (PL_endwhite) {
	    addmad(newMADsv('p', PL_endwhite), &PL_thismad, 0);
	    PL_endwhite = 0;
	}
	break;

    case ']':
    case '}':
	if (PL_faketokens)
	    break;
	/* remember any fake bracket that lexer is about to discard */ 
	if (PL_lex_brackets == 1 &&
	    ((expectation)PL_lex_brackstack[0] & XFAKEBRACK))
	{
	    s = PL_bufptr;
	    while (s < PL_bufend && (*s == ' ' || *s == '\t'))
		s++;
	    if (*s == '}') {
		PL_thiswhite = newSVpvn(PL_bufptr, ++s - PL_bufptr);
		addmad(newMADsv('#', PL_thiswhite), &PL_thismad, 0);
		PL_thiswhite = 0;
		PL_bufptr = s - 1;
		break;	/* don't bother looking for trailing comment */
	    }
	    else
		s = PL_bufptr;
	}
	if (optype == ']')
	    break;
	/* FALLTHROUGH */

    /* attach a trailing comment to its statement instead of next token */
    case ';':
	if (PL_faketokens)
	    break;
	if (PL_bufptr > PL_oldbufptr && PL_bufptr[-1] == optype) {
	    s = PL_bufptr;
	    while (s < PL_bufend && (*s == ' ' || *s == '\t'))
		s++;
	    if (*s == '\n' || *s == '#') {
		while (s < PL_bufend && *s != '\n')
		    s++;
		if (s < PL_bufend)
		    s++;
		PL_thiswhite = newSVpvn(PL_bufptr, s - PL_bufptr);
		addmad(newMADsv('#', PL_thiswhite), &PL_thismad, 0);
		PL_thiswhite = 0;
		PL_bufptr = s;
	    }
	}
	break;

    /* pval */
    case LABEL:
	break;

    /* ival */
    default:
	break;

    }

    /* Create new token struct.  Note: opvals return early above. */
    pl_yylval.tkval = newTOKEN(optype, pl_yylval, PL_thismad);
    PL_thismad = 0;
    return optype;
}
#endif

STATIC char *
S_tokenize_use(pTHX_ int is_use, char *s) {
    dVAR;

    PERL_ARGS_ASSERT_TOKENIZE_USE;

    if (PL_expect != XSTATE)
	yyerror(Perl_form(aTHX_ "\"%s\" not allowed in expression",
		    is_use ? "use" : "no"));
    s = SKIPSPACE1(s);
    if (isDIGIT(*s) || (*s == 'v' && isDIGIT(s[1]))) {
	s = force_version(s, TRUE);
	if (*s == ';' || *s == '}'
		|| (s = SKIPSPACE1(s), (*s == ';' || *s == '}'))) {
	    start_force(PL_curforce);
	    NEXTVAL_NEXTTOKE.opval = NULL;
	    force_next(WORD);
	}
	else if (*s == 'v') {
	    s = force_word(s,WORD,FALSE,TRUE,FALSE);
	    s = force_version(s, FALSE);
	}
    }
    else {
	s = force_word(s,WORD,FALSE,TRUE,FALSE);
	s = force_version(s, FALSE);
    }
    pl_yylval.ival = is_use;
    return s;
}
#ifdef DEBUGGING
    static const char* const exp_name[] =
	{ "OPERATOR", "TERM", "REF", "STATE", "BLOCK", "ATTRBLOCK",
	  "ATTRTERM", "TERMBLOCK", "TERMORDORDOR"
	};
#endif

/*
  yylex

  Works out what to call the token just pulled out of the input
  stream.  The yacc parser takes care of taking the ops we return and
  stitching them into a tree.

  Returns:
    PRIVATEREF

  Structure:
      if read an identifier
          if we're in a my declaration
	      croak if they tried to say my($foo::bar)
	      build the ops for a my() declaration
	  if it's an access to a my() variable
	      are we in a sort block?
	          croak if my($a); $a <=> $b
	      build ops for access to a my() variable
	  if in a dq string, and they've said @foo and we can't find @foo
	      croak
	  build ops for a bareword
      if we already built the token before, use it.
*/


#ifdef __SC__
#pragma segment Perl_yylex
#endif
int
Perl_yylex(pTHX)
{
    dVAR;
    register char *s = PL_bufptr;
    register char *d;
    STRLEN len;
    bool bof = FALSE;
    U32 fake_eof = 0;

    /* orig_keyword, gvp, and gv are initialized here because
     * jump to the label just_a_word_zero can bypass their
     * initialization later. */
    I32 orig_keyword = 0;
    GV *gv = NULL;
    GV **gvp = NULL;

    DEBUG_T( {
	SV* tmp = newSVpvs("");
	PerlIO_printf(Perl_debug_log, "### %"IVdf":LEX_%s/X%s %s\n",
	    (IV)CopLINE(PL_curcop),
	    lex_state_names[PL_lex_state],
	    exp_name[PL_expect],
	    pv_display(tmp, s, strlen(s), 0, 60));
	SvREFCNT_dec(tmp);
    } );
    /* check if there's an identifier for us to look at */
    if (PL_pending_ident)
        return REPORT(S_pending_ident(aTHX));

    /* no identifier pending identification */

    switch (PL_lex_state) {
#ifdef COMMENTARY
    case LEX_NORMAL:		/* Some compilers will produce faster */
    case LEX_INTERPNORMAL:	/* code if we comment these out. */
	break;
#endif

    /* when we've already built the next token, just pull it out of the queue */
    case LEX_KNOWNEXT:
#ifdef PERL_MAD
	PL_lasttoke--;
	pl_yylval = PL_nexttoke[PL_lasttoke].next_val;
	if (PL_madskills) {
	    PL_thismad = PL_nexttoke[PL_lasttoke].next_mad;
	    PL_nexttoke[PL_lasttoke].next_mad = 0;
	    if (PL_thismad && PL_thismad->mad_key == '_') {
		PL_thiswhite = MUTABLE_SV(PL_thismad->mad_val);
		PL_thismad->mad_val = 0;
		mad_free(PL_thismad);
		PL_thismad = 0;
	    }
	}
	if (!PL_lasttoke) {
	    PL_lex_state = PL_lex_defer;
  	    PL_expect = PL_lex_expect;
  	    PL_lex_defer = LEX_NORMAL;
	    if (!PL_nexttoke[PL_lasttoke].next_type)
		return yylex();
  	}
#else
	PL_nexttoke--;
	pl_yylval = PL_nextval[PL_nexttoke];
	if (!PL_nexttoke) {
	    PL_lex_state = PL_lex_defer;
	    PL_expect = PL_lex_expect;
	    PL_lex_defer = LEX_NORMAL;
	}
#endif
#ifdef PERL_MAD
	/* FIXME - can these be merged?  */
	return(PL_nexttoke[PL_lasttoke].next_type);
#else
	return REPORT(PL_nexttype[PL_nexttoke]);
#endif

    /* interpolated case modifiers like \L \U, including \Q and \E.
       when we get here, PL_bufptr is at the \
    */
    case LEX_INTERPCASEMOD:
#ifdef DEBUGGING
	if (PL_bufptr != PL_bufend && *PL_bufptr != '\\')
	    Perl_croak(aTHX_ "panic: INTERPCASEMOD");
#endif
	/* handle \E or end of string */
       	if (PL_bufptr == PL_bufend || PL_bufptr[1] == 'E') {
	    /* if at a \E */
	    if (PL_lex_casemods) {
		const char oldmod = PL_lex_casestack[--PL_lex_casemods];
		PL_lex_casestack[PL_lex_casemods] = '\0';

		if (PL_bufptr != PL_bufend
		    && (oldmod == 'L' || oldmod == 'U' || oldmod == 'Q')) {
		    PL_bufptr += 2;
		    PL_lex_state = LEX_INTERPCONCAT;
#ifdef PERL_MAD
		    if (PL_madskills)
			PL_thistoken = newSVpvs("\\E");
#endif
		}
		return REPORT(')');
	    }
#ifdef PERL_MAD
	    while (PL_bufptr != PL_bufend &&
	      PL_bufptr[0] == '\\' && PL_bufptr[1] == 'E') {
		if (!PL_thiswhite)
		    PL_thiswhite = newSVpvs("");
		sv_catpvn(PL_thiswhite, PL_bufptr, 2);
		PL_bufptr += 2;
	    }
#else
	    if (PL_bufptr != PL_bufend)
		PL_bufptr += 2;
#endif
	    PL_lex_state = LEX_INTERPCONCAT;
	    return yylex();
	}
	else {
	    DEBUG_T({ PerlIO_printf(Perl_debug_log,
              "### Saw case modifier\n"); });
	    s = PL_bufptr + 1;
	    if (s[1] == '\\' && s[2] == 'E') {
#ifdef PERL_MAD
		if (!PL_thiswhite)
		    PL_thiswhite = newSVpvs("");
		sv_catpvn(PL_thiswhite, PL_bufptr, 4);
#endif
	        PL_bufptr = s + 3;
		PL_lex_state = LEX_INTERPCONCAT;
		return yylex();
	    }
	    else {
		I32 tmp;
		if (!PL_madskills) /* when just compiling don't need correct */
		    if (strnEQ(s, "L\\u", 3) || strnEQ(s, "U\\l", 3))
			tmp = *s, *s = s[2], s[2] = (char)tmp;	/* misordered... */
		if ((*s == 'L' || *s == 'U') &&
		    (strchr(PL_lex_casestack, 'L') || strchr(PL_lex_casestack, 'U'))) {
		    PL_lex_casestack[--PL_lex_casemods] = '\0';
		    return REPORT(')');
		}
		if (PL_lex_casemods > 10)
		    Renew(PL_lex_casestack, PL_lex_casemods + 2, char);
		PL_lex_casestack[PL_lex_casemods++] = *s;
		PL_lex_casestack[PL_lex_casemods] = '\0';
		PL_lex_state = LEX_INTERPCONCAT;
		start_force(PL_curforce);
		NEXTVAL_NEXTTOKE.ival = 0;
		force_next('(');
		start_force(PL_curforce);
		if (*s == 'l')
		    NEXTVAL_NEXTTOKE.ival = OP_LCFIRST;
		else if (*s == 'u')
		    NEXTVAL_NEXTTOKE.ival = OP_UCFIRST;
		else if (*s == 'L')
		    NEXTVAL_NEXTTOKE.ival = OP_LC;
		else if (*s == 'U')
		    NEXTVAL_NEXTTOKE.ival = OP_UC;
		else if (*s == 'Q')
		    NEXTVAL_NEXTTOKE.ival = OP_QUOTEMETA;
		else
		    Perl_croak(aTHX_ "panic: yylex");
		if (PL_madskills) {
		    SV* const tmpsv = newSVpvs("\\ ");
		    /* replace the space with the character we want to escape
		     */
		    SvPVX(tmpsv)[1] = *s;
		    curmad('_', tmpsv);
		}
		PL_bufptr = s + 1;
	    }
	    force_next(FUNC);
	    if (PL_lex_starts) {
		s = PL_bufptr;
		PL_lex_starts = 0;
#ifdef PERL_MAD
		if (PL_madskills) {
		    if (PL_thistoken)
			sv_free(PL_thistoken);
		    PL_thistoken = newSVpvs("");
		}
#endif
		/* commas only at base level: /$a\Ub$c/ => ($a,uc(b.$c)) */
		if (PL_lex_casemods == 1 && PL_lex_inpat)
		    OPERATOR(',');
		else
		    Aop(OP_CONCAT);
	    }
	    else
		return yylex();
	}

    case LEX_INTERPPUSH:
        return REPORT(sublex_push());

    case LEX_INTERPSTART:
	if (PL_bufptr == PL_bufend)
	    return REPORT(sublex_done());
	DEBUG_T({ PerlIO_printf(Perl_debug_log,
              "### Interpolated variable\n"); });
	PL_expect = XTERM;
	PL_lex_dojoin = (*PL_bufptr == '@');
	PL_lex_state = LEX_INTERPNORMAL;
	if (PL_lex_dojoin) {
	    start_force(PL_curforce);
	    NEXTVAL_NEXTTOKE.ival = 0;
	    force_next(',');
	    start_force(PL_curforce);
	    force_ident("\"", '$');
	    start_force(PL_curforce);
	    NEXTVAL_NEXTTOKE.ival = 0;
	    force_next('$');
	    start_force(PL_curforce);
	    NEXTVAL_NEXTTOKE.ival = 0;
	    force_next('(');
	    start_force(PL_curforce);
	    NEXTVAL_NEXTTOKE.ival = OP_JOIN;	/* emulate join($", ...) */
	    force_next(FUNC);
	}
	if (PL_lex_starts++) {
	    s = PL_bufptr;
#ifdef PERL_MAD
	    if (PL_madskills) {
		if (PL_thistoken)
		    sv_free(PL_thistoken);
		PL_thistoken = newSVpvs("");
	    }
#endif
	    /* commas only at base level: /$a\Ub$c/ => ($a,uc(b.$c)) */
	    if (!PL_lex_casemods && PL_lex_inpat)
		OPERATOR(',');
	    else
		Aop(OP_CONCAT);
	}
	return yylex();

    case LEX_INTERPENDMAYBE:
	if (intuit_more(PL_bufptr)) {
	    PL_lex_state = LEX_INTERPNORMAL;	/* false alarm, more expr */
	    break;
	}
	/* FALL THROUGH */

    case LEX_INTERPEND:
	if (PL_lex_dojoin) {
	    PL_lex_dojoin = FALSE;
	    PL_lex_state = LEX_INTERPCONCAT;
#ifdef PERL_MAD
	    if (PL_madskills) {
		if (PL_thistoken)
		    sv_free(PL_thistoken);
		PL_thistoken = newSVpvs("");
	    }
#endif
	    return REPORT(')');
	}
	if (PL_lex_inwhat == OP_SUBST && PL_linestr == PL_lex_repl
	    && SvEVALED(PL_lex_repl))
	{
	    if (PL_bufptr != PL_bufend)
		Perl_croak(aTHX_ "Bad evalled substitution pattern");
	    PL_lex_repl = NULL;
	}
	/* FALLTHROUGH */
    case LEX_INTERPCONCAT:
#ifdef DEBUGGING
	if (PL_lex_brackets)
	    Perl_croak(aTHX_ "panic: INTERPCONCAT");
#endif
	if (PL_bufptr == PL_bufend)
	    return REPORT(sublex_done());

	if (SvIVX(PL_linestr) == '\'') {
	    SV *sv = newSVsv(PL_linestr);
	    if (!PL_lex_inpat)
		sv = tokeq(sv);
	    else if ( PL_hints & HINT_NEW_RE )
		sv = new_constant(NULL, 0, "qr", sv, sv, "q", 1);
	    pl_yylval.opval = (OP*)newSVOP(OP_CONST, 0, sv);
	    s = PL_bufend;
	}
	else {
	    s = scan_const(PL_bufptr);
	    if (*s == '\\')
		PL_lex_state = LEX_INTERPCASEMOD;
	    else
		PL_lex_state = LEX_INTERPSTART;
	}

	if (s != PL_bufptr) {
	    start_force(PL_curforce);
	    if (PL_madskills) {
		curmad('X', newSVpvn(PL_bufptr,s-PL_bufptr));
	    }
	    NEXTVAL_NEXTTOKE = pl_yylval;
	    PL_expect = XTERM;
	    force_next(THING);
	    if (PL_lex_starts++) {
#ifdef PERL_MAD
		if (PL_madskills) {
		    if (PL_thistoken)
			sv_free(PL_thistoken);
		    PL_thistoken = newSVpvs("");
		}
#endif
		/* commas only at base level: /$a\Ub$c/ => ($a,uc(b.$c)) */
		if (!PL_lex_casemods && PL_lex_inpat)
		    OPERATOR(',');
		else
		    Aop(OP_CONCAT);
	    }
	    else {
		PL_bufptr = s;
		return yylex();
	    }
	}

	return yylex();
    case LEX_FORMLINE:
	PL_lex_state = LEX_NORMAL;
	s = scan_formline(PL_bufptr);
	if (!PL_lex_formbrack)
	    goto rightbracket;
	OPERATOR(';');
    }

    s = PL_bufptr;
    PL_oldoldbufptr = PL_oldbufptr;
    PL_oldbufptr = s;

  retry:
#ifdef PERL_MAD
    if (PL_thistoken) {
	sv_free(PL_thistoken);
	PL_thistoken = 0;
    }
    PL_realtokenstart = s - SvPVX(PL_linestr);	/* assume but undo on ws */
#endif
    switch (*s) {
    default:
	if (isIDFIRST_lazy_if(s,UTF))
	    goto keylookup;
	{
        unsigned char c = *s;
        len = UTF ? Perl_utf8_length(aTHX_ (U8 *) PL_linestart, (U8 *) s) : (STRLEN) (s - PL_linestart);
        if (len > UNRECOGNIZED_PRECEDE_COUNT) {
            d = UTF ? (char *) Perl_utf8_hop(aTHX_ (U8 *) s, -UNRECOGNIZED_PRECEDE_COUNT) : s - UNRECOGNIZED_PRECEDE_COUNT;
        } else {
            d = PL_linestart;
        }	
        *s = '\0';
        Perl_croak(aTHX_ "Unrecognized character \\x%02X; marked by <-- HERE after %s<-- HERE near column %d", c, d, (int) len + 1);
    }
    case 4:
    case 26:
	goto fake_eof;			/* emulate EOF on ^D or ^Z */
    case 0:
#ifdef PERL_MAD
	if (PL_madskills)
	    PL_faketokens = 0;
#endif
	if (!PL_rsfp) {
	    PL_last_uni = 0;
	    PL_last_lop = 0;
	    if (PL_lex_brackets) {
		yyerror((const char *)
			(PL_lex_formbrack
			 ? "Format not terminated"
			 : "Missing right curly or square bracket"));
	    }
            DEBUG_T( { PerlIO_printf(Perl_debug_log,
                        "### Tokener got EOF\n");
            } );
	    TOKEN(0);
	}
	if (s++ < PL_bufend)
	    goto retry;			/* ignore stray nulls */
	PL_last_uni = 0;
	PL_last_lop = 0;
	if (!PL_in_eval && !PL_preambled) {
	    PL_preambled = TRUE;
#ifdef PERL_MAD
	    if (PL_madskills)
		PL_faketokens = 1;
#endif
	    if (PL_perldb) {
		/* Generate a string of Perl code to load the debugger.
		 * If PERL5DB is set, it will return the contents of that,
		 * otherwise a compile-time require of perl5db.pl.  */

		const char * const pdb = PerlEnv_getenv("PERL5DB");

		if (pdb) {
		    sv_setpv(PL_linestr, pdb);
		    sv_catpvs(PL_linestr,";");
		} else {
		    SETERRNO(0,SS_NORMAL);
		    sv_setpvs(PL_linestr, "BEGIN { require 'perl5db.pl' };");
		}
	    } else
		sv_setpvs(PL_linestr,"");
	    if (PL_preambleav) {
		SV **svp = AvARRAY(PL_preambleav);
		SV **const end = svp + AvFILLp(PL_preambleav);
		while(svp <= end) {
		    sv_catsv(PL_linestr, *svp);
		    ++svp;
		    sv_catpvs(PL_linestr, ";");
		}
		sv_free(MUTABLE_SV(PL_preambleav));
		PL_preambleav = NULL;
	    }
	    if (PL_minus_E)
		sv_catpvs(PL_linestr,
			  "use feature ':5." STRINGIFY(PERL_VERSION) "';");
	    if (PL_minus_n || PL_minus_p) {
		sv_catpvs(PL_linestr, "LINE: while (<>) {"/*}*/);
		if (PL_minus_l)
		    sv_catpvs(PL_linestr,"chomp;");
		if (PL_minus_a) {
		    if (PL_minus_F) {
			if ((*PL_splitstr == '/' || *PL_splitstr == '\''
			     || *PL_splitstr == '"')
			      && strchr(PL_splitstr + 1, *PL_splitstr))
			    Perl_sv_catpvf(aTHX_ PL_linestr, "our @F=split(%s);", PL_splitstr);
			else {
			    /* "q\0${splitstr}\0" is legal perl. Yes, even NUL
			       bytes can be used as quoting characters.  :-) */
			    const char *splits = PL_splitstr;
			    sv_catpvs(PL_linestr, "our @F=split(q\0");
			    do {
				/* Need to \ \s  */
				if (*splits == '\\')
				    sv_catpvn(PL_linestr, splits, 1);
				sv_catpvn(PL_linestr, splits, 1);
			    } while (*splits++);
			    /* This loop will embed the trailing NUL of
			       PL_linestr as the last thing it does before
			       terminating.  */
			    sv_catpvs(PL_linestr, ");");
			}
		    }
		    else
		        sv_catpvs(PL_linestr,"our @F=split(' ');");
		}
	    }
	    sv_catpvs(PL_linestr, "\n");
	    PL_oldoldbufptr = PL_oldbufptr = s = PL_linestart = SvPVX(PL_linestr);
	    PL_bufend = SvPVX(PL_linestr) + SvCUR(PL_linestr);
	    PL_last_lop = PL_last_uni = NULL;
	    if ((PERLDB_LINE || PERLDB_SAVESRC) && PL_curstash != PL_debstash)
		update_debugger_info(PL_linestr, NULL, 0);
	    goto retry;
	}
	do {
	    fake_eof = 0;
	    bof = PL_rsfp ? TRUE : FALSE;
	    if (0) {
	      fake_eof:
		fake_eof = LEX_FAKE_EOF;
	    }
	    PL_bufptr = PL_bufend;
	    CopLINE_inc(PL_curcop);
	    if (!lex_next_chunk(fake_eof)) {
		CopLINE_dec(PL_curcop);
		s = PL_bufptr;
		TOKEN(';');	/* not infinite loop because rsfp is NULL now */
	    }
	    CopLINE_dec(PL_curcop);
#ifdef PERL_MAD
	    if (!PL_rsfp)
		PL_realtokenstart = -1;
#endif
	    s = PL_bufptr;
	    /* If it looks like the start of a BOM or raw UTF-16,
	     * check if it in fact is. */
	    if (bof && PL_rsfp &&
		     (*s == 0 ||
		      *(U8*)s == 0xEF ||
		      *(U8*)s >= 0xFE ||
		      s[1] == 0)) {
		bof = PerlIO_tell(PL_rsfp) == (Off_t)SvCUR(PL_linestr);
		if (bof) {
		    PL_bufend = SvPVX(PL_linestr) + SvCUR(PL_linestr);
		    s = swallow_bom((U8*)s);
		}
	    }
	    if (PL_doextract) {
		/* Incest with pod. */
#ifdef PERL_MAD
		if (PL_madskills)
		    sv_catsv(PL_thiswhite, PL_linestr);
#endif
		if (*s == '=' && strnEQ(s, "=cut", 4) && !isALPHA(s[4])) {
		    sv_setpvs(PL_linestr, "");
		    PL_oldoldbufptr = PL_oldbufptr = s = PL_linestart = SvPVX(PL_linestr);
		    PL_bufend = SvPVX(PL_linestr) + SvCUR(PL_linestr);
		    PL_last_lop = PL_last_uni = NULL;
		    PL_doextract = FALSE;
		}
	    }
	    if (PL_rsfp)
		incline(s);
	} while (PL_doextract);
	PL_oldoldbufptr = PL_oldbufptr = PL_bufptr = PL_linestart = s;
	PL_bufend = SvPVX(PL_linestr) + SvCUR(PL_linestr);
	PL_last_lop = PL_last_uni = NULL;
	if (CopLINE(PL_curcop) == 1) {
	    while (s < PL_bufend && isSPACE(*s))
		s++;
	    if (*s == ':' && s[1] != ':') /* for csh execing sh scripts */
		s++;
#ifdef PERL_MAD
	    if (PL_madskills)
		PL_thiswhite = newSVpvn(PL_linestart, s - PL_linestart);
#endif
	    d = NULL;
	    if (!PL_in_eval) {
		if (*s == '#' && *(s+1) == '!')
		    d = s + 2;
#ifdef ALTERNATE_SHEBANG
		else {
		    static char const as[] = ALTERNATE_SHEBANG;
		    if (*s == as[0] && strnEQ(s, as, sizeof(as) - 1))
			d = s + (sizeof(as) - 1);
		}
#endif /* ALTERNATE_SHEBANG */
	    }
	    if (d) {
		char *ipath;
		char *ipathend;

		while (isSPACE(*d))
		    d++;
		ipath = d;
		while (*d && !isSPACE(*d))
		    d++;
		ipathend = d;

#ifdef ARG_ZERO_IS_SCRIPT
		if (ipathend > ipath) {
		    /*
		     * HP-UX (at least) sets argv[0] to the script name,
		     * which makes $^X incorrect.  And Digital UNIX and Linux,
		     * at least, set argv[0] to the basename of the Perl
		     * interpreter. So, having found "#!", we'll set it right.
		     */
		    SV * const x = GvSV(gv_fetchpvs("\030", GV_ADD|GV_NOTQUAL,
						    SVt_PV)); /* $^X */
		    assert(SvPOK(x) || SvGMAGICAL(x));
		    if (sv_eq(x, CopFILESV(PL_curcop))) {
			sv_setpvn(x, ipath, ipathend - ipath);
			SvSETMAGIC(x);
		    }
		    else {
			STRLEN blen;
			STRLEN llen;
			const char *bstart = SvPV_const(CopFILESV(PL_curcop),blen);
			const char * const lstart = SvPV_const(x,llen);
			if (llen < blen) {
			    bstart += blen - llen;
			    if (strnEQ(bstart, lstart, llen) &&	bstart[-1] == '/') {
				sv_setpvn(x, ipath, ipathend - ipath);
				SvSETMAGIC(x);
			    }
			}
		    }
		    TAINT_NOT;	/* $^X is always tainted, but that's OK */
		}
#endif /* ARG_ZERO_IS_SCRIPT */

		/*
		 * Look for options.
		 */
		d = instr(s,"perl -");
		if (!d) {
		    d = instr(s,"perl");
#if defined(DOSISH)
		    /* avoid getting into infinite loops when shebang
		     * line contains "Perl" rather than "perl" */
		    if (!d) {
			for (d = ipathend-4; d >= ipath; --d) {
			    if ((*d == 'p' || *d == 'P')
				&& !ibcmp(d, "perl", 4))
			    {
				break;
			    }
			}
			if (d < ipath)
			    d = NULL;
		    }
#endif
		}
#ifdef ALTERNATE_SHEBANG
		/*
		 * If the ALTERNATE_SHEBANG on this system starts with a
		 * character that can be part of a Perl expression, then if
		 * we see it but not "perl", we're probably looking at the
		 * start of Perl code, not a request to hand off to some
		 * other interpreter.  Similarly, if "perl" is there, but
		 * not in the first 'word' of the line, we assume the line
		 * contains the start of the Perl program.
		 */
		if (d && *s != '#') {
		    const char *c = ipath;
		    while (*c && !strchr("; \t\r\n\f\v#", *c))
			c++;
		    if (c < d)
			d = NULL;	/* "perl" not in first word; ignore */
		    else
			*s = '#';	/* Don't try to parse shebang line */
		}
#endif /* ALTERNATE_SHEBANG */
		if (!d &&
		    *s == '#' &&
		    ipathend > ipath &&
		    !PL_minus_c &&
		    !instr(s,"indir") &&
		    instr(PL_origargv[0],"perl"))
		{
		    dVAR;
		    char **newargv;

		    *ipathend = '\0';
		    s = ipathend + 1;
		    while (s < PL_bufend && isSPACE(*s))
			s++;
		    if (s < PL_bufend) {
			Newx(newargv,PL_origargc+3,char*);
			newargv[1] = s;
			while (s < PL_bufend && !isSPACE(*s))
			    s++;
			*s = '\0';
			Copy(PL_origargv+1, newargv+2, PL_origargc+1, char*);
		    }
		    else
			newargv = PL_origargv;
		    newargv[0] = ipath;
		    PERL_FPU_PRE_EXEC
		    PerlProc_execv(ipath, EXEC_ARGV_CAST(newargv));
		    PERL_FPU_POST_EXEC
		    Perl_croak(aTHX_ "Can't exec %s", ipath);
		}
		if (d) {
		    while (*d && !isSPACE(*d))
			d++;
		    while (SPACE_OR_TAB(*d))
			d++;

		    if (*d++ == '-') {
			const bool switches_done = PL_doswitches;
			const U32 oldpdb = PL_perldb;
			const bool oldn = PL_minus_n;
			const bool oldp = PL_minus_p;
			const char *d1 = d;

			do {
			    bool baduni = FALSE;
			    if (*d1 == 'C') {
				const char *d2 = d1 + 1;
				if (parse_unicode_opts((const char **)&d2)
				    != PL_unicode)
				    baduni = TRUE;
			    }
			    if (baduni || *d1 == 'M' || *d1 == 'm') {
				const char * const m = d1;
				while (*d1 && !isSPACE(*d1))
				    d1++;
				Perl_croak(aTHX_ "Too late for \"-%.*s\" option",
				      (int)(d1 - m), m);
			    }
			    d1 = moreswitches(d1);
			} while (d1);
			if (PL_doswitches && !switches_done) {
			    int argc = PL_origargc;
			    char **argv = PL_origargv;
			    do {
				argc--,argv++;
			    } while (argc && argv[0][0] == '-' && argv[0][1]);
			    init_argv_symbols(argc,argv);
			}
			if (((PERLDB_LINE || PERLDB_SAVESRC) && !oldpdb) ||
			    ((PL_minus_n || PL_minus_p) && !(oldn || oldp)))
			      /* if we have already added "LINE: while (<>) {",
			         we must not do it again */
			{
			    sv_setpvs(PL_linestr, "");
			    PL_oldoldbufptr = PL_oldbufptr = s = PL_linestart = SvPVX(PL_linestr);
			    PL_bufend = SvPVX(PL_linestr) + SvCUR(PL_linestr);
			    PL_last_lop = PL_last_uni = NULL;
			    PL_preambled = FALSE;
			    if (PERLDB_LINE || PERLDB_SAVESRC)
				(void)gv_fetchfile(PL_origfilename);
			    goto retry;
			}
		    }
		}
	    }
	}
	if (PL_lex_formbrack && PL_lex_brackets <= PL_lex_formbrack) {
	    PL_bufptr = s;
	    PL_lex_state = LEX_FORMLINE;
	    return yylex();
	}
	goto retry;
    case '\r':
#ifdef PERL_STRICT_CR
	Perl_warn(aTHX_ "Illegal character \\%03o (carriage return)", '\r');
	Perl_croak(aTHX_
      "\t(Maybe you didn't strip carriage returns after a network transfer?)\n");
#endif
    case ' ': case '\t': case '\f': case 013:
#ifdef PERL_MAD
	PL_realtokenstart = -1;
	if (!PL_thiswhite)
	    PL_thiswhite = newSVpvs("");
	sv_catpvn(PL_thiswhite, s, 1);
#endif
	s++;
	goto retry;
    case '#':
    case '\n':
#ifdef PERL_MAD
	PL_realtokenstart = -1;
	if (PL_madskills)
	    PL_faketokens = 0;
#endif
	if (PL_lex_state != LEX_NORMAL || (PL_in_eval && !PL_rsfp)) {
	    if (*s == '#' && s == PL_linestart && PL_in_eval && !PL_rsfp) {
		/* handle eval qq[#line 1 "foo"\n ...] */
		CopLINE_dec(PL_curcop);
		incline(s);
	    }
	    if (PL_madskills && !PL_lex_formbrack && !PL_in_eval) {
		s = SKIPSPACE0(s);
		if (!PL_in_eval || PL_rsfp)
		    incline(s);
	    }
	    else {
		d = s;
		while (d < PL_bufend && *d != '\n')
		    d++;
		if (d < PL_bufend)
		    d++;
		else if (d > PL_bufend) /* Found by Ilya: feed random input to Perl. */
		  Perl_croak(aTHX_ "panic: input overflow");
#ifdef PERL_MAD
		if (PL_madskills)
		    PL_thiswhite = newSVpvn(s, d - s);
#endif
		s = d;
		incline(s);
	    }
	    if (PL_lex_formbrack && PL_lex_brackets <= PL_lex_formbrack) {
		PL_bufptr = s;
		PL_lex_state = LEX_FORMLINE;
		return yylex();
	    }
	}
	else {
#ifdef PERL_MAD
	    if (PL_madskills && CopLINE(PL_curcop) >= 1 && !PL_lex_formbrack) {
		if (CopLINE(PL_curcop) == 1 && s[0] == '#' && s[1] == '!') {
		    PL_faketokens = 0;
		    s = SKIPSPACE0(s);
		    TOKEN(PEG);	/* make sure any #! line is accessible */
		}
		s = SKIPSPACE0(s);
	    }
	    else {
/*		if (PL_madskills && PL_lex_formbrack) { */
		    d = s;
		    while (d < PL_bufend && *d != '\n')
			d++;
		    if (d < PL_bufend)
			d++;
		    else if (d > PL_bufend) /* Found by Ilya: feed random input to Perl. */
		      Perl_croak(aTHX_ "panic: input overflow");
		    if (PL_madskills && CopLINE(PL_curcop) >= 1) {
			if (!PL_thiswhite)
			    PL_thiswhite = newSVpvs("");
			if (CopLINE(PL_curcop) == 1) {
			    sv_setpvs(PL_thiswhite, "");
			    PL_faketokens = 0;
			}
			sv_catpvn(PL_thiswhite, s, d - s);
		    }
		    s = d;
/*		}
		*s = '\0';
		PL_bufend = s; */
	    }
#else
	    *s = '\0';
	    PL_bufend = s;
#endif
	}
	goto retry;
    case '-':
	if (s[1] && isALPHA(s[1]) && !isALNUM(s[2])) {
	    I32 ftst = 0;
	    char tmp;

	    s++;
	    PL_bufptr = s;
	    tmp = *s++;

	    while (s < PL_bufend && SPACE_OR_TAB(*s))
		s++;

	    if (strnEQ(s,"=>",2)) {
		s = force_word(PL_bufptr,WORD,FALSE,FALSE,FALSE);
		DEBUG_T( { printbuf("### Saw unary minus before =>, forcing word %s\n", s); } );
		OPERATOR('-');		/* unary minus */
	    }
	    PL_last_uni = PL_oldbufptr;
	    switch (tmp) {
	    case 'r': ftst = OP_FTEREAD;	break;
	    case 'w': ftst = OP_FTEWRITE;	break;
	    case 'x': ftst = OP_FTEEXEC;	break;
	    case 'o': ftst = OP_FTEOWNED;	break;
	    case 'R': ftst = OP_FTRREAD;	break;
	    case 'W': ftst = OP_FTRWRITE;	break;
	    case 'X': ftst = OP_FTREXEC;	break;
	    case 'O': ftst = OP_FTROWNED;	break;
	    case 'e': ftst = OP_FTIS;		break;
	    case 'z': ftst = OP_FTZERO;		break;
	    case 's': ftst = OP_FTSIZE;		break;
	    case 'f': ftst = OP_FTFILE;		break;
	    case 'd': ftst = OP_FTDIR;		break;
	    case 'l': ftst = OP_FTLINK;		break;
	    case 'p': ftst = OP_FTPIPE;		break;
	    case 'S': ftst = OP_FTSOCK;		break;
	    case 'u': ftst = OP_FTSUID;		break;
	    case 'g': ftst = OP_FTSGID;		break;
	    case 'k': ftst = OP_FTSVTX;		break;
	    case 'b': ftst = OP_FTBLK;		break;
	    case 'c': ftst = OP_FTCHR;		break;
	    case 't': ftst = OP_FTTTY;		break;
	    case 'T': ftst = OP_FTTEXT;		break;
	    case 'B': ftst = OP_FTBINARY;	break;
	    case 'M': case 'A': case 'C':
		gv_fetchpvs("\024", GV_ADD|GV_NOTQUAL, SVt_PV);
		switch (tmp) {
		case 'M': ftst = OP_FTMTIME;	break;
		case 'A': ftst = OP_FTATIME;	break;
		case 'C': ftst = OP_FTCTIME;	break;
		default:			break;
		}
		break;
	    default:
		break;
	    }
	    if (ftst) {
		PL_last_lop_op = (OPCODE)ftst;
		DEBUG_T( { PerlIO_printf(Perl_debug_log,
                        "### Saw file test %c\n", (int)tmp);
		} );
		FTST(ftst);
	    }
	    else {
		/* Assume it was a minus followed by a one-letter named
		 * subroutine call (or a -bareword), then. */
		DEBUG_T( { PerlIO_printf(Perl_debug_log,
			"### '-%c' looked like a file test but was not\n",
			(int) tmp);
		} );
		s = --PL_bufptr;
	    }
	}
	{
	    const char tmp = *s++;
	    if (*s == tmp) {
		s++;
		if (PL_expect == XOPERATOR)
		    TERM(POSTDEC);
		else
		    OPERATOR(PREDEC);
	    }
	    else if (*s == '>') {
		s++;
		s = SKIPSPACE1(s);
		if (isIDFIRST_lazy_if(s,UTF)) {
		    s = force_word(s,METHOD,FALSE,TRUE,FALSE);
		    TOKEN(ARROW);
		}
		else if (*s == '$')
		    OPERATOR(ARROW);
		else
		    TERM(ARROW);
	    }
	    if (PL_expect == XOPERATOR)
		Aop(OP_SUBTRACT);
	    else {
		if (isSPACE(*s) || !isSPACE(*PL_bufptr))
		    check_uni();
		OPERATOR('-');		/* unary minus */
	    }
	}

    case '+':
	{
	    const char tmp = *s++;
	    if (*s == tmp) {
		s++;
		if (PL_expect == XOPERATOR)
		    TERM(POSTINC);
		else
		    OPERATOR(PREINC);
	    }
	    if (PL_expect == XOPERATOR)
		Aop(OP_ADD);
	    else {
		if (isSPACE(*s) || !isSPACE(*PL_bufptr))
		    check_uni();
		OPERATOR('+');
	    }
	}

    case '*':
	if (PL_expect != XOPERATOR) {
	    s = scan_ident(s, PL_bufend, PL_tokenbuf, sizeof PL_tokenbuf, TRUE);
	    PL_expect = XOPERATOR;
	    force_ident(PL_tokenbuf, '*');
	    if (!*PL_tokenbuf)
		PREREF('*');
	    TERM('*');
	}
	s++;
	if (*s == '*') {
	    s++;
	    PWop(OP_POW);
	}
	Mop(OP_MULTIPLY);

    case '%':
	if (PL_expect == XOPERATOR) {
	    ++s;
	    Mop(OP_MODULO);
	}
	PL_tokenbuf[0] = '%';
	s = scan_ident(s, PL_bufend, PL_tokenbuf + 1,
		sizeof PL_tokenbuf - 1, FALSE);
	if (!PL_tokenbuf[1]) {
	    PREREF('%');
	}
	PL_pending_ident = '%';
	TERM('%');

    case '^':
	s++;
	BOop(OP_BIT_XOR);
    case '[':
	PL_lex_brackets++;
	{
	    const char tmp = *s++;
	    OPERATOR(tmp);
	}
    case '~':
	if (s[1] == '~'
	    && (PL_expect == XOPERATOR || PL_expect == XTERMORDORDOR))
	{
	    s += 2;
	    Eop(OP_SMARTMATCH);
	}
    case ',':
	{
	    const char tmp = *s++;
	    OPERATOR(tmp);
	}
    case ':':
	if (s[1] == ':') {
	    len = 0;
	    goto just_a_word_zero_gv;
	}
	s++;
	switch (PL_expect) {
	    OP *attrs;
#ifdef PERL_MAD
	    I32 stuffstart;
#endif
	case XOPERATOR:
	    if (!PL_in_my || PL_lex_state != LEX_NORMAL)
		break;
	    PL_bufptr = s;	/* update in case we back off */
	    if (*s == '=') {
		deprecate(":= for an empty attribute list");
	    }
	    goto grabattrs;
	case XATTRBLOCK:
	    PL_expect = XBLOCK;
	    goto grabattrs;
	case XATTRTERM:
	    PL_expect = XTERMBLOCK;
	 grabattrs:
#ifdef PERL_MAD
	    stuffstart = s - SvPVX(PL_linestr) - 1;
#endif
	    s = PEEKSPACE(s);
	    attrs = NULL;
	    while (isIDFIRST_lazy_if(s,UTF)) {
		I32 tmp;
		SV *sv;
		d = scan_word(s, PL_tokenbuf, sizeof PL_tokenbuf, FALSE, &len);
		if (isLOWER(*s) && (tmp = keyword(PL_tokenbuf, len, 0))) {
		    if (tmp < 0) tmp = -tmp;
		    switch (tmp) {
		    case KEY_or:
		    case KEY_and:
		    case KEY_for:
		    case KEY_foreach:
		    case KEY_unless:
		    case KEY_if:
		    case KEY_while:
		    case KEY_until:
			goto got_attrs;
		    default:
			break;
		    }
		}
		sv = newSVpvn(s, len);
		if (*d == '(') {
		    d = scan_str(d,TRUE,TRUE);
		    if (!d) {
			/* MUST advance bufptr here to avoid bogus
			   "at end of line" context messages from yyerror().
			 */
			PL_bufptr = s + len;
			yyerror("Unterminated attribute parameter in attribute list");
			if (attrs)
			    op_free(attrs);
			sv_free(sv);
			return REPORT(0);	/* EOF indicator */
		    }
		}
		if (PL_lex_stuff) {
		    sv_catsv(sv, PL_lex_stuff);
		    attrs = append_elem(OP_LIST, attrs,
					newSVOP(OP_CONST, 0, sv));
		    SvREFCNT_dec(PL_lex_stuff);
		    PL_lex_stuff = NULL;
		}
		else {
		    if (len == 6 && strnEQ(SvPVX(sv), "unique", len)) {
			sv_free(sv);
			if (PL_in_my == KEY_our) {
			    deprecate(":unique");
			}
			else
			    Perl_croak(aTHX_ "The 'unique' attribute may only be applied to 'our' variables");
		    }

		    /* NOTE: any CV attrs applied here need to be part of
		       the CVf_BUILTIN_ATTRS define in cv.h! */
		    else if (!PL_in_my && len == 6 && strnEQ(SvPVX(sv), "lvalue", len)) {
			sv_free(sv);
			CvLVALUE_on(PL_compcv);
		    }
		    else if (!PL_in_my && len == 6 && strnEQ(SvPVX(sv), "locked", len)) {
			sv_free(sv);
			deprecate(":locked");
		    }
		    else if (!PL_in_my && len == 6 && strnEQ(SvPVX(sv), "method", len)) {
			sv_free(sv);
			CvMETHOD_on(PL_compcv);
		    }
		    /* After we've set the flags, it could be argued that
		       we don't need to do the attributes.pm-based setting
		       process, and shouldn't bother appending recognized
		       flags.  To experiment with that, uncomment the
		       following "else".  (Note that's already been
		       uncommented.  That keeps the above-applied built-in
		       attributes from being intercepted (and possibly
		       rejected) by a package's attribute routines, but is
		       justified by the performance win for the common case
		       of applying only built-in attributes.) */
		    else
		        attrs = append_elem(OP_LIST, attrs,
					    newSVOP(OP_CONST, 0,
					      	    sv));
		}
		s = PEEKSPACE(d);
		if (*s == ':' && s[1] != ':')
		    s = PEEKSPACE(s+1);
		else if (s == d)
		    break;	/* require real whitespace or :'s */
		/* XXX losing whitespace on sequential attributes here */
	    }
	    {
		const char tmp
		    = (PL_expect == XOPERATOR ? '=' : '{'); /*'}(' for vi */
		if (*s != ';' && *s != '}' && *s != tmp
		    && (tmp != '=' || *s != ')')) {
		    const char q = ((*s == '\'') ? '"' : '\'');
		    /* If here for an expression, and parsed no attrs, back
		       off. */
		    if (tmp == '=' && !attrs) {
			s = PL_bufptr;
			break;
		    }
		    /* MUST advance bufptr here to avoid bogus "at end of line"
		       context messages from yyerror().
		    */
		    PL_bufptr = s;
		    yyerror( (const char *)
			     (*s
			      ? Perl_form(aTHX_ "Invalid separator character "
					  "%c%c%c in attribute list", q, *s, q)
			      : "Unterminated attribute list" ) );
		    if (attrs)
			op_free(attrs);
		    OPERATOR(':');
		}
	    }
	got_attrs:
	    if (attrs) {
		start_force(PL_curforce);
		NEXTVAL_NEXTTOKE.opval = attrs;
		CURMAD('_', PL_nextwhite);
		force_next(THING);
	    }
#ifdef PERL_MAD
	    if (PL_madskills) {
		PL_thistoken = newSVpvn(SvPVX(PL_linestr) + stuffstart,
				     (s - SvPVX(PL_linestr)) - stuffstart);
	    }
#endif
	    TOKEN(COLONATTR);
	}
	OPERATOR(':');
    case '(':
	s++;
	if (PL_last_lop == PL_oldoldbufptr || PL_last_uni == PL_oldoldbufptr)
	    PL_oldbufptr = PL_oldoldbufptr;		/* allow print(STDOUT 123) */
	else
	    PL_expect = XTERM;
	s = SKIPSPACE1(s);
	TOKEN('(');
    case ';':
	CLINE;
	{
	    const char tmp = *s++;
	    OPERATOR(tmp);
	}
    case ')':
	{
	    const char tmp = *s++;
	    s = SKIPSPACE1(s);
	    if (*s == '{')
		PREBLOCK(tmp);
	    TERM(tmp);
	}
    case ']':
	s++;
	if (PL_lex_brackets <= 0)
	    yyerror("Unmatched right square bracket");
	else
	    --PL_lex_brackets;
	if (PL_lex_state == LEX_INTERPNORMAL) {
	    if (PL_lex_brackets == 0) {
		if (*s == '-' && s[1] == '>')
		    PL_lex_state = LEX_INTERPENDMAYBE;
		else if (*s != '[' && *s != '{')
		    PL_lex_state = LEX_INTERPEND;
	    }
	}
	TERM(']');
    case '{':
      leftbracket:
	s++;
	if (PL_lex_brackets > 100) {
	    Renew(PL_lex_brackstack, PL_lex_brackets + 10, char);
	}
	switch (PL_expect) {
	case XTERM:
	    if (PL_lex_formbrack) {
		s--;
		PRETERMBLOCK(DO);
	    }
	    if (PL_oldoldbufptr == PL_last_lop)
		PL_lex_brackstack[PL_lex_brackets++] = XTERM;
	    else
		PL_lex_brackstack[PL_lex_brackets++] = XOPERATOR;
	    OPERATOR(HASHBRACK);
	case XOPERATOR:
	    while (s < PL_bufend && SPACE_OR_TAB(*s))
		s++;
	    d = s;
	    PL_tokenbuf[0] = '\0';
	    if (d < PL_bufend && *d == '-') {
		PL_tokenbuf[0] = '-';
		d++;
		while (d < PL_bufend && SPACE_OR_TAB(*d))
		    d++;
	    }
	    if (d < PL_bufend && isIDFIRST_lazy_if(d,UTF)) {
		d = scan_word(d, PL_tokenbuf + 1, sizeof PL_tokenbuf - 1,
			      FALSE, &len);
		while (d < PL_bufend && SPACE_OR_TAB(*d))
		    d++;
		if (*d == '}') {
		    const char minus = (PL_tokenbuf[0] == '-');
		    s = force_word(s + minus, WORD, FALSE, TRUE, FALSE);
		    if (minus)
			force_next('-');
		}
	    }
	    /* FALL THROUGH */
	case XATTRBLOCK:
	case XBLOCK:
	    PL_lex_brackstack[PL_lex_brackets++] = XSTATE;
	    PL_expect = XSTATE;
	    break;
	case XATTRTERM:
	case XTERMBLOCK:
	    PL_lex_brackstack[PL_lex_brackets++] = XOPERATOR;
	    PL_expect = XSTATE;
	    break;
	default: {
		const char *t;
		if (PL_oldoldbufptr == PL_last_lop)
		    PL_lex_brackstack[PL_lex_brackets++] = XTERM;
		else
		    PL_lex_brackstack[PL_lex_brackets++] = XOPERATOR;
		s = SKIPSPACE1(s);
		if (*s == '}') {
		    if (PL_expect == XREF && PL_lex_state == LEX_INTERPNORMAL) {
			PL_expect = XTERM;
			/* This hack is to get the ${} in the message. */
			PL_bufptr = s+1;
			yyerror("syntax error");
			break;
		    }
		    OPERATOR(HASHBRACK);
		}
		/* This hack serves to disambiguate a pair of curlies
		 * as being a block or an anon hash.  Normally, expectation
		 * determines that, but in cases where we're not in a
		 * position to expect anything in particular (like inside
		 * eval"") we have to resolve the ambiguity.  This code
		 * covers the case where the first term in the curlies is a
		 * quoted string.  Most other cases need to be explicitly
		 * disambiguated by prepending a "+" before the opening
		 * curly in order to force resolution as an anon hash.
		 *
		 * XXX should probably propagate the outer expectation
		 * into eval"" to rely less on this hack, but that could
		 * potentially break current behavior of eval"".
		 * GSAR 97-07-21
		 */
		t = s;
		if (*s == '\'' || *s == '"' || *s == '`') {
		    /* common case: get past first string, handling escapes */
		    for (t++; t < PL_bufend && *t != *s;)
			if (*t++ == '\\' && (*t == '\\' || *t == *s))
			    t++;
		    t++;
		}
		else if (*s == 'q') {
		    if (++t < PL_bufend
			&& (!isALNUM(*t)
			    || ((*t == 'q' || *t == 'x') && ++t < PL_bufend
				&& !isALNUM(*t))))
		    {
			/* skip q//-like construct */
			const char *tmps;
			char open, close, term;
			I32 brackets = 1;

			while (t < PL_bufend && isSPACE(*t))
			    t++;
			/* check for q => */
			if (t+1 < PL_bufend && t[0] == '=' && t[1] == '>') {
			    OPERATOR(HASHBRACK);
			}
			term = *t;
			open = term;
			if (term && (tmps = strchr("([{< )]}> )]}>",term)))
			    term = tmps[5];
			close = term;
			if (open == close)
			    for (t++; t < PL_bufend; t++) {
				if (*t == '\\' && t+1 < PL_bufend && open != '\\')
				    t++;
				else if (*t == open)
				    break;
			    }
			else {
			    for (t++; t < PL_bufend; t++) {
				if (*t == '\\' && t+1 < PL_bufend)
				    t++;
				else if (*t == close && --brackets <= 0)
				    break;
				else if (*t == open)
				    brackets++;
			    }
			}
			t++;
		    }
		    else
			/* skip plain q word */
			while (t < PL_bufend && isALNUM_lazy_if(t,UTF))
			     t += UTF8SKIP(t);
		}
		else if (isALNUM_lazy_if(t,UTF)) {
		    t += UTF8SKIP(t);
		    while (t < PL_bufend && isALNUM_lazy_if(t,UTF))
			 t += UTF8SKIP(t);
		}
		while (t < PL_bufend && isSPACE(*t))
		    t++;
		/* if comma follows first term, call it an anon hash */
		/* XXX it could be a comma expression with loop modifiers */
		if (t < PL_bufend && ((*t == ',' && (*s == 'q' || !isLOWER(*s)))
				   || (*t == '=' && t[1] == '>')))
		    OPERATOR(HASHBRACK);
		if (PL_expect == XREF)
		    PL_expect = XTERM;
		else {
		    PL_lex_brackstack[PL_lex_brackets-1] = XSTATE;
		    PL_expect = XSTATE;
		}
	    }
	    break;
	}
	pl_yylval.ival = CopLINE(PL_curcop);
	if (isSPACE(*s) || *s == '#')
	    PL_copline = NOLINE;   /* invalidate current command line number */
	TOKEN('{');
    case '}':
      rightbracket:
	s++;
	if (PL_lex_brackets <= 0)
	    yyerror("Unmatched right curly bracket");
	else
	    PL_expect = (expectation)PL_lex_brackstack[--PL_lex_brackets];
	if (PL_lex_brackets < PL_lex_formbrack && PL_lex_state != LEX_INTERPNORMAL)
	    PL_lex_formbrack = 0;
	if (PL_lex_state == LEX_INTERPNORMAL) {
	    if (PL_lex_brackets == 0) {
		if (PL_expect & XFAKEBRACK) {
		    PL_expect &= XENUMMASK;
		    PL_lex_state = LEX_INTERPEND;
		    PL_bufptr = s;
#if 0
		    if (PL_madskills) {
			if (!PL_thiswhite)
			    PL_thiswhite = newSVpvs("");
			sv_catpvs(PL_thiswhite,"}");
		    }
#endif
		    return yylex();	/* ignore fake brackets */
		}
		if (*s == '-' && s[1] == '>')
		    PL_lex_state = LEX_INTERPENDMAYBE;
		else if (*s != '[' && *s != '{')
		    PL_lex_state = LEX_INTERPEND;
	    }
	}
	if (PL_expect & XFAKEBRACK) {
	    PL_expect &= XENUMMASK;
	    PL_bufptr = s;
	    return yylex();		/* ignore fake brackets */
	}
	start_force(PL_curforce);
	if (PL_madskills) {
	    curmad('X', newSVpvn(s-1,1));
	    CURMAD('_', PL_thiswhite);
	}
	force_next('}');
#ifdef PERL_MAD
	if (!PL_thistoken)
	    PL_thistoken = newSVpvs("");
#endif
	TOKEN(';');
    case '&':
	s++;
	if (*s++ == '&')
	    AOPERATOR(ANDAND);
	s--;
	if (PL_expect == XOPERATOR) {
	    if (PL_bufptr == PL_linestart && ckWARN(WARN_SEMICOLON)
		&& isIDFIRST_lazy_if(s,UTF))
	    {
		CopLINE_dec(PL_curcop);
		Perl_warner(aTHX_ packWARN(WARN_SEMICOLON), "%s", PL_warn_nosemi);
		CopLINE_inc(PL_curcop);
	    }
	    BAop(OP_BIT_AND);
	}

	s = scan_ident(s - 1, PL_bufend, PL_tokenbuf, sizeof PL_tokenbuf, TRUE);
	if (*PL_tokenbuf) {
	    PL_expect = XOPERATOR;
	    force_ident(PL_tokenbuf, '&');
	}
	else
	    PREREF('&');
	pl_yylval.ival = (OPpENTERSUB_AMPER<<8);
	TERM('&');

    case '|':
	s++;
	if (*s++ == '|')
	    AOPERATOR(OROR);
	s--;
	BOop(OP_BIT_OR);
    case '=':
	s++;
	{
	    const char tmp = *s++;
	    if (tmp == '=')
		Eop(OP_EQ);
	    if (tmp == '>')
		OPERATOR(',');
	    if (tmp == '~')
		PMop(OP_MATCH);
	    if (tmp && isSPACE(*s) && ckWARN(WARN_SYNTAX)
		&& strchr("+-*/%.^&|<",tmp))
		Perl_warner(aTHX_ packWARN(WARN_SYNTAX),
			    "Reversed %c= operator",(int)tmp);
	    s--;
	    if (PL_expect == XSTATE && isALPHA(tmp) &&
		(s == PL_linestart+1 || s[-2] == '\n') )
		{
		    if (PL_in_eval && !PL_rsfp) {
			d = PL_bufend;
			while (s < d) {
			    if (*s++ == '\n') {
				incline(s);
				if (strnEQ(s,"=cut",4)) {
				    s = strchr(s,'\n');
				    if (s)
					s++;
				    else
					s = d;
				    incline(s);
				    goto retry;
				}
			    }
			}
			goto retry;
		    }
#ifdef PERL_MAD
		    if (PL_madskills) {
			if (!PL_thiswhite)
			    PL_thiswhite = newSVpvs("");
			sv_catpvn(PL_thiswhite, PL_linestart,
				  PL_bufend - PL_linestart);
		    }
#endif
		    s = PL_bufend;
		    PL_doextract = TRUE;
		    goto retry;
		}
	}
	if (PL_lex_brackets < PL_lex_formbrack) {
	    const char *t = s;
#ifdef PERL_STRICT_CR
	    while (SPACE_OR_TAB(*t))
#else
	    while (SPACE_OR_TAB(*t) || *t == '\r')
#endif
		t++;
	    if (*t == '\n' || *t == '#') {
		s--;
		PL_expect = XBLOCK;
		goto leftbracket;
	    }
	}
	pl_yylval.ival = 0;
	OPERATOR(ASSIGNOP);
    case '!':
	s++;
	{
	    const char tmp = *s++;
	    if (tmp == '=') {
		/* was this !=~ where !~ was meant?
		 * warn on m:!=~\s+([/?]|[msy]\W|tr\W): */

		if (*s == '~' && ckWARN(WARN_SYNTAX)) {
		    const char *t = s+1;

		    while (t < PL_bufend && isSPACE(*t))
			++t;

		    if (*t == '/' || *t == '?' ||
			((*t == 'm' || *t == 's' || *t == 'y')
			 && !isALNUM(t[1])) ||
			(*t == 't' && t[1] == 'r' && !isALNUM(t[2])))
			Perl_warner(aTHX_ packWARN(WARN_SYNTAX),
				    "!=~ should be !~");
		}
		Eop(OP_NE);
	    }
	    if (tmp == '~')
		PMop(OP_NOT);
	}
	s--;
	OPERATOR('!');
    case '<':
	if (PL_expect != XOPERATOR) {
	    if (s[1] != '<' && !strchr(s,'>'))
		check_uni();
	    if (s[1] == '<')
		s = scan_heredoc(s);
	    else
		s = scan_inputsymbol(s);
	    TERM(sublex_start());
	}
	s++;
	{
	    char tmp = *s++;
	    if (tmp == '<')
		SHop(OP_LEFT_SHIFT);
	    if (tmp == '=') {
		tmp = *s++;
		if (tmp == '>')
		    Eop(OP_NCMP);
		s--;
		Rop(OP_LE);
	    }
	}
	s--;
	Rop(OP_LT);
    case '>':
	s++;
	{
	    const char tmp = *s++;
	    if (tmp == '>')
		SHop(OP_RIGHT_SHIFT);
	    else if (tmp == '=')
		Rop(OP_GE);
	}
	s--;
	Rop(OP_GT);

    case '$':
	CLINE;

	if (PL_expect == XOPERATOR) {
	    if (PL_lex_formbrack && PL_lex_brackets == PL_lex_formbrack) {
		return deprecate_commaless_var_list();
	    }
	}

	if (s[1] == '#' && (isIDFIRST_lazy_if(s+2,UTF) || strchr("{$:+-", s[2]))) {
	    PL_tokenbuf[0] = '@';
	    s = scan_ident(s + 1, PL_bufend, PL_tokenbuf + 1,
			   sizeof PL_tokenbuf - 1, FALSE);
	    if (PL_expect == XOPERATOR)
		no_op("Array length", s);
	    if (!PL_tokenbuf[1])
		PREREF(DOLSHARP);
	    PL_expect = XOPERATOR;
	    PL_pending_ident = '#';
	    TOKEN(DOLSHARP);
	}

	PL_tokenbuf[0] = '$';
	s = scan_ident(s, PL_bufend, PL_tokenbuf + 1,
		       sizeof PL_tokenbuf - 1, FALSE);
	if (PL_expect == XOPERATOR)
	    no_op("Scalar", s);
	if (!PL_tokenbuf[1]) {
	    if (s == PL_bufend)
		yyerror("Final $ should be \\$ or $name");
	    PREREF('$');
	}

	/* This kludge not intended to be bulletproof. */
	if (PL_tokenbuf[1] == '[' && !PL_tokenbuf[2]) {
	    pl_yylval.opval = newSVOP(OP_CONST, 0,
				   newSViv(CopARYBASE_get(&PL_compiling)));
	    pl_yylval.opval->op_private = OPpCONST_ARYBASE;
	    TERM(THING);
	}

	d = s;
	{
	    const char tmp = *s;
	    if (PL_lex_state == LEX_NORMAL || PL_lex_brackets)
		s = SKIPSPACE1(s);

	    if ((PL_expect != XREF || PL_oldoldbufptr == PL_last_lop)
		&& intuit_more(s)) {
		if (*s == '[') {
		    PL_tokenbuf[0] = '@';
		    if (ckWARN(WARN_SYNTAX)) {
			char *t = s+1;

			while (isSPACE(*t) || isALNUM_lazy_if(t,UTF) || *t == '$')
			    t++;
			if (*t++ == ',') {
			    PL_bufptr = PEEKSPACE(PL_bufptr); /* XXX can realloc */
			    while (t < PL_bufend && *t != ']')
				t++;
			    Perl_warner(aTHX_ packWARN(WARN_SYNTAX),
					"Multidimensional syntax %.*s not supported",
				    (int)((t - PL_bufptr) + 1), PL_bufptr);
			}
		    }
		}
		else if (*s == '{') {
		    char *t;
		    PL_tokenbuf[0] = '%';
		    if (strEQ(PL_tokenbuf+1, "SIG")  && ckWARN(WARN_SYNTAX)
			&& (t = strchr(s, '}')) && (t = strchr(t, '=')))
			{
			    char tmpbuf[sizeof PL_tokenbuf];
			    do {
				t++;
			    } while (isSPACE(*t));
			    if (isIDFIRST_lazy_if(t,UTF)) {
				STRLEN len;
				t = scan_word(t, tmpbuf, sizeof tmpbuf, TRUE,
					      &len);
				while (isSPACE(*t))
				    t++;
				if (*t == ';' && get_cvn_flags(tmpbuf, len, 0))
				    Perl_warner(aTHX_ packWARN(WARN_SYNTAX),
						"You need to quote \"%s\"",
						tmpbuf);
			    }
			}
		}
	    }

	    PL_expect = XOPERATOR;
	    if (PL_lex_state == LEX_NORMAL && isSPACE((char)tmp)) {
		const bool islop = (PL_last_lop == PL_oldoldbufptr);
		if (!islop || PL_last_lop_op == OP_GREPSTART)
		    PL_expect = XOPERATOR;
		else if (strchr("$@\"'`q", *s))
		    PL_expect = XTERM;		/* e.g. print $fh "foo" */
		else if (strchr("&*<%", *s) && isIDFIRST_lazy_if(s+1,UTF))
		    PL_expect = XTERM;		/* e.g. print $fh &sub */
		else if (isIDFIRST_lazy_if(s,UTF)) {
		    char tmpbuf[sizeof PL_tokenbuf];
		    int t2;
		    scan_word(s, tmpbuf, sizeof tmpbuf, TRUE, &len);
		    if ((t2 = keyword(tmpbuf, len, 0))) {
			/* binary operators exclude handle interpretations */
			switch (t2) {
			case -KEY_x:
			case -KEY_eq:
			case -KEY_ne:
			case -KEY_gt:
			case -KEY_lt:
			case -KEY_ge:
			case -KEY_le:
			case -KEY_cmp:
			    break;
			default:
			    PL_expect = XTERM;	/* e.g. print $fh length() */
			    break;
			}
		    }
		    else {
			PL_expect = XTERM;	/* e.g. print $fh subr() */
		    }
		}
		else if (isDIGIT(*s))
		    PL_expect = XTERM;		/* e.g. print $fh 3 */
		else if (*s == '.' && isDIGIT(s[1]))
		    PL_expect = XTERM;		/* e.g. print $fh .3 */
		else if ((*s == '?' || *s == '-' || *s == '+')
			 && !isSPACE(s[1]) && s[1] != '=')
		    PL_expect = XTERM;		/* e.g. print $fh -1 */
		else if (*s == '/' && !isSPACE(s[1]) && s[1] != '='
			 && s[1] != '/')
		    PL_expect = XTERM;		/* e.g. print $fh /.../
						   XXX except DORDOR operator
						*/
		else if (*s == '<' && s[1] == '<' && !isSPACE(s[2])
			 && s[2] != '=')
		    PL_expect = XTERM;		/* print $fh <<"EOF" */
	    }
	}
	PL_pending_ident = '$';
	TOKEN('$');

    case '@':
	if (PL_expect == XOPERATOR)
	    no_op("Array", s);
	PL_tokenbuf[0] = '@';
	s = scan_ident(s, PL_bufend, PL_tokenbuf + 1, sizeof PL_tokenbuf - 1, FALSE);
	if (!PL_tokenbuf[1]) {
	    PREREF('@');
	}
	if (PL_lex_state == LEX_NORMAL)
	    s = SKIPSPACE1(s);
	if ((PL_expect != XREF || PL_oldoldbufptr == PL_last_lop) && intuit_more(s)) {
	    if (*s == '{')
		PL_tokenbuf[0] = '%';

	    /* Warn about @ where they meant $. */
	    if (*s == '[' || *s == '{') {
		if (ckWARN(WARN_SYNTAX)) {
		    const char *t = s + 1;
		    while (*t && (isALNUM_lazy_if(t,UTF) || strchr(" \t$#+-'\"", *t)))
			t++;
		    if (*t == '}' || *t == ']') {
			t++;
			PL_bufptr = PEEKSPACE(PL_bufptr); /* XXX can realloc */
			Perl_warner(aTHX_ packWARN(WARN_SYNTAX),
			    "Scalar value %.*s better written as $%.*s",
			    (int)(t-PL_bufptr), PL_bufptr,
			    (int)(t-PL_bufptr-1), PL_bufptr+1);
		    }
		}
	    }
	}
	PL_pending_ident = '@';
	TERM('@');

     case '/':			/* may be division, defined-or, or pattern */
	if (PL_expect == XTERMORDORDOR && s[1] == '/') {
	    s += 2;
	    AOPERATOR(DORDOR);
	}
     case '?':			/* may either be conditional or pattern */
	if (PL_expect == XOPERATOR) {
	     char tmp = *s++;
	     if(tmp == '?') {
		OPERATOR('?');
	     }
             else {
	         tmp = *s++;
	         if(tmp == '/') {
	             /* A // operator. */
	            AOPERATOR(DORDOR);
	         }
	         else {
	             s--;
	             Mop(OP_DIVIDE);
	         }
	     }
	 }
	 else {
	     /* Disable warning on "study /blah/" */
	     if (PL_oldoldbufptr == PL_last_uni
	      && (*PL_last_uni != 's' || s - PL_last_uni < 5
	          || memNE(PL_last_uni, "study", 5)
	          || isALNUM_lazy_if(PL_last_uni+5,UTF)
	      ))
	         check_uni();
	     s = scan_pat(s,OP_MATCH);
	     TERM(sublex_start());
	 }

    case '.':
	if (PL_lex_formbrack && PL_lex_brackets == PL_lex_formbrack
#ifdef PERL_STRICT_CR
	    && s[1] == '\n'
#else
	    && (s[1] == '\n' || (s[1] == '\r' && s[2] == '\n'))
#endif
	    && (s == PL_linestart || s[-1] == '\n') )
	{
	    PL_lex_formbrack = 0;
	    PL_expect = XSTATE;
	    goto rightbracket;
	}
	if (PL_expect == XSTATE && s[1] == '.' && s[2] == '.') {
	    s += 3;
	    OPERATOR(YADAYADA);
	}
	if (PL_expect == XOPERATOR || !isDIGIT(s[1])) {
	    char tmp = *s++;
	    if (*s == tmp) {
		s++;
		if (*s == tmp) {
		    s++;
		    pl_yylval.ival = OPf_SPECIAL;
		}
		else
		    pl_yylval.ival = 0;
		OPERATOR(DOTDOT);
	    }
	    Aop(OP_CONCAT);
	}
	/* FALL THROUGH */
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
	s = scan_num(s, &pl_yylval);
	DEBUG_T( { printbuf("### Saw number in %s\n", s); } );
	if (PL_expect == XOPERATOR)
	    no_op("Number",s);
	TERM(THING);

    case '\'':
	s = scan_str(s,!!PL_madskills,FALSE);
	DEBUG_T( { printbuf("### Saw string before %s\n", s); } );
	if (PL_expect == XOPERATOR) {
	    if (PL_lex_formbrack && PL_lex_brackets == PL_lex_formbrack) {
		return deprecate_commaless_var_list();
	    }
	    else
		no_op("String",s);
	}
	if (!s)
	    missingterm(NULL);
	pl_yylval.ival = OP_CONST;
	TERM(sublex_start());

    case '"':
	s = scan_str(s,!!PL_madskills,FALSE);
	DEBUG_T( { printbuf("### Saw string before %s\n", s); } );
	if (PL_expect == XOPERATOR) {
	    if (PL_lex_formbrack && PL_lex_brackets == PL_lex_formbrack) {
		return deprecate_commaless_var_list();
	    }
	    else
		no_op("String",s);
	}
	if (!s)
	    missingterm(NULL);
	pl_yylval.ival = OP_CONST;
	/* FIXME. I think that this can be const if char *d is replaced by
	   more localised variables.  */
	for (d = SvPV(PL_lex_stuff, len); len; len--, d++) {
	    if (*d == '$' || *d == '@' || *d == '\\' || !UTF8_IS_INVARIANT((U8)*d)) {
		pl_yylval.ival = OP_STRINGIFY;
		break;
	    }
	}
	TERM(sublex_start());

    case '`':
	s = scan_str(s,!!PL_madskills,FALSE);
	DEBUG_T( { printbuf("### Saw backtick string before %s\n", s); } );
	if (PL_expect == XOPERATOR)
	    no_op("Backticks",s);
	if (!s)
	    missingterm(NULL);
	readpipe_override();
	TERM(sublex_start());

    case '\\':
	s++;
	if (PL_lex_inwhat && isDIGIT(*s))
	    Perl_ck_warner(aTHX_ packWARN(WARN_SYNTAX),"Can't use \\%c to mean $%c in expression",
			   *s, *s);
	if (PL_expect == XOPERATOR)
	    no_op("Backslash",s);
	OPERATOR(REFGEN);

    case 'v':
	if (isDIGIT(s[1]) && PL_expect != XOPERATOR) {
	    char *start = s + 2;
	    while (isDIGIT(*start) || *start == '_')
		start++;
	    if (*start == '.' && isDIGIT(start[1])) {
		s = scan_num(s, &pl_yylval);
		TERM(THING);
	    }
	    /* avoid v123abc() or $h{v1}, allow C<print v10;> */
	    else if (!isALPHA(*start) && (PL_expect == XTERM
			|| PL_expect == XREF || PL_expect == XSTATE
			|| PL_expect == XTERMORDORDOR)) {
		GV *const gv = gv_fetchpvn_flags(s, start - s, 0, SVt_PVCV);
		if (!gv) {
		    s = scan_num(s, &pl_yylval);
		    TERM(THING);
		}
	    }
	}
	goto keylookup;
    case 'x':
	if (isDIGIT(s[1]) && PL_expect == XOPERATOR) {
	    s++;
	    Mop(OP_REPEAT);
	}
	goto keylookup;

    case '_':
    case 'a': case 'A':
    case 'b': case 'B':
    case 'c': case 'C':
    case 'd': case 'D':
    case 'e': case 'E':
    case 'f': case 'F':
    case 'g': case 'G':
    case 'h': case 'H':
    case 'i': case 'I':
    case 'j': case 'J':
    case 'k': case 'K':
    case 'l': case 'L':
    case 'm': case 'M':
    case 'n': case 'N':
    case 'o': case 'O':
    case 'p': case 'P':
    case 'q': case 'Q':
    case 'r': case 'R':
    case 's': case 'S':
    case 't': case 'T':
    case 'u': case 'U':
	      case 'V':
    case 'w': case 'W':
	      case 'X':
    case 'y': case 'Y':
    case 'z': case 'Z':

      keylookup: {
	bool anydelim;
	I32 tmp;

	orig_keyword = 0;
	gv = NULL;
	gvp = NULL;

	PL_bufptr = s;
	s = scan_word(s, PL_tokenbuf, sizeof PL_tokenbuf, FALSE, &len);

	/* Some keywords can be followed by any delimiter, including ':' */
	anydelim = ((len == 1 && strchr("msyq", PL_tokenbuf[0])) ||
	       (len == 2 && ((PL_tokenbuf[0] == 't' && PL_tokenbuf[1] == 'r') ||
			     (PL_tokenbuf[0] == 'q' &&
			      strchr("qwxr", PL_tokenbuf[1])))));

	/* x::* is just a word, unless x is "CORE" */
	if (!anydelim && *s == ':' && s[1] == ':' && strNE(PL_tokenbuf, "CORE"))
	    goto just_a_word;

	d = s;
	while (d < PL_bufend && isSPACE(*d))
		d++;	/* no comments skipped here, or s### is misparsed */

	/* Is this a word before a => operator? */
	if (*d == '=' && d[1] == '>') {
	    CLINE;
	    pl_yylval.opval
		= (OP*)newSVOP(OP_CONST, 0,
			       S_newSV_maybe_utf8(aTHX_ PL_tokenbuf, len));
	    pl_yylval.opval->op_private = OPpCONST_BARE;
	    TERM(WORD);
	}

	/* Check for plugged-in keyword */
	{
	    OP *o;
	    int result;
	    char *saved_bufptr = PL_bufptr;
	    PL_bufptr = s;
	    result = CALL_FPTR(PL_keyword_plugin)(aTHX_ PL_tokenbuf, len, &o);
	    s = PL_bufptr;
	    if (result == KEYWORD_PLUGIN_DECLINE) {
		/* not a plugged-in keyword */
		PL_bufptr = saved_bufptr;
	    } else if (result == KEYWORD_PLUGIN_STMT) {
		pl_yylval.opval = o;
		CLINE;
		PL_expect = XSTATE;
		return REPORT(PLUGSTMT);
	    } else if (result == KEYWORD_PLUGIN_EXPR) {
		pl_yylval.opval = o;
		CLINE;
		PL_expect = XOPERATOR;
		return REPORT(PLUGEXPR);
	    } else {
		Perl_croak(aTHX_ "Bad plugin affecting keyword '%s'",
					PL_tokenbuf);
	    }
	}

	/* Check for built-in keyword */
	tmp = keyword(PL_tokenbuf, len, 0);

	/* Is this a label? */
	if (!anydelim && PL_expect == XSTATE
	      && d < PL_bufend && *d == ':' && *(d + 1) != ':') {
	    s = d + 1;
	    pl_yylval.pval = CopLABEL_alloc(PL_tokenbuf);
	    CLINE;
	    TOKEN(LABEL);
	}

	if (tmp < 0) {			/* second-class keyword? */
	    GV *ogv = NULL;	/* override (winner) */
	    GV *hgv = NULL;	/* hidden (loser) */
	    if (PL_expect != XOPERATOR && (*s != ':' || s[1] != ':')) {
		CV *cv;
		if ((gv = gv_fetchpvn_flags(PL_tokenbuf, len, 0, SVt_PVCV)) &&
		    (cv = GvCVu(gv)))
		{
		    if (GvIMPORTED_CV(gv))
			ogv = gv;
		    else if (! CvMETHOD(cv))
			hgv = gv;
		}
		if (!ogv &&
		    (gvp = (GV**)hv_fetch(PL_globalstash,PL_tokenbuf,len,FALSE)) &&
		    (gv = *gvp) && isGV_with_GP(gv) &&
		    GvCVu(gv) && GvIMPORTED_CV(gv))
		{
		    ogv = gv;
		}
	    }
	    if (ogv) {
		orig_keyword = tmp;
		tmp = 0;		/* overridden by import or by GLOBAL */
	    }
	    else if (gv && !gvp
		     && -tmp==KEY_lock	/* XXX generalizable kludge */
		     && GvCVu(gv))
	    {
		tmp = 0;		/* any sub overrides "weak" keyword */
	    }
	    else {			/* no override */
		tmp = -tmp;
		if (tmp == KEY_dump) {
		    Perl_ck_warner(aTHX_ packWARN(WARN_MISC),
				   "dump() better written as CORE::dump()");
		}
		gv = NULL;
		gvp = 0;
		if (hgv && tmp != KEY_x && tmp != KEY_CORE)	/* never ambiguous */
		    Perl_ck_warner(aTHX_ packWARN(WARN_AMBIGUOUS),
				   "Ambiguous call resolved as CORE::%s(), %s",
				   GvENAME(hgv), "qualify as such or use &");
	    }
	}

      reserved_word:
	switch (tmp) {

	default:			/* not a keyword */
	    /* Trade off - by using this evil construction we can pull the
	       variable gv into the block labelled keylookup. If not, then
	       we have to give it function scope so that the goto from the
	       earlier ':' case doesn't bypass the initialisation.  */
	    if (0) {
	    just_a_word_zero_gv:
		gv = NULL;
		gvp = NULL;
		orig_keyword = 0;
	    }
	  just_a_word: {
		SV *sv;
		int pkgname = 0;
		const char lastchar = (PL_bufptr == PL_oldoldbufptr ? 0 : PL_bufptr[-1]);
		OP *rv2cv_op;
		CV *cv;
#ifdef PERL_MAD
		SV *nextPL_nextwhite = 0;
#endif


		/* Get the rest if it looks like a package qualifier */

		if (*s == '\'' || (*s == ':' && s[1] == ':')) {
		    STRLEN morelen;
		    s = scan_word(s, PL_tokenbuf + len, sizeof PL_tokenbuf - len,
				  TRUE, &morelen);
		    if (!morelen)
			Perl_croak(aTHX_ "Bad name after %s%s", PL_tokenbuf,
				*s == '\'' ? "'" : "::");
		    len += morelen;
		    pkgname = 1;
		}

		if (PL_expect == XOPERATOR) {
		    if (PL_bufptr == PL_linestart) {
			CopLINE_dec(PL_curcop);
			Perl_warner(aTHX_ packWARN(WARN_SEMICOLON), "%s", PL_warn_nosemi);
			CopLINE_inc(PL_curcop);
		    }
		    else
			no_op("Bareword",s);
		}

		/* Look for a subroutine with this name in current package,
		   unless name is "Foo::", in which case Foo is a bearword
		   (and a package name). */

		if (len > 2 && !PL_madskills &&
		    PL_tokenbuf[len - 2] == ':' && PL_tokenbuf[len - 1] == ':')
		{
		    if (ckWARN(WARN_BAREWORD)
			&& ! gv_fetchpvn_flags(PL_tokenbuf, len, 0, SVt_PVHV))
			Perl_warner(aTHX_ packWARN(WARN_BAREWORD),
		  	    "Bareword \"%s\" refers to nonexistent package",
			     PL_tokenbuf);
		    len -= 2;
		    PL_tokenbuf[len] = '\0';
		    gv = NULL;
		    gvp = 0;
		}
		else {
		    if (!gv) {
			/* Mustn't actually add anything to a symbol table.
			   But also don't want to "initialise" any placeholder
			   constants that might already be there into full
			   blown PVGVs with attached PVCV.  */
			gv = gv_fetchpvn_flags(PL_tokenbuf, len,
					       GV_NOADD_NOINIT, SVt_PVCV);
		    }
		    len = 0;
		}

		/* if we saw a global override before, get the right name */

		if (gvp) {
		    sv = newSVpvs("CORE::GLOBAL::");
		    sv_catpv(sv,PL_tokenbuf);
		}
		else {
		    /* If len is 0, newSVpv does strlen(), which is correct.
		       If len is non-zero, then it will be the true length,
		       and so the scalar will be created correctly.  */
		    sv = newSVpv(PL_tokenbuf,len);
		}
#ifdef PERL_MAD
		if (PL_madskills && !PL_thistoken) {
		    char *start = SvPVX(PL_linestr) + PL_realtokenstart;
		    PL_thistoken = newSVpvn(start,s - start);
		    PL_realtokenstart = s - SvPVX(PL_linestr);
		}
#endif

		/* Presume this is going to be a bareword of some sort. */

		CLINE;
		pl_yylval.opval = (OP*)newSVOP(OP_CONST, 0, sv);
		pl_yylval.opval->op_private = OPpCONST_BARE;
		/* UTF-8 package name? */
		if (UTF && !IN_BYTES &&
		    is_utf8_string((U8*)SvPVX_const(sv), SvCUR(sv)))
		    SvUTF8_on(sv);

		/* And if "Foo::", then that's what it certainly is. */

		if (len)
		    goto safe_bareword;

		cv = NULL;
		{
		    OP *const_op = newSVOP(OP_CONST, 0, SvREFCNT_inc(sv));
		    const_op->op_private = OPpCONST_BARE;
		    rv2cv_op = newCVREF(0, const_op);
		}
		if (rv2cv_op->op_type == OP_RV2CV &&
			(rv2cv_op->op_flags & OPf_KIDS)) {
		    OP *rv_op = cUNOPx(rv2cv_op)->op_first;
		    switch (rv_op->op_type) {
			case OP_CONST: {
			    SV *sv = cSVOPx_sv(rv_op);
			    if (SvROK(sv) && SvTYPE(SvRV(sv)) == SVt_PVCV)
				cv = (CV*)SvRV(sv);
			} break;
			case OP_GV: {
			    GV *gv = cGVOPx_gv(rv_op);
			    CV *maybe_cv = GvCVu(gv);
			    if (maybe_cv && SvTYPE((SV*)maybe_cv) == SVt_PVCV)
				cv = maybe_cv;
			} break;
		    }
		}

		/* See if it's the indirect object for a list operator. */

		if (PL_oldoldbufptr &&
		    PL_oldoldbufptr < PL_bufptr &&
		    (PL_oldoldbufptr == PL_last_lop
		     || PL_oldoldbufptr == PL_last_uni) &&
		    /* NO SKIPSPACE BEFORE HERE! */
		    (PL_expect == XREF ||
		     ((PL_opargs[PL_last_lop_op] >> OASHIFT)& 7) == OA_FILEREF))
		{
		    bool immediate_paren = *s == '(';

		    /* (Now we can afford to cross potential line boundary.) */
		    s = SKIPSPACE2(s,nextPL_nextwhite);
#ifdef PERL_MAD
		    PL_nextwhite = nextPL_nextwhite;	/* assume no & deception */
#endif

		    /* Two barewords in a row may indicate method call. */

		    if ((isIDFIRST_lazy_if(s,UTF) || *s == '$') &&
			(tmp = intuit_method(s, gv, cv))) {
			op_free(rv2cv_op);
			return REPORT(tmp);
		    }

		    /* If not a declared subroutine, it's an indirect object. */
		    /* (But it's an indir obj regardless for sort.) */
		    /* Also, if "_" follows a filetest operator, it's a bareword */

		    if (
			( !immediate_paren && (PL_last_lop_op == OP_SORT ||
                         (!cv &&
                        (PL_last_lop_op != OP_MAPSTART &&
			 PL_last_lop_op != OP_GREPSTART))))
		       || (PL_tokenbuf[0] == '_' && PL_tokenbuf[1] == '\0'
			    && ((PL_opargs[PL_last_lop_op] & OA_CLASS_MASK) == OA_FILESTATOP))
		       )
		    {
			PL_expect = (PL_last_lop == PL_oldoldbufptr) ? XTERM : XOPERATOR;
			goto bareword;
		    }
		}

		PL_expect = XOPERATOR;
#ifdef PERL_MAD
		if (isSPACE(*s))
		    s = SKIPSPACE2(s,nextPL_nextwhite);
		PL_nextwhite = nextPL_nextwhite;
#else
		s = skipspace(s);
#endif

		/* Is this a word before a => operator? */
		if (*s == '=' && s[1] == '>' && !pkgname) {
		    op_free(rv2cv_op);
		    CLINE;
		    sv_setpv(((SVOP*)pl_yylval.opval)->op_sv, PL_tokenbuf);
		    if (UTF && !IN_BYTES && is_utf8_string((U8*)PL_tokenbuf, len))
		      SvUTF8_on(((SVOP*)pl_yylval.opval)->op_sv);
		    TERM(WORD);
		}

		/* If followed by a paren, it's certainly a subroutine. */
		if (*s == '(') {
		    CLINE;
		    if (cv) {
			d = s + 1;
			while (SPACE_OR_TAB(*d))
			    d++;
			if (*d == ')' && (sv = cv_const_sv(cv))) {
			    s = d + 1;
			    goto its_constant;
			}
		    }
#ifdef PERL_MAD
		    if (PL_madskills) {
			PL_nextwhite = PL_thiswhite;
			PL_thiswhite = 0;
		    }
		    start_force(PL_curforce);
#endif
		    NEXTVAL_NEXTTOKE.opval = pl_yylval.opval;
		    PL_expect = XOPERATOR;
#ifdef PERL_MAD
		    if (PL_madskills) {
			PL_nextwhite = nextPL_nextwhite;
			curmad('X', PL_thistoken);
			PL_thistoken = newSVpvs("");
		    }
#endif
		    op_free(rv2cv_op);
		    force_next(WORD);
		    pl_yylval.ival = 0;
		    TOKEN('&');
		}

		/* If followed by var or block, call it a method (unless sub) */

		if ((*s == '$' || *s == '{') && !cv) {
		    op_free(rv2cv_op);
		    PL_last_lop = PL_oldbufptr;
		    PL_last_lop_op = OP_METHOD;
		    PREBLOCK(METHOD);
		}

		/* If followed by a bareword, see if it looks like indir obj. */

		if (!orig_keyword
			&& (isIDFIRST_lazy_if(s,UTF) || *s == '$')
			&& (tmp = intuit_method(s, gv, cv))) {
		    op_free(rv2cv_op);
		    return REPORT(tmp);
		}

		/* Not a method, so call it a subroutine (if defined) */

		if (cv) {
		    if (lastchar == '-')
			Perl_ck_warner_d(aTHX_ packWARN(WARN_AMBIGUOUS),
					 "Ambiguous use of -%s resolved as -&%s()",
					 PL_tokenbuf, PL_tokenbuf);
		    /* Check for a constant sub */
		    if ((sv = cv_const_sv(cv))) {
		  its_constant:
			op_free(rv2cv_op);
			SvREFCNT_dec(((SVOP*)pl_yylval.opval)->op_sv);
			((SVOP*)pl_yylval.opval)->op_sv = SvREFCNT_inc_simple(sv);
			pl_yylval.opval->op_private = 0;
			TOKEN(WORD);
		    }

		    op_free(pl_yylval.opval);
		    pl_yylval.opval = rv2cv_op;
		    pl_yylval.opval->op_private |= OPpENTERSUB_NOPAREN;
		    PL_last_lop = PL_oldbufptr;
		    PL_last_lop_op = OP_ENTERSUB;
		    /* Is there a prototype? */
		    if (
#ifdef PERL_MAD
			cv &&
#endif
			SvPOK(cv))
		    {
			STRLEN protolen;
			const char *proto = SvPV_const(MUTABLE_SV(cv), protolen);
			if (!protolen)
			    TERM(FUNC0SUB);
			if ((*proto == '$' || *proto == '_') && proto[1] == '\0')
			    OPERATOR(UNIOPSUB);
			while (*proto == ';')
			    proto++;
			if (*proto == '&' && *s == '{') {
			    if (PL_curstash)
				sv_setpvs(PL_subname, "__ANON__");
			    else
				sv_setpvs(PL_subname, "__ANON__::__ANON__");
			    PREBLOCK(LSTOPSUB);
			}
		    }
#ifdef PERL_MAD
		    {
			if (PL_madskills) {
			    PL_nextwhite = PL_thiswhite;
			    PL_thiswhite = 0;
			}
			start_force(PL_curforce);
			NEXTVAL_NEXTTOKE.opval = pl_yylval.opval;
			PL_expect = XTERM;
			if (PL_madskills) {
			    PL_nextwhite = nextPL_nextwhite;
			    curmad('X', PL_thistoken);
			    PL_thistoken = newSVpvs("");
			}
			force_next(WORD);
			TOKEN(NOAMP);
		    }
		}

		/* Guess harder when madskills require "best effort". */
		if (PL_madskills && (!gv || !GvCVu(gv))) {
		    int probable_sub = 0;
		    if (strchr("\"'`$@%0123456789!*+{[<", *s))
			probable_sub = 1;
		    else if (isALPHA(*s)) {
			char tmpbuf[1024];
			STRLEN tmplen;
			d = s;
			d = scan_word(d, tmpbuf, sizeof tmpbuf, TRUE, &tmplen);
			if (!keyword(tmpbuf, tmplen, 0))
			    probable_sub = 1;
			else {
			    while (d < PL_bufend && isSPACE(*d))
				d++;
			    if (*d == '=' && d[1] == '>')
				probable_sub = 1;
			}
		    }
		    if (probable_sub) {
			gv = gv_fetchpv(PL_tokenbuf, GV_ADD, SVt_PVCV);
			op_free(pl_yylval.opval);
			pl_yylval.opval = rv2cv_op;
			pl_yylval.opval->op_private |= OPpENTERSUB_NOPAREN;
			PL_last_lop = PL_oldbufptr;
			PL_last_lop_op = OP_ENTERSUB;
			PL_nextwhite = PL_thiswhite;
			PL_thiswhite = 0;
			start_force(PL_curforce);
			NEXTVAL_NEXTTOKE.opval = pl_yylval.opval;
			PL_expect = XTERM;
			PL_nextwhite = nextPL_nextwhite;
			curmad('X', PL_thistoken);
			PL_thistoken = newSVpvs("");
			force_next(WORD);
			TOKEN(NOAMP);
		    }
#else
		    NEXTVAL_NEXTTOKE.opval = pl_yylval.opval;
		    PL_expect = XTERM;
		    force_next(WORD);
		    TOKEN(NOAMP);
#endif
		}

		/* Call it a bare word */

		if (PL_hints & HINT_STRICT_SUBS)
		    pl_yylval.opval->op_private |= OPpCONST_STRICT;
		else {
		bareword:
		    /* after "print" and similar functions (corresponding to
		     * "F? L" in opcode.pl), whatever wasn't already parsed as
		     * a filehandle should be subject to "strict subs".
		     * Likewise for the optional indirect-object argument to system
		     * or exec, which can't be a bareword */
		    if ((PL_last_lop_op == OP_PRINT
			    || PL_last_lop_op == OP_PRTF
			    || PL_last_lop_op == OP_SAY
			    || PL_last_lop_op == OP_SYSTEM
			    || PL_last_lop_op == OP_EXEC)
			    && (PL_hints & HINT_STRICT_SUBS))
			pl_yylval.opval->op_private |= OPpCONST_STRICT;
		    if (lastchar != '-') {
			if (ckWARN(WARN_RESERVED)) {
			    d = PL_tokenbuf;
			    while (isLOWER(*d))
				d++;
			    if (!*d && !gv_stashpv(PL_tokenbuf, 0))
				Perl_warner(aTHX_ packWARN(WARN_RESERVED), PL_warn_reserved,
				       PL_tokenbuf);
			}
		    }
		}
		op_free(rv2cv_op);

	    safe_bareword:
		if ((lastchar == '*' || lastchar == '%' || lastchar == '&')) {
		    Perl_ck_warner_d(aTHX_ packWARN(WARN_AMBIGUOUS),
				     "Operator or semicolon missing before %c%s",
				     lastchar, PL_tokenbuf);
		    Perl_ck_warner_d(aTHX_ packWARN(WARN_AMBIGUOUS),
				     "Ambiguous use of %c resolved as operator %c",
				     lastchar, lastchar);
		}
		TOKEN(WORD);
	    }

	case KEY___FILE__:
	    pl_yylval.opval = (OP*)newSVOP(OP_CONST, 0,
					newSVpv(CopFILE(PL_curcop),0));
	    TERM(THING);

	case KEY___LINE__:
            pl_yylval.opval = (OP*)newSVOP(OP_CONST, 0,
                                    Perl_newSVpvf(aTHX_ "%"IVdf, (IV)CopLINE(PL_curcop)));
	    TERM(THING);

	case KEY___PACKAGE__:
	    pl_yylval.opval = (OP*)newSVOP(OP_CONST, 0,
					(PL_curstash
					 ? newSVhek(HvNAME_HEK(PL_curstash))
					 : &PL_sv_undef));
	    TERM(THING);

	case KEY___DATA__:
	case KEY___END__: {
	    GV *gv;
	    if (PL_rsfp && (!PL_in_eval || PL_tokenbuf[2] == 'D')) {
		const char *pname = "main";
		if (PL_tokenbuf[2] == 'D')
		    pname = HvNAME_get(PL_curstash ? PL_curstash : PL_defstash);
		gv = gv_fetchpv(Perl_form(aTHX_ "%s::DATA", pname), GV_ADD,
				SVt_PVIO);
		GvMULTI_on(gv);
		if (!GvIO(gv))
		    GvIOp(gv) = newIO();
		IoIFP(GvIOp(gv)) = PL_rsfp;
#if defined(HAS_FCNTL) && defined(F_SETFD)
		{
		    const int fd = PerlIO_fileno(PL_rsfp);
		    fcntl(fd,F_SETFD,fd >= 3);
		}
#endif
		/* Mark this internal pseudo-handle as clean */
		IoFLAGS(GvIOp(gv)) |= IOf_UNTAINT;
		if ((PerlIO*)PL_rsfp == PerlIO_stdin())
		    IoTYPE(GvIOp(gv)) = IoTYPE_STD;
		else
		    IoTYPE(GvIOp(gv)) = IoTYPE_RDONLY;
#if defined(WIN32) && !defined(PERL_TEXTMODE_SCRIPTS)
		/* if the script was opened in binmode, we need to revert
		 * it to text mode for compatibility; but only iff it has CRs
		 * XXX this is a questionable hack at best. */
		if (PL_bufend-PL_bufptr > 2
		    && PL_bufend[-1] == '\n' && PL_bufend[-2] == '\r')
		{
		    Off_t loc = 0;
		    if (IoTYPE(GvIOp(gv)) == IoTYPE_RDONLY) {
			loc = PerlIO_tell(PL_rsfp);
			(void)PerlIO_seek(PL_rsfp, 0L, 0);
		    }
#ifdef NETWARE
			if (PerlLIO_setmode(PL_rsfp, O_TEXT) != -1) {
#else
		    if (PerlLIO_setmode(PerlIO_fileno(PL_rsfp), O_TEXT) != -1) {
#endif	/* NETWARE */
#ifdef PERLIO_IS_STDIO /* really? */
#  if defined(__BORLANDC__)
			/* XXX see note in do_binmode() */
			((FILE*)PL_rsfp)->flags &= ~_F_BIN;
#  endif
#endif
			if (loc > 0)
			    PerlIO_seek(PL_rsfp, loc, 0);
		    }
		}
#endif
#ifdef PERLIO_LAYERS
		if (!IN_BYTES) {
		    if (UTF)
			PerlIO_apply_layers(aTHX_ PL_rsfp, NULL, ":utf8");
		    else if (PL_encoding) {
			SV *name;
			dSP;
			ENTER;
			SAVETMPS;
			PUSHMARK(sp);
			EXTEND(SP, 1);
			XPUSHs(PL_encoding);
			PUTBACK;
			call_method("name", G_SCALAR);
			SPAGAIN;
			name = POPs;
			PUTBACK;
			PerlIO_apply_layers(aTHX_ PL_rsfp, NULL,
					    Perl_form(aTHX_ ":encoding(%"SVf")",
						      SVfARG(name)));
			FREETMPS;
			LEAVE;
		    }
		}
#endif
#ifdef PERL_MAD
		if (PL_madskills) {
		    if (PL_realtokenstart >= 0) {
			char *tstart = SvPVX(PL_linestr) + PL_realtokenstart;
			if (!PL_endwhite)
			    PL_endwhite = newSVpvs("");
			sv_catsv(PL_endwhite, PL_thiswhite);
			PL_thiswhite = 0;
			sv_catpvn(PL_endwhite, tstart, PL_bufend - tstart);
			PL_realtokenstart = -1;
		    }
		    while ((s = filter_gets(PL_endwhite, SvCUR(PL_endwhite)))
			   != NULL) ;
		}
#endif
		PL_rsfp = NULL;
	    }
	    goto fake_eof;
	}

	case KEY_AUTOLOAD:
	case KEY_DESTROY:
	case KEY_BEGIN:
	case KEY_UNITCHECK:
	case KEY_CHECK:
	case KEY_INIT:
	case KEY_END:
	    if (PL_expect == XSTATE) {
		s = PL_bufptr;
		goto really_sub;
	    }
	    goto just_a_word;

	case KEY_CORE:
	    if (*s == ':' && s[1] == ':') {
		s += 2;
		d = s;
		s = scan_word(s, PL_tokenbuf, sizeof PL_tokenbuf, FALSE, &len);
		if (!(tmp = keyword(PL_tokenbuf, len, 0)))
		    Perl_croak(aTHX_ "CORE::%s is not a keyword", PL_tokenbuf);
		if (tmp < 0)
		    tmp = -tmp;
		else if (tmp == KEY_require || tmp == KEY_do)
		    /* that's a way to remember we saw "CORE::" */
		    orig_keyword = tmp;
		goto reserved_word;
	    }
	    goto just_a_word;

	case KEY_abs:
	    UNI(OP_ABS);

	case KEY_alarm:
	    UNI(OP_ALARM);

	case KEY_accept:
	    LOP(OP_ACCEPT,XTERM);

	case KEY_and:
	    OPERATOR(ANDOP);

	case KEY_atan2:
	    LOP(OP_ATAN2,XTERM);

	case KEY_bind:
	    LOP(OP_BIND,XTERM);

	case KEY_binmode:
	    LOP(OP_BINMODE,XTERM);

	case KEY_bless:
	    LOP(OP_BLESS,XTERM);

	case KEY_break:
	    FUN0(OP_BREAK);

	case KEY_chop:
	    UNI(OP_CHOP);

	case KEY_continue:
	    /* When 'use switch' is in effect, continue has a dual
	       life as a control operator. */
	    {
		if (!FEATURE_IS_ENABLED("switch"))
		    PREBLOCK(CONTINUE);
		else {
		    /* We have to disambiguate the two senses of
		      "continue". If the next token is a '{' then
		      treat it as the start of a continue block;
		      otherwise treat it as a control operator.
		     */
		    s = skipspace(s);
		    if (*s == '{')
	    PREBLOCK(CONTINUE);
		    else
			FUN0(OP_CONTINUE);
		}
	    }

	case KEY_chdir:
	    /* may use HOME */
	    (void)gv_fetchpvs("ENV", GV_ADD|GV_NOTQUAL, SVt_PVHV);
	    UNI(OP_CHDIR);

	case KEY_close:
	    UNI(OP_CLOSE);

	case KEY_closedir:
	    UNI(OP_CLOSEDIR);

	case KEY_cmp:
	    Eop(OP_SCMP);

	case KEY_caller:
	    UNI(OP_CALLER);

	case KEY_crypt:
#ifdef FCRYPT
	    if (!PL_cryptseen) {
		PL_cryptseen = TRUE;
		init_des();
	    }
#endif
	    LOP(OP_CRYPT,XTERM);

	case KEY_chmod:
	    LOP(OP_CHMOD,XTERM);

	case KEY_chown:
	    LOP(OP_CHOWN,XTERM);

	case KEY_connect:
	    LOP(OP_CONNECT,XTERM);

	case KEY_chr:
	    UNI(OP_CHR);

	case KEY_cos:
	    UNI(OP_COS);

	case KEY_chroot:
	    UNI(OP_CHROOT);

	case KEY_default:
	    PREBLOCK(DEFAULT);

	case KEY_do:
	    s = SKIPSPACE1(s);
	    if (*s == '{')
		PRETERMBLOCK(DO);
	    if (*s != '\'')
		s = force_word(s,WORD,TRUE,TRUE,FALSE);
	    if (orig_keyword == KEY_do) {
		orig_keyword = 0;
		pl_yylval.ival = 1;
	    }
	    else
		pl_yylval.ival = 0;
	    OPERATOR(DO);

	case KEY_die:
	    PL_hints |= HINT_BLOCK_SCOPE;
	    LOP(OP_DIE,XTERM);

	case KEY_defined:
	    UNI(OP_DEFINED);

	case KEY_delete:
	    UNI(OP_DELETE);

	case KEY_dbmopen:
	    gv_fetchpvs("AnyDBM_File::ISA", GV_ADDMULTI, SVt_PVAV);
	    LOP(OP_DBMOPEN,XTERM);

	case KEY_dbmclose:
	    UNI(OP_DBMCLOSE);

	case KEY_dump:
	    s = force_word(s,WORD,TRUE,FALSE,FALSE);
	    LOOPX(OP_DUMP);

	case KEY_else:
	    PREBLOCK(ELSE);

	case KEY_elsif:
	    pl_yylval.ival = CopLINE(PL_curcop);
	    OPERATOR(ELSIF);

	case KEY_eq:
	    Eop(OP_SEQ);

	case KEY_exists:
	    UNI(OP_EXISTS);
	
	case KEY_exit:
	    if (PL_madskills)
		UNI(OP_INT);
	    UNI(OP_EXIT);

	case KEY_eval:
	    s = SKIPSPACE1(s);
	    if (*s == '{') { /* block eval */
		PL_expect = XTERMBLOCK;
		UNIBRACK(OP_ENTERTRY);
	    }
	    else { /* string eval */
		PL_expect = XTERM;
		UNIBRACK(OP_ENTEREVAL);
	    }

	case KEY_eof:
	    UNI(OP_EOF);

	case KEY_exp:
	    UNI(OP_EXP);

	case KEY_each:
	    UNI(OP_EACH);

	case KEY_exec:
	    LOP(OP_EXEC,XREF);

	case KEY_endhostent:
	    FUN0(OP_EHOSTENT);

	case KEY_endnetent:
	    FUN0(OP_ENETENT);

	case KEY_endservent:
	    FUN0(OP_ESERVENT);

	case KEY_endprotoent:
	    FUN0(OP_EPROTOENT);

	case KEY_endpwent:
	    FUN0(OP_EPWENT);

	case KEY_endgrent:
	    FUN0(OP_EGRENT);

	case KEY_for:
	case KEY_foreach:
	    pl_yylval.ival = CopLINE(PL_curcop);
	    s = SKIPSPACE1(s);
	    if (PL_expect == XSTATE && isIDFIRST_lazy_if(s,UTF)) {
		char *p = s;
#ifdef PERL_MAD
		int soff = s - SvPVX(PL_linestr); /* for skipspace realloc */
#endif

		if ((PL_bufend - p) >= 3 &&
		    strnEQ(p, "my", 2) && isSPACE(*(p + 2)))
		    p += 2;
		else if ((PL_bufend - p) >= 4 &&
		    strnEQ(p, "our", 3) && isSPACE(*(p + 3)))
		    p += 3;
		p = PEEKSPACE(p);
		if (isIDFIRST_lazy_if(p,UTF)) {
		    p = scan_ident(p, PL_bufend,
			PL_tokenbuf, sizeof PL_tokenbuf, TRUE);
		    p = PEEKSPACE(p);
		}
		if (*p != '$')
		    Perl_croak(aTHX_ "Missing $ on loop variable");
#ifdef PERL_MAD
		s = SvPVX(PL_linestr) + soff;
#endif
	    }
	    OPERATOR(FOR);

	case KEY_formline:
	    LOP(OP_FORMLINE,XTERM);

	case KEY_fork:
	    FUN0(OP_FORK);

	case KEY_fcntl:
	    LOP(OP_FCNTL,XTERM);

	case KEY_fileno:
	    UNI(OP_FILENO);

	case KEY_flock:
	    LOP(OP_FLOCK,XTERM);

	case KEY_gt:
	    Rop(OP_SGT);

	case KEY_ge:
	    Rop(OP_SGE);

	case KEY_grep:
	    LOP(OP_GREPSTART, XREF);

	case KEY_goto:
	    s = force_word(s,WORD,TRUE,FALSE,FALSE);
	    LOOPX(OP_GOTO);

	case KEY_gmtime:
	    UNI(OP_GMTIME);

	case KEY_getc:
	    UNIDOR(OP_GETC);

	case KEY_getppid:
	    FUN0(OP_GETPPID);

	case KEY_getpgrp:
	    UNI(OP_GETPGRP);

	case KEY_getpriority:
	    LOP(OP_GETPRIORITY,XTERM);

	case KEY_getprotobyname:
	    UNI(OP_GPBYNAME);

	case KEY_getprotobynumber:
	    LOP(OP_GPBYNUMBER,XTERM);

	case KEY_getprotoent:
	    FUN0(OP_GPROTOENT);

	case KEY_getpwent:
	    FUN0(OP_GPWENT);

	case KEY_getpwnam:
	    UNI(OP_GPWNAM);

	case KEY_getpwuid:
	    UNI(OP_GPWUID);

	case KEY_getpeername:
	    UNI(OP_GETPEERNAME);

	case KEY_gethostbyname:
	    UNI(OP_GHBYNAME);

	case KEY_gethostbyaddr:
	    LOP(OP_GHBYADDR,XTERM);

	case KEY_gethostent:
	    FUN0(OP_GHOSTENT);

	case KEY_getnetbyname:
	    UNI(OP_GNBYNAME);

	case KEY_getnetbyaddr:
	    LOP(OP_GNBYADDR,XTERM);

	case KEY_getnetent:
	    FUN0(OP_GNETENT);

	case KEY_getservbyname:
	    LOP(OP_GSBYNAME,XTERM);

	case KEY_getservbyport:
	    LOP(OP_GSBYPORT,XTERM);

	case KEY_getservent:
	    FUN0(OP_GSERVENT);

	case KEY_getsockname:
	    UNI(OP_GETSOCKNAME);

	case KEY_getsockopt:
	    LOP(OP_GSOCKOPT,XTERM);

	case KEY_getgrent:
	    FUN0(OP_GGRENT);

	case KEY_getgrnam:
	    UNI(OP_GGRNAM);

	case KEY_getgrgid:
	    UNI(OP_GGRGID);

	case KEY_getlogin:
	    FUN0(OP_GETLOGIN);

	case KEY_given:
	    pl_yylval.ival = CopLINE(PL_curcop);
	    OPERATOR(GIVEN);

	case KEY_glob:
	    LOP(OP_GLOB,XTERM);

	case KEY_hex:
	    UNI(OP_HEX);

	case KEY_if:
	    pl_yylval.ival = CopLINE(PL_curcop);
	    OPERATOR(IF);

	case KEY_index:
	    LOP(OP_INDEX,XTERM);

	case KEY_int:
	    UNI(OP_INT);

	case KEY_ioctl:
	    LOP(OP_IOCTL,XTERM);

	case KEY_join:
	    LOP(OP_JOIN,XTERM);

	case KEY_keys:
	    UNI(OP_KEYS);

	case KEY_kill:
	    LOP(OP_KILL,XTERM);

	case KEY_last:
	    s = force_word(s,WORD,TRUE,FALSE,FALSE);
	    LOOPX(OP_LAST);
	
	case KEY_lc:
	    UNI(OP_LC);

	case KEY_lcfirst:
	    UNI(OP_LCFIRST);

	case KEY_local:
	    pl_yylval.ival = 0;
	    OPERATOR(LOCAL);

	case KEY_length:
	    UNI(OP_LENGTH);

	case KEY_lt:
	    Rop(OP_SLT);

	case KEY_le:
	    Rop(OP_SLE);

	case KEY_localtime:
	    UNI(OP_LOCALTIME);

	case KEY_log:
	    UNI(OP_LOG);

	case KEY_link:
	    LOP(OP_LINK,XTERM);

	case KEY_listen:
	    LOP(OP_LISTEN,XTERM);

	case KEY_lock:
	    UNI(OP_LOCK);

	case KEY_lstat:
	    UNI(OP_LSTAT);

	case KEY_m:
	    s = scan_pat(s,OP_MATCH);
	    TERM(sublex_start());

	case KEY_map:
	    LOP(OP_MAPSTART, XREF);

	case KEY_mkdir:
	    LOP(OP_MKDIR,XTERM);

	case KEY_msgctl:
	    LOP(OP_MSGCTL,XTERM);

	case KEY_msgget:
	    LOP(OP_MSGGET,XTERM);

	case KEY_msgrcv:
	    LOP(OP_MSGRCV,XTERM);

	case KEY_msgsnd:
	    LOP(OP_MSGSND,XTERM);

	case KEY_our:
	case KEY_my:
	case KEY_state:
	    PL_in_my = (U16)tmp;
	    s = SKIPSPACE1(s);
	    if (isIDFIRST_lazy_if(s,UTF)) {
#ifdef PERL_MAD
		char* start = s;
#endif
		s = scan_word(s, PL_tokenbuf, sizeof PL_tokenbuf, TRUE, &len);
		if (len == 3 && strnEQ(PL_tokenbuf, "sub", 3))
		    goto really_sub;
		PL_in_my_stash = find_in_my_stash(PL_tokenbuf, len);
		if (!PL_in_my_stash) {
		    char tmpbuf[1024];
		    PL_bufptr = s;
		    my_snprintf(tmpbuf, sizeof(tmpbuf), "No such class %.1000s", PL_tokenbuf);
		    yyerror(tmpbuf);
		}
#ifdef PERL_MAD
		if (PL_madskills) {	/* just add type to declarator token */
		    sv_catsv(PL_this