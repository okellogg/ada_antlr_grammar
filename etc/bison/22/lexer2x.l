%{
/******* A "lex"-style lexer for Ada 9X ****************************/
/* Copyright (C) Intermetrics, Inc. 1994 Cambridge, MA  USA        */
/* Copying permitted if accompanied by this statement.             */
/* Derivative works are permitted if accompanied by this statement.*/
/* This lexer is known to be only approximately correct, but it is */
/* more than adequate for most uses (the lexing of apostrophe is   */
/* not as sophisticated as it needs to be to be "perfect").        */
/* As usual there is *no warranty* but we hope it is useful.       */
/*******************************************************************/
/*                                                                 */
/* 2022/08/27 Added Ada22 keywords (okellogg)                      */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include "grammar2x.tab.h"

  int soft_keywords = 0;

  int error_count;

  int lk_keyword(char *str);

  bool definable_operator (const char *string) { // operator_symbol sans "/="
    static const char *ops[] = {
                          "and", "or", "xor",           // logical
                          "=", "<", "<=", ">", ">=",    // relational (omitting "/=")
                          "+", "-",    "&",             // binary/unary adding
                          "*", "/", "mod", "rem",       // multiplying
                          "**", "abs", "not"            // highest precedence
                        };
    char *str;
    if (strlen(string) < 3 || string[0] != '"')
      return false;
    str = strdup(string + 1);
    for (int i = 0; i < sizeof(ops) / sizeof(char*); i++)
    {
      if (strcasecmp(str, ops[i]) == 0)
        return true;
    }
    return false;
  }
  bool equiv(const char *s1, const char *s2) {
    return strcasecmp(s1, s2) == 0;
  }
  bool is_operator_symbol (const char *string) {
    return definable_operator(string) || equiv(string, "\"/=\"");
  }
%}

DIGIT                   [0-9]
EXTENDED_DIGIT          [0-9a-zA-Z]
INTEGER                 ({DIGIT}(_?{DIGIT})*)
EXPONENT                ([eE](\+?|-){INTEGER})
DECIMAL_LITERAL         {INTEGER}(\.?{INTEGER})?{EXPONENT}?
BASE                    {INTEGER}
BASED_INTEGER           {EXTENDED_DIGIT}(_?{EXTENDED_DIGIT})*
BASED_LITERAL           {BASE}#{BASED_INTEGER}(\.{BASED_INTEGER})?#{EXPONENT}?

%option yylineno

%%
"."                     return('.');
"<"                     return('<');
"("                     return('(');
"+"                     return('+');
"|"                     return('|');
"&"                     return('&');
"*"                     return('*');
")"                     return(')');
";"                     return(';');
"-"                     return('-');
"/"                     return('/');
","                     return(',');
">"                     return('>');
":"                     return(':');
"="                     return('=');
"'"                     return(TIC);
".."                    return(DOT_DOT);
"<<"                    return(LT_LT);
"<>"                    return(BOX);
"<="                    return(LT_EQ);
"**"                    return(EXPON);
"/="                    return(NE);
">>"                    return(GT_GT);
">="                    return(GE);
":="                    return(IS_ASSIGNED);
"=>"                    return(RIGHT_SHAFT);
[a-zA-Z](_?[a-zA-Z0-9])* return(lk_keyword(yytext));
"'"."'"                 return(char_lit);
\"(\"\"|[^\n\"])*\"     return(string_lit);
{DECIMAL_LITERAL}       return(numeric_lit);
{BASED_LITERAL}         return(numeric_lit);
--.*\n                  ;
[ \t\f\n]               ;
.                      {fprintf(stderr, "  Illegal character:%c: on line %d\n",
                            *yytext, yylineno);
                         error_count++;}
%%

/* To build this, run it through lex, compile it, and link it with */
/* the result of yacc'ing and cc'ing grammar9x.y, plus "-ly"       */

int main(argc, argv)
   int argc;
   char *argv[];
{
    /* Simple Ada syntax checker */
    /* Checks standard input if no arguments */
    /* Checks files if one or more arguments */

    extern int error_count;
    extern int yyparse();
    extern int yylineno;
    extern int yydebug;
    FILE *flptr;
    int i = 1;

    if (argc == 1) {
	yylineno = 1;
	yyparse();
    } else {
        if (strcmp(argv[i], "-d") == 0) {
	    yydebug = 1;
	    i++;
	}
	for (; i < argc; i++) {
	    if ((flptr = freopen(argv[i], "r",stdin)) == NULL) {
		fprintf(stderr, "%s:  Can't open %s", argv[0], argv[i]);
	    } else {
		if (argc > 2) fprintf(stderr, "%s:\n", argv[i]);
		yylineno = 1;
		yyparse();
	    }
	}
    }
    if (error_count) {
	fprintf(stderr, "%d syntax error%s detected\n", error_count,
	  error_count == 1? "": "s");
	return 1;
    } else {
	fprintf(stderr, "No syntax errors detected\n");
    }
    return 0;
}

int yywrap() {return 1;}

/*
 * Keywords stored in alpha order
 */

typedef struct
        {
        char    * kw;
        int     kwv;
        } KEY_TABLE;

/*
 *      Reserved keyword list and Token values
 *      as defined in y.tab.h
 */

# define NUM_KEYWORDS  74
# define SOFT_KEYWORDS  6

KEY_TABLE key_tab[NUM_KEYWORDS] = {
                {"ABORT",       ABORT},
                {"ABS",         ABS},
                {"ABSTRACT",    ABSTRACT},
                {"ACCEPT",      ACCEPT},
                {"ACCESS",      ACCESS},
                {"ALIASED",     ALIASED},
                {"ALL",         ALL},
                {"AND",         AND},
                {"ARRAY",       ARRAY},
                {"AT",          AT},
                {"BEGIN",       BEGiN},
                {"BODY",        BODY},
                {"CASE",        CASE},
                {"CONSTANT",    CONSTANT},
                {"DECLARE",     DECLARE},
                {"DELAY",       DELAY},
                {"DELTA",       DELTA},
                {"DIGITS",      DIGITS},
                {"DO",          DO},
                {"ELSE",        ELSE},
                {"ELSIF",       ELSIF},
                {"END",         END},
                {"ENTRY",       ENTRY},
                {"EXCEPTION",   EXCEPTION},
                {"EXIT",        EXIT},
                {"FOR",         FOR},
                {"FUNCTION",    FUNCTION},
                {"GENERIC",     GENERIC},
                {"GOTO",        GOTO},
                {"IF",          IF},
                {"IN",          IN},
                {"INTERFACE",   INTERFACE},
                {"IS",          IS},
                {"LIMITED",     LIMITED},
                {"LOOP",        LOOP},
                {"MOD",         MOD},
                {"NEW",         NEW},
                {"NOT",         NOT},
                {"NULL",        NuLL},
                {"OF",          OF},
                {"OR",          OR},
                {"OTHERS",      OTHERS},
                {"OUT",         OUT},
                {"OVERRIDING",  OVERRIDING},
                {"PACKAGE",     PACKAGE},
                {"PARALLEL",    PARALLEL},
                {"PRAGMA",      PRAGMA},
                {"PRIVATE",     PRIVATE},
                {"PROCEDURE",   PROCEDURE},
                {"PROTECTED",   PROTECTED},
                {"RAISE",       RAISE},
                {"RANGE",       RANGE},
                {"RECORD",      RECORD},
                {"REM",         REM},
                {"RENAMES",     RENAMES},
                {"REQUEUE",     REQUEUE},
                {"RETURN",      RETURN},
                {"REVERSE",     REVERSE},
                {"SELECT",      SELECT},
                {"SEPARATE",    SEPARATE},
                {"SOME",        SOME},
                {"SUBTYPE",     SUBTYPE},
                {"SYNCHRONIZED",SYNCHRONIZED},
                {"TAGGED",      TAGGED},
                {"TASK",        TASK},
                {"TERMINATE",   TERMINATE},
                {"THEN",        THEN},
                {"TYPE",        TYPE},
                {"UNTIL",       UNTIL},
                {"USE",         USE},
                {"WHEN",        WHEN},
                {"WHILE",       WHILE},
                {"WITH",        WITH},
                {"XOR",         XOR}
                };

KEY_TABLE softkey_tab[SOFT_KEYWORDS] = {
     { "UNSPECIFIED",            UNSPECIFIED },
     { "SYNCHRONIZATION",        SYNCHRONIZATION },
     { "BY_ENTRY",               BY_ENTRY },
     { "BY_PROTECTED_PROCEDURE", BY_PROTECTED_PROCEDURE },
     { "OPTIONAL",               OPTIONAL },
     { "CLASS",                  CLASS }
  };

void
to_upper(char *str)
{
        char * cp;
        for (cp=str; *cp; cp++) {
                if (islower(*cp)) *cp -= ('a' - 'A') ;
	}
}

int
lk_keyword(char *str)
{
        int min;
        int max;
        int guess, compare;

        min = 0;
        max = NUM_KEYWORDS-1;
        guess = (min + max) / 2;
        to_upper(str);

        for (guess=(min+max)/2; min<=max; guess=(min+max)/2) {
                if ((compare = strcmp(key_tab[guess].kw, str)) < 0) {
                        min = guess + 1;
                } else if (compare > 0) {
                        max = guess - 1;
                } else {
                        return key_tab[guess].kwv;
		}
	}
        if (soft_keywords) {
                for (guess = 0; guess < SOFT_KEYWORDS; guess++) {
                        if (strcmp(softkey_tab[guess].kw, str) == 0)
                                return softkey_tab[guess].kwv;
                }
        }
        return identifier;
 }

void
yyerror(const char *s)
{
        extern int yychar;

        error_count++;

        fprintf(stderr,"  %s", s);
        if (yylineno)
                fprintf(stderr,", on line %d,", yylineno);
        fprintf(stderr," on input: ");
        if (yychar >= 0400) {
                if ((yychar >= ABORT) && (yychar <= XOR)) {
                        fprintf(stderr, "(token) %s #%d\n",
                            key_tab[yychar-ABORT].kw, yychar);
                } else if (yychar >= UNSPECIFIED && yychar <= CLASS) {
                        fprintf(stderr, "(softtoken) %s #%d\n",
                            key_tab[yychar-UNSPECIFIED].kw, yychar);
                } else switch (yychar) {
                        case char_lit : fprintf(stderr, "character literal\n");
                                break;
                        case identifier : fprintf(stderr, "identifier\n");
                                break;
                        case string_lit : fprintf(stderr, "string\n");
                                break;
                        case numeric_lit : fprintf(stderr, "numeric literal\n");
                                break;
			case TIC : fprintf(stderr, "single-quote\n");
				break;
			case DOT_DOT : fprintf(stderr, "..\n");
				break;
			case LT_LT : fprintf(stderr, "<<\n");
				break;
			case BOX : fprintf(stderr, "<>\n");
				break;
			case LT_EQ : fprintf(stderr, "<=\n");
				break;
			case EXPON : fprintf(stderr, "**\n");
				break;
			case NE : fprintf(stderr, "/=\n");
				break;
			case GT_GT : fprintf(stderr, ">>\n");
				break;
			case GE : fprintf(stderr, ">=\n");
				break;
			case IS_ASSIGNED : fprintf(stderr, ":=\n");
				break;
			case RIGHT_SHAFT : fprintf(stderr, "=>\n");
				break;
			default :
                                fprintf(stderr, "(token) %d\n", yychar);
                }
        } else {
                switch (yychar) {
                case '\t': fprintf(stderr,"horizontal-tab\n"); return;
                case '\n': fprintf(stderr,"newline\n"); return;
                case '\0': fprintf(stderr,"$end\n"); return;
                case ' ': fprintf(stderr, "(blank)"); return;
                default :
                        if (isprint(yychar))
                                fprintf(stderr,"(char) %c\n", yychar);
                        else
                                fprintf(stderr,"(int) %d\n", yychar);
                }
	}
}
