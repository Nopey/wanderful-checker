 /* starter lex file for Wanderful recognizer:
  *     currently what it actually (incorrectly) recognizes as a Wanderful program is
  *     anything of the form "start NUMBER finish" where NUMBER is a positive integer
  */

 /* ---- part 1: declarations ---- */

 /* part 1a: any character sets we want to identify */
Digit [0-9]

 /* part 1b: the C setup */
%{
#include<stdio.h>
#include "y.tab.h"
extern YYSTYPE yylval;
int yywrap();
int yyerror(char* s);
int col=0;
int row=0;
%}

%%

 /* ---- part 2: token rules ---- */

({Digit})+ { col+=strlen(yytext); return(NUMBER); }

"start"    { col+=5; return(START); }
"finish"    { col+=6; return(FINISH); }

 /* identify any characters that are just to be skipped, e.g. whitespace */
[ \t\f\v]  { col++; }

 /* adjust row/column after a newline */
([\n]) { row++; col=0; }

 /* anything else is an error, return it as a token so the yacc rules can reject it */
.          { char errmsg[] = "Unknown char in input: x";
             errmsg[23] = yytext[0];
             yyerror(errmsg);
             return(yytext[0]); }

%%

 /* ---- part 3: supporting code ---- */

 /* cleanup any loose ends at the end of input */
int yywrap()
{
   return(1);
}

 /* process any error messages generated */
int yyerror(char* s)
{
   fprintf(stderr, "\n***Error detected: %s\n   on/after row %d, col %d.\n\n", s, row, col);
   return 1;
}

