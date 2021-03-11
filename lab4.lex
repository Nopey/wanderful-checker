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
#include "shareddefs.h"
#include "y.tab.h"
extern YYSTYPE yylval;
int yywrap();
int yyerror(char* s);
int col=0;
int row=0;
%}

%%

 /* ---- part 2: token rules ---- */

 /* Literals */
\'([^\'\n]*)\' {
   // String Literal
   int len = strlen(yytext);
   if (len >= MaxNameLen)
   {
      // Because we reuse the info.name buffer, MaxNameLen is the limit for strings as well.
      yyerror("String exceeds max length.");
      printf("   offending string was: %s, limit is %d.\n", yytext, MaxNameLen-1);
  }
  col += len;
  strcpy(yylval.info.name, yytext);
  return(L_STRING);
}
"north"   { col+=5; yylval.info.type = T_FACING; yylval.info.dir = DIR_N; return(L_DIR); }
"south"   { col+=5; yylval.info.type = T_FACING; yylval.info.dir = DIR_S; return(L_DIR); }
"west"    { col+=4; yylval.info.type = T_FACING; yylval.info.dir = DIR_W; return(L_DIR); }
"east"    { col+=4; yylval.info.type = T_FACING; yylval.info.dir = DIR_E; return(L_DIR); }
({Digit})+     { yylval.info.number = atol(yytext);
                 yylval.info.type = T_INT;
                 col+=strlen(yytext);
                 return(L_NUMBER); }

 /* Symbols */
";"   { col+=1; return(SEMI); }
"<"   { col+=1; yylval.info.comp = COMP_LT;   return(COMPOP); }
"<="  { col+=2; yylval.info.comp = COMP_LTEQ; return(COMPOP); }
"="   { col+=1; yylval.info.comp = COMP_EQ;   return(COMPOP); }
"><"  { col+=2; yylval.info.comp = COMP_NE;   return(COMPOP); }
"("   { col+=1; return(LBRACKET); }
")"   { col+=1; return(RBRACKET); }
":="  { col+=2; return(ASSIGNOP); }
":"   { col+=1; return(COLON); }
","   { col+=1; return(COMMA); }
"+"   { col+=1; yylval.info.arth = ARTH_ADD; return(ADDSUB); }
"-"   { col+=1; yylval.info.arth = ARTH_SUB; return(ADDSUB); }

 /* Datatypes */
"int"     { col+=3; yylval.info.type = T_INT;    return(TYPENAME); }
"name"    { col+=4; yylval.info.type = T_NAME;   return(TYPENAME); }
"str"     { col+=3; yylval.info.type = T_STR;    return(TYPENAME); }
"facing"  { col+=6; yylval.info.type = T_FACING; return(TYPENAME); }
"none"    { col+=4; yylval.info.type = T_NONE;   return(TYPENAME); }

 /* program */
"mapsize" { col+=7; return(MAPSIZE); }
"start"   { col+=5; return(START); }
"finish"  { col+=6; return(FINISH); }

 /* program */
"function" { col+=8; return(FUNCTION); }
"begin"    { col+=5; return(FUNC_BEGIN); }

 /* action */
"create"  { col+=6; return(CREATE); }
"move"    { col+=4; return(MOVE); }
"turn"    { col+=4; return(TURN); }
"print"   { col+=5; return(PRINT); }
"return"  { col+=6; return(RETURN); }

 /* loop */
"repeat"  { col+=6; return(REPEAT); }
"until"   { col+=5; return(UNTIL); }

 /* select */
"if"      { col+=2; return(IF); }
"then"    { col+=4; return(THEN); }
"end"     { col+=3; return(END); }

 /* builtin */
"row"     { col+=3; yylval.info.builtin = B_ROW;  return(BUILTIN); }
"col"     { col+=3; yylval.info.builtin = B_COL;  return(BUILTIN); }
"face"    { col+=4; yylval.info.builtin = B_FACE; return(BUILTIN); }

 /* Names */
([A-Z][a-z]+) {
   int len = strlen(yytext);
   if (len >= MaxNameLen)
   {
      yyerror("Name exceeds max name length.");
      printf("   offending name was: %s, limit is %d.\n", yytext, MaxNameLen-1);
  }
  col += len;
  strcpy(yylval.info.name, yytext);
  return(BOT_NAME);
}
([a-z][a-z0-9]*) {
   int len = strlen(yytext);
   if (len >= MaxNameLen)
   {
      yyerror("Name exceeds max name length.");
      printf("   offending name was: %s, limit is %d.\n", yytext, MaxNameLen-1);
  }
  col += len;
  strcpy(yylval.info.name, yytext);
  return(VAR_NAME);
}

 /* identify any characters that are just to be skipped, e.g. whitespace */
[ \t\f\v]  { col++; }
 /* comments */
(%[^\n]*)     { col+=strlen(yytext); }

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
   // Printing these +1 so they align with text editor's counting
   fprintf(stderr, "\n***Error detected: %s\n   on/after %d:%d.\n\n", s, row+1, col+1);
   ErrorLevel = 1;
   return 1;
}

