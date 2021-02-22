 /*
 */

 /* ---- part 1: declarations ---- */

 /* C setup code: libraries, prototypes, etc */
%{
#include<stdio.h>
#include<string.h>
int yylex(void);
int yywrap();
int yyerror(char* s);
extern int row;
extern int col;
%}

 /* begin processing the top-level component */
%start program

%union { long info; }

 /* identify the valid token types, all have yylval type long */
%token<long> NUMBER START FINISH
%type<long> program statement

 /* ---- part 2: grammar rules ----
  */

%%

program: START statement FINISH
   ;

statement: NUMBER
   ;

 /* ---- part 3: supporting programs ---- */

%%

 /* begin parsing */
int main() {
   printf("Compilation begins:\n\n");
   int res = yyparse();
   printf("\nCompilation complete.\n", res);
   return(res);
}

