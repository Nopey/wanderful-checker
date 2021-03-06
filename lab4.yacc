 /*
 */

 /* ---- part 1: declarations ---- */

 /* C setup code: libraries, prototypes, etc */
%{
#include<stdio.h>
#include<string.h>
#include<assert.h>
#include "shareddefs.h"
int yylex(void);
int yywrap();
int yyerror(char* s);

// declared in lex
extern int row;
extern int col;

int ErrorLevel;
int MapSize = 0;

/* Define a struct for our symbol table entries,
 *    each holds the symbol name, file row/col of declaration and the
 *    associated data values for map row/col and facing direction */
typedef struct Symbol {
   char SymName[MaxNameLen];
   int row, col;
   symbol_tag_t tag;
   union {
       // Var is valid for tag values of both ST_PARAM and ST_VAR
       struct { type_t type; struct Symbol *function; } var;
       struct { type_t rtype; int argc; } func;
       struct { } bot;
   };
} Symbol_t;

/* declare the empty symbol table, track number of known symbols */
static Symbol_t SymTable[MaxSyms];
static int NumSyms = 0;
static Symbol_t *CurrentFunction = 0;

/* Arguments iterator for checking arguments during function calls */
static int argumentsRemaining = 0;
static Symbol_t *argumentsIterator = 0;
static Symbol_t *argumentsFunction = 0;


/* function to insert a new bot in the symbol table
 * returns 1 if successful, 0 otherwise
 * (Cannot be used for creating variables) */
int insertBot(char const *name, int r, int c);

/* function to insert a new var in the symbol table
 * returns 1 if successful, 0 otherwise
 * (Cannot be used for creating bots) */
int insertVar(char const *name, int r, int c, type_t type, Symbol_t *function);

/* function to insert a new parameter into the symbol table
 * returns 1 if successful, 0 otherwise
 * (Cannot be used for creating other things) */
int insertParam(char const *name, int r, int c, type_t type);

/* function to insert a new func in the symbol table
 * returns 1 if successful, 0 otherwise
 * (Cannot be used for creating variables or bots) */
Symbol_t *createFunc(char const *name, int r, int c, int rtype);

/* function to check if a name is in the symbol table
 * returns symbol pointer, or null if unsuccessful */
Symbol_t *findSymbol(char const *name);

/* function to convert a type to a typename string
 * returns nonempty null-terminated string */
char const *typeToName( type_t type );

/* function to print the current contents of the symbol table */
void printTable();

%}

 /* begin processing the top-level component */
%start program

%union { struct nodeinfo {
   char name[ /* MaxNameLen */ 256 ];
   union {
      // used on `L_NUMBER` token
      long number;
      // DIR_*, used on `L_DIR` token
      dir_t dir;
      // B_*, used on `BUILTIN` token
      builtin_t builtin;
      // T_*, used on `value` nonterminal
      type_t type;
      // COMP_*, used on `COMPOP` token
      comp_t comp;
      // ARTH_*, used by Arithmatic operators like + and -
      arth_t arth;
   };
} info; }

 /* identify the valid token types, all have yylval type long */
%token<struct nodeinfo>
 /* Literals */
 L_STRING L_NUMBER L_DIR
 /* Symbols */
 SEMI LBRACKET RBRACKET ASSIGNOP COLON COMMA ADDSUB
 /* Typenames */
 TYPENAME
 /* program */
 MAPSIZE START FINISH
 /* functions */
 FUNCTION FUNC_BEGIN
 /* action */
 CREATE MOVE TURN PRINT RETURN
 /* loop */
 REPEAT UNTIL
 /* select */
 IF THEN END
 /* builtin */
 BUILTIN COMPOP
 /* User provided names */
 BOT_NAME VAR_NAME

/* Addition and Subtraction uses LTR associativity */
%left ADDSUB

 /* identify all nonterminals */
%type<struct nodeinfo> program bot_ref var_ref statement statements function functions
 parameter oneormoreparameters parameters action loop select comparison value lookup
 var_decl argument oneormorearguments arguments addsub_expr return_stmt
 func_call func_call_stmt

 /* ---- part 2: grammar rules ----
  */

%%

program: MAPSIZE L_NUMBER SEMI
   {
       MapSize = $<info.number>2;
   }
       functions
   START
       statements
   FINISH
   ;

function: FUNCTION VAR_NAME LBRACKET parameters RBRACKET COLON TYPENAME
   {
      Symbol_t *func = createFunc($<info.name>2, row, col, $<info.type>7);
      if (!func)
      {
         char buf[MaxNameLen + 50];
         sprintf(buf, "cannot redeclare function `%s`", $<info.name>2);
         yyerror(buf);
         break;
      }

      // Set our parameters' scope by greedily claiming all un-scoped parameters.
      for( int idx=0; idx<NumSyms; idx++ )
      if( SymTable[idx].tag == ST_PARAM && !SymTable[idx].var.function )
      {
         SymTable[idx].var.function = func;
         func->func.argc++;
      }

      // Used by local variables and scope resolution
      CurrentFunction = func;
   }
   FUNC_BEGIN statements END
   {
      // We are no longer in a function
      CurrentFunction = 0;
   }
   ;

functions: {}
   | function functions
   ;

parameter: TYPENAME VAR_NAME
   {
   if (!insertParam($<info.name>2, row, col, $<info.type>1))
   {
      char buf[MaxNameLen + 100];
      sprintf(buf,
         "cannot declare parameter %s (type %s). You're likely shadowing another parameter or function",
         $<info.name>2, typeToName($<info.type>1)
      );
      yyerror(buf);
   }
}
   ;


oneormoreparameters: parameter
   | parameter COMMA oneormoreparameters
   ;

parameters: {}
   | oneormoreparameters
   ;

bot_ref: BOT_NAME {
   Symbol_t *sym = findSymbol($<info.name>1);
   if(!sym)
   {
      char buf[MaxNameLen+40];
      sprintf(buf, "Bot %s referenced before creation.", $<info.name>1);
      yyerror(buf);
      $<info.type>$ = T_ERROR;
      break;
   }
   $<info.type>$ = T_NAME;
}

var_ref: VAR_NAME {
   Symbol_t *sym = findSymbol($<info.name>1);
   if(!sym)
   {
      char buf[MaxNameLen+40];
      sprintf(buf, "Variable %s referenced before creation.", $<info.name>1);
      yyerror(buf);
      $<info.type>$ = T_ERROR;
      break;
   }
   if(sym->tag != ST_VAR && sym->tag != ST_PARAM)
   {
      char buf[MaxNameLen+100];
      sprintf(buf, "Cannot reference symbol '%s' as variable nor parameter. Perhaps it's a function?", $<info.name>1);
      yyerror(buf);
      $<info.type>$ = T_ERROR;
      break;
   }
   $<info.type>$ = sym->var.type;
}

statements: statement statements
   | statement
   ;

statement: select
   | loop
   | action
   | var_decl
   | return_stmt
   | func_call_stmt
   ;

return_stmt: RETURN value SEMI
   {
      if ( !CurrentFunction )
      {
         yyerror("`return` is only allowed inside of a function!");
         break;
      }
      if ( CurrentFunction->func.rtype == T_NONE )
      {
         char buf[MaxNameLen+50];
         sprintf(buf, "Cannot `return` in function `%s`, because it returns type none!", CurrentFunction->SymName);
         yyerror(buf);
         break;
      }
      type_t tval = $<info.type>2;
      if ( CurrentFunction->func.rtype != tval )
      {
         char buf[MaxNameLen+100];
         sprintf(buf, "Cannot `return` value of type %s in function `%s`, because it returns type `%s`!",
            typeToName(tval), CurrentFunction->SymName, typeToName(CurrentFunction->func.rtype)
         );
         yyerror(buf);
         break;
      }
   }
   ;

func_call_stmt: func_call SEMI
   {
      if ( $<info.type>1 != T_NONE )
      {
         char buf[MaxNameLen+128];
         sprintf(buf, "Function call to `%s` returns type `%s`. Only nonetype function calls are allowed at statement level!", $<info.name>1, typeToName($<info.type>1));
         yyerror(buf);
      }
   }
   ;

func_call: VAR_NAME LBRACKET
   {
      char const *func_name = $<info.name>1;
      Symbol_t *func = findSymbol(func_name);
      if( !func )
      {
          char buf[MaxNameLen + 30];
          sprintf(buf, "Cannot call undeclared function `%s`!\n", func_name);
          yyerror(buf);
          $<info.type>$ = T_ERROR;
          break;
      }

      if( func->tag != ST_FUNC )
      {
          char buf[MaxNameLen + 30];
          sprintf(buf, "Cannot call symbol `%s`! is it a variable or parameter?\n", func_name);
          yyerror(buf);
          $<info.type>$ = T_ERROR;
          break;
      }

      $<info.type>$ = func->func.rtype;

      // Preparations for arguments typechecking
      argumentsRemaining = func->func.argc;
      argumentsFunction = func;
      // Find the start of the arguments
      if(argumentsRemaining>0)
      for(
         argumentsIterator = SymTable;
         argumentsIterator->tag != ST_PARAM
         || argumentsIterator->var.function != func;
         argumentsIterator++
      ) assert(argumentsIterator < SymTable + MaxSyms);
   }
   arguments RBRACKET
   {
      if( ErrorLevel==0 && argumentsRemaining>0 )
      {
         char buf[MaxNameLen+100];
         sprintf(buf, "Function call to `%s` is missing %d arguments! Expected %d total.",
            argumentsFunction->SymName,
            argumentsRemaining,
            argumentsFunction->func.argc
         );
         yyerror(buf);
      }
      // Clear iterator
      argumentsRemaining = 0;
      argumentsFunction = 0;
      argumentsIterator = 0;

      // Only the last code block has an effect on the whole rule's value.
      $<info.type>$ = $<info.type>3;
      // Allow function name to be printed in func_call_stmt and value error messages
      strcpy($<info.name>$, $<info.name>1);
   }
   ;

argument: value
   {
      // Skip arguments check if there's already been an error.
      if( ErrorLevel!=0 ) break;

      // ICE
      assert(argumentsFunction);

      if( argumentsRemaining<=0 )
      {
         char buf[MaxNameLen+40];
         sprintf(buf, "Too many arguments in call to function `%s`", argumentsFunction->SymName);
         yyerror(buf);
         break;
      }

      // ICE's:
      assert(argumentsIterator);
      assert(argumentsIterator->tag == ST_PARAM);
      assert(argumentsIterator->var.function == argumentsFunction);
      assert(argumentsIterator < SymTable + MaxSyms);

     if( argumentsIterator->var.type != $<info.type>1 )
     {
         char buf[MaxNameLen+100];
         sprintf(buf, "Type mismatch in argument to function `%s`! expected type `%s`, got `%s`.",
            argumentsFunction->SymName,
            typeToName(argumentsIterator->var.type),
            typeToName($<info.type>1)
         );
         yyerror(buf);
         break;
     }

      argumentsIterator++;
      argumentsRemaining--;
   }
   ;

oneormorearguments: argument
   | argument COMMA oneormorearguments
   ;

arguments: {}
   | oneormorearguments
   ;

action: CREATE BOT_NAME value value value SEMI
   {
      if ($<info.type>3 != T_INT)
      {
         char buf[MaxNameLen+64];
         sprintf(buf,
            "Expected integer for maprow of bot `%s`, got value of type `%s`.",
            $<info.name>2, typeToName( $<info.type>3 )
         );
         yyerror(buf);
      }
      if ($<info.type>4 != T_INT)
      {
         char buf[MaxNameLen+64];
         sprintf(buf,
            "Expected integer for maprow of bot `%s`, got value of type `%s`.",
            $<info.name>2, typeToName( $<info.type>4 )
         );
         yyerror(buf);
      }
      if ($<info.type>5 != T_FACING)
      {
         char buf[MaxNameLen+64];
         sprintf(buf,
            "Expected facing for direction of bot `%s`, got value of type `%s`.",
            $<info.name>2, typeToName( $<info.type>5 )
         );
         yyerror(buf);
      }
      if (!insertBot($<info.name>2, row, col))
      {
         char buf[MaxNameLen+40];
         sprintf(buf, "Redeclaration of `%s`", $<info.name>2);
         yyerror(buf);
      }
   }
   | MOVE value value SEMI {
       if( $<info.type>2 != T_NAME )
       {
           char buf[128];
           sprintf(buf, "`%s` expression invalid here, only bots can `move`!", typeToName($<info.type>2));
           yyerror(buf);
       }
       if( $<info.type>3 != T_INT )
       {
           char buf[128];
           sprintf(buf, "%s expression invalid here, bots can only `move` an `int` number of steps!", typeToName($<info.type>3));
           yyerror(buf);
       }
   }
   | TURN value value SEMI {
       if( $<info.type>2 != T_NAME )
       {
           char buf[128];
           sprintf(buf, "`%s` expression invalid here, only bots can `turn`!", typeToName($<info.type>2));
           yyerror(buf);
       }
       if( $<info.type>3 != T_FACING )
       {
           char buf[128];
           sprintf(buf, "`%s` expression invalid here, bots can only `turn` a `facing` number of steps!", typeToName($<info.type>3));
           yyerror(buf);
       }
   }
   | PRINT value SEMI
   | VAR_NAME ASSIGNOP value SEMI
   {
      char const *const varName = $<info.name>1;
      Symbol_t *sym = findSymbol(varName);
      type_t exprType = $<info.type>3;
      if( !sym )
      {
          char buf[MaxNameLen + 20];
          sprintf(buf, "Cannot assign to undeclared variable `%s`\n", varName);
          yyerror(buf);
          break;
      }
      // NOTE: We allow assigning T_ERROR to anything, as the T_ERROR is only generated
      //  by a prior error.
      if( sym->var.type != exprType  && exprType != T_ERROR )
      {
          char buf[MaxNameLen + 20];
          sprintf(buf, "Cannot assign `%s` to variable `%s` of type `%s`: Type mismatch\n",
             typeToName(exprType), varName, typeToName(sym->var.type)
          );
          yyerror(buf);
          break;
      }
   }
   ;

loop: REPEAT statements UNTIL comparison
   ;

select: IF comparison THEN statements END
   ;

comparison: LBRACKET value COMPOP value RBRACKET
   {
      switch( $<info.comp>3 )
      {
         case COMP_EQ:
         case COMP_NE:
         if( $<info.type>2 != $<info.type>4 )
         {
             yyerror("Comparison of mismatched types!");
         }
         break;

         default:
         if( $<info.type>2 != T_INT || $<info.type>4 != T_INT)
         {
             yyerror("Numeric comparison involving one or more invalid types!");
         }
         break;
      }
   }

value: addsub_expr
   | lookup
   | var_ref
   | bot_ref /* BOT_NAME Literal */
   | func_call
   {
      if( $<info.type>1==T_NONE )
      {
         char buf[MaxNameLen+70];
         sprintf(buf, "Cannot call function `%s` in an expression; nonetype is forbidden in expressions!", $<info.name>1);
         yyerror(buf);
         $<info.type>$ = T_ERROR;
      }
   }
   | L_NUMBER { $<info.type>$ = T_INT; }
   | L_DIR { $<info.type>$ = T_FACING; }
   | L_STRING { $<info.type>$ = T_STR; }
   ;

addsub_expr: value ADDSUB value
   {
      // Add/sub always returns an integer.
      $<info.type>$ = T_INT;

      // Typechecking
      int lgood = $<info.type>1 == T_INT;
      int rgood = $<info.type>3 == T_INT;
      if(!lgood && !rgood)
      {
          char buf[256];
          sprintf(buf,
             "Add/sub invalid on non-integer argument types %s and %s!",
             typeToName($<info.type>1),
             typeToName($<info.type>3)
          );
          yyerror(buf);
      }
      else if (!lgood && rgood)
      {
          char buf[256];
          sprintf(buf,
             "Add/sub's left hand expression invalid; non-integer type %s!",
             typeToName($<info.type>1)
          );
          yyerror(buf);
      }
      else if (lgood && !rgood)
      {
          char buf[256];
          sprintf(buf,
             "Add/sub's right hand expression invalid; non-integer type %s!",
             typeToName($<info.type>1)
          );
          yyerror(buf);
      }
      // else, all OK.
   }
   ;

lookup: BUILTIN LBRACKET value RBRACKET
   {
       // builtin determines return type;
       type_t ret = T_ERROR;
       switch($<info.builtin>1)
       {
           case B_ROW: case B_COL:
               ret = T_INT;
               break;
           case B_FACE:
               ret = T_FACING;
               break;
       }
       $<info.type>$ = ret;

       if( $<info.type>3 != T_NAME )
       {
           char buf[128];
           sprintf(buf, "Lookup can only operate on Bots! Was passed type of %s\n", typeToName($<info.type>3));
           yyerror(buf);
       }
   }
   ;

var_decl: TYPENAME VAR_NAME SEMI
   {
      if ( $<info.type>1 == T_NONE )
      {
         char buf[MaxNameLen+50];
         sprintf(buf, "Variable %s declared as type `none`; this is forbidden!", $<info.name>2);
         yyerror(buf);
      }
      if (!insertVar($<info.name>2, row, col, $<info.type>1, CurrentFunction))
      {
         char buf[MaxNameLen + 100];
         // TODO: Convert printf's to sprintf's, check all sprintf str safety.
         sprintf(buf,
            "Cannot declare variable `%s` (type %s)! You may be shadowing another variable, a parameter, or a function.",
            $<info.name>2, typeToName($<info.type>1)
         );
         yyerror(buf);
      }
   }
   ;

 /* ---- part 3: supporting programs ---- */

%%

 /* begin parsing */
int main()
{
   printf("Compilation begins:\n\n");
   ErrorLevel = 0;
   int res = yyparse();
   printf("\nCompilation complete.\n", res);
   res |= ErrorLevel;
   if( res ) printf("(Compilation failed)\n");
   else printTable();
   return(res);
}

Symbol_t *findSymbol(char const *name)
{
   // NOTE: When finding a variable or a bot, there's no need
   //  to check if it's the other kind, because the set of bot
   //  names and var names are disjoint.
   // the is_bot flag is simply for displayTable's use.
   for( int idx=0; idx<NumSyms; idx++ )
   {
      if(
         ( SymTable[idx].tag==ST_VAR || SymTable[idx].tag==ST_PARAM )
         && SymTable[idx].var.function != CurrentFunction
      ) continue; // out of scope
      if( !strcmp( SymTable[idx].SymName, name ) )
         return &SymTable[idx];
   }
   return 0;
}

static Symbol_t *_insert(char const *name, int r, int c, symbol_tag_t st)
{
   if( NumSyms>=MaxSyms )
   {
      yyerror("ICE: Symbol table full!");
      return 0;
   }
   if( findSymbol( name ) )
      return 0;

   Symbol_t *sym = &SymTable[NumSyms++];
   sym->tag = st;
   sym->row = row;
   sym->col = col;   

   sym->SymName[0] = '\0';
   strncat(sym->SymName, name, MaxNameLen);

   return sym;
}

int insertBot(char const *name, int r, int c)
{
   Symbol_t *sym = _insert(name, r, c, ST_BOT);
   if( !sym )
      return 0;

   return 1;
}

int insertVar(char const *name, int r, int c, type_t type, Symbol_t *function)
{
   Symbol_t *sym = _insert(name, r, c, ST_VAR);
   if( !sym )
      return 0;

   sym->var.type = type;
   sym->var.function = function;

   return 1;
}

int insertParam(char const *name, int r, int c, type_t type)
{
   Symbol_t *sym = _insert(name, r, c, ST_PARAM);
   if( !sym )
      return 0;

   sym->var.type = type;
   sym->var.function = 0; // filled by function, not parameter.. :P

   return 1;
}

Symbol_t *createFunc(char const *name, int r, int c, int rtype)
{
   Symbol_t *sym = _insert(name, r, c, ST_FUNC);
   if( !sym )
      return 0;

   sym->func.rtype = rtype;
   sym->func.argc = 0;

   return sym;
}

void printTable()
{
   printf(
      "Map Size %d\n"
      "Symbol table (%d entries):\n",
      MapSize, NumSyms
   );
   for( int x=0; x<NumSyms; x++ )
   {
      Symbol_t *s = &SymTable[x];
      if(s->tag!=ST_BOT) continue;
      char const *DIR = "NSWE";
      printf(
         " %d,%d\t%s\n",
         s->row+1, s->col+1, s->SymName
         //"   %s declared line %d, column %d, coordinates (%d,%d), facing %c\n",
         //s->SymName, s->row+1, s->col+1, s->bot.maprow, s->bot.mapcol, DIR[s->bot.facing]
      );
   }
   puts(" - - - -");
   for( int x=0; x<NumSyms; x++ )
   {
      Symbol_t *s = &SymTable[x];
      if(s->tag!=ST_VAR) continue;
      printf(
         " %d,%d\t%s\t%s\t{%s}\n",
         s->row+1, s->col+1, typeToName(s->var.type), s->SymName,
         s->var.function ? s->var.function->SymName : "~"
         //"   %s declared line %d, column %d, type %s\n",
         //s->SymName, s->row+1, s->col+1, typeToName(s->var.type)
      );
   }
   puts(" - - - -");
   for( int x=0; x<NumSyms; x++ )
   {
      Symbol_t *s = &SymTable[x];
      if(s->tag!=ST_FUNC) continue;
      printf(
         " %d,%d\t%d\t%s\t%s()\n",
         s->row+1, s->col, s->func.argc, typeToName(s->func.rtype), s->SymName
         //"   %s declared line %d, column %d, return-type %s\n",
         //s->SymName, s->row+1, s->col, typeToName(s->func.rtype)
      );
      for( int y=0; y<NumSyms; y++ )
      {
         Symbol_t *s2 = &SymTable[y];
         if( s2->tag != ST_PARAM || s2->var.function != s ) continue;
         printf(
            " ... %d, %d\t%s\t%s\n",
            s2->row+1, s2->col+1, typeToName(s2->var.type), s2->SymName
         );
      }
   }
}

char const *typeToName( type_t type ) {
   char const *const typenames[T_END - T_ERROR] = {
      "<ERRORTYPE>",
      "none",
      "facing",
      "name",
      "str",
      "int"
   };
   if(type<T_ERROR || type >= T_END)
   {
      // NOTE: slightly unsafe, but I think it's worthwhile.
      // This code will only run in the case of an internal compiler bug.
      static char s_buf[20];
      sprintf(s_buf, "<<ICE ERRORTYPE: 0x%x>>", type);
      return s_buf;
   }
   return typenames[type - T_ERROR];
}
