#pragma once

#define MaxNameLen 256
#define MaxSyms    256

extern int ErrorLevel;

typedef enum {
   DIR_N,
   DIR_S,
   DIR_W,
   DIR_E
} dir_t;

// typenames
typedef enum {
   T_ERROR = 51,
   T_FACING,
   T_NAME,
   T_STR,
   T_INT,
   T_END
} type_t;

// builtins (BUILTIN)
typedef enum {
   B_ROW,
   B_COL,
   B_FACE
} builtin_t;

// comparisons (COMPOP)
typedef enum {
   COMP_LT,
   COMP_LTEQ,
   COMP_EQ,
   COMP_NE,
} comp_t;
