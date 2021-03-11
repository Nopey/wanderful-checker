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
   // as these enum values are arbitrary, I'm distancing
   // them from `0` to make debugging uninitialized values easier
   T_ERROR = 51,
   T_NONE, // Like C's `void`.
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

// discriminator for symbol union
typedef enum {
    ST_PARAM,
    ST_VAR,
    ST_BOT,
    ST_FUNC
} symbol_tag_t;

// Arithmatic operators
typedef enum {
    ARTH_ADD,
    ARTH_SUB,
} arth_t;
