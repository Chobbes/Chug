{-
"a//b" - string literal

#include "//e" - undefined behaviour

// */ - comment

f = g/**//h - f = g/h

//\
i (); - Two line comment

/\
/ j(); - Two line comment

#define glue(x,y) x##y
glue(/,/) k();  - Syntax error

/*//*/ l();  - l();

m = n//**/o
  + p;  - m = n + p

-}
