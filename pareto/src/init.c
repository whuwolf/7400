#include "Rinternals.h"
#include "R_ext/Rdynload.h"
#include "pareto.h"

/* This defines a data structure for registering the routine
   `addone'. It records the number of arguments, which allows some
   error checking when the routine is called. */
static R_CMethodDef DotCEntries[] = {
    {"dparetoDotC", (DL_FUNC) paretodensDotC, 8},
    {"qparetoDotC", (DL_FUNC) paretoquantDotC, 9},
    {"pparetoDotC", (DL_FUNC) paretocdfDotC, 9},
    {NULL}
};

/* This is called by the dynamic loader to register the routine. */
void R_init_pareto(DllInfo *info)
{
    R_registerRoutines(info, DotCEntries, NULL, NULL, NULL);
}
