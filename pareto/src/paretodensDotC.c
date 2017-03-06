#include <Rmath.h>
#include <R.h>
#ifndef max
#define max( a, b ) ( ((a) > (b)) ? (a) : (b) )
#endif
#include "pareto.h"

void paretodensDotC(double *x, int *nx, double *alpha, int *nalpha,
                    double *beta, int *nbeta, double *dens, int *lg)
{
    /*Calculate the maximum input length */
    int i, n = max(max(nx[0], nalpha[0]), nbeta[0]), flag = 0, ind[3];
    double ldens[n];

    for (i = 0; i < n; i++) {
        ind[0] = i % nx[0];
        ind[1] = i % nalpha[0];
        ind[2] = i % nbeta[0];

        /*Calculate whether alpha and beta are meaningful */
        if (alpha[ind[1]] <= 0 || beta[ind[2]] <= 0) {
            ldens[i] = NAN;
            flag++;
        }
        /*Check whether x is greater than alpha */
        else if (x[ind[0]] <= alpha[ind[1]]) {
            ldens[i] = log(0);
        }
        else {
            ldens[i] =
                log(beta[ind[2]]) + beta[ind[2]] * log(alpha[ind[1]]) -
                (beta[ind[2]] + 1) * log(x[ind[0]]);
        }

        if (lg[0] == 0) {
            dens[i] = exp(ldens[i]);
        }
        else {
            dens[i] = ldens[i];
        }

    }
    /*Check whether NaNs exist */
    if (flag > 0) {
        warning("NaNs produced");
    }

}


/* This defines a data structure for registering the routine
   `addone'. It records the number of arguments, which allows some
   error checking when the routine is called. */
/*
static R_CMethodDef DotCEntries[] = {
    {"dparetoDotC", (DL_FUNC) paretodensDotC, 8},
    {NULL}
};
*/

/* This is called by the dynamic loader to register the routine. */
/*
void R_init_pareto(DllInfo *info)
{
    R_registerRoutines(info, DotCEntries, NULL, NULL, NULL);
}
*/
