#include <Rmath.h>
#include <R.h>
#ifndef max
#define max( a, b ) ( ((a) > (b)) ? (a) : (b) )
#endif
#include "pareto.h"

void paretocdfDotC(double *q, int *nq, double *alpha, int *nalpha,
                    double *beta, int *nbeta, double *p, int *lt, int *lg)
{
    /*Calculate the maximum input length */
    int i, n = max(max(nq[0], nalpha[0]), nbeta[0]), flag = 0, ind[3];
    double lp[n];

/*    if (lg[0] == 1) {
        for (i = 0; i < np[0]; i++) {
            if (R_FINITE(p[i])){
                p[i] = exp(p[i]);
            }
            else if (ISNAN(p[i])) {
                p[i] = NAN;
            }
            else if (p[i] == R_PosInf) {
                p[i] = R_PosInf;
            }
            else if (p[i] == R_NegInf) {
                p[i] = R_NegInf;
            }
        }
    }
*/
    for (i = 0; i < n; i++) {
        ind[0] = i % nq[0];
        ind[1] = i % nalpha[0];
        ind[2] = i % nbeta[0];

        /*Calculate whether alpha and beta are meaningful */
        if (alpha[ind[1]] <= 0 || beta[ind[2]] <= 0) {
            lp[i] = NAN;
            flag++;
        }
        /*Check whether x is greater than alpha */
        else if (q[ind[0]] <= alpha[ind[1]]) {
            lp[i] = 0;
        }
        else {
            lp[i] = beta[ind[2]] * (log(alpha[ind[1]]) - log(q[ind[0]]));
        }

    }

    if (lt[0] == 1) {
        if (lg[0] == 1) {
            for (i = 0; i < n; i++) {
                p[i] = log(1 - exp(lp[i]));
            }
        }
        else {
            for (i = 0; i < n; i++) {
                p[i] = 1 - exp(lp[i]);
            }
        }
    }
    else {
        if (lg[0] == 1) {
            for (i = 0; i < n; i++) {
                p[i] = lp[i];
            }
        }
        else {
            for (i = 0; i < n; i++) {
                p[i] = exp(lp[i]);
            }
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
    {"qparetoDotC", (DL_FUNC) paretoquantDotC, 9},
    {NULL}
};
*/

/* This is called by the dynamic loader to register the routine. */
/*void R_init_pareto(DllInfo *info)
{
    R_registerRoutines(info, DotCEntries, NULL, NULL, NULL);
}
*/
