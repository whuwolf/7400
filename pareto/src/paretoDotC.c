#include "Rinternals.h"
#include "R_ext/Rdynload.h"
#include <Rmath.h>
#include <R.h>
#ifndef max
#define max( a, b ) ( ((a) > (b)) ? (a) : (b) )
#endif

static void paretodensDotC(double *x, int *nx, double *alpha, int *nalpha,
                    double *beta, int *nbeta, double *dens, int *lg)
{
    /*Calculate the maximum input length */
    int i, n = max(max(nx[0], nalpha[0]), nbeta[0]), flag = 0, ind[3];
    /*double ldens[n];*/

    for (i = 0; i < n; i++) {
        ind[0] = i % nx[0];
        ind[1] = i % nalpha[0];
        ind[2] = i % nbeta[0];

        /*Calculate whether alpha and beta are meaningful */
        if (alpha[ind[1]] <= 0 || beta[ind[2]] <= 0) {
            dens[i] = NAN;
            flag++;
        }
        /*Check whether x is greater than alpha */
        else if (x[ind[0]] <= alpha[ind[1]]) {
            dens[i] = log(0);
        }
        else {
            dens[i] =
                log(beta[ind[2]]) + beta[ind[2]] * log(alpha[ind[1]]) -
                (beta[ind[2]] + 1) * log(x[ind[0]]);
        }

        if (lg[0] == 0) {
            dens[i] = exp(dens[i]);
        }

    }
    /*Check whether NaNs exist */
    if (flag > 0) {
        warning("NaNs produced");
    }

}

static void paretodensDotC_p(double *x, int *nx, double *alpha, int *nalpha,
                    double *beta, int *nbeta, double *dens, int *lg, int *P)
{
    /*Calculate the maximum input length */
    int i, n = max(max(nx[0], nalpha[0]), nbeta[0]), ind[3];
    /*double ldens[n];*/
    int naflag = FALSE;
#pragma omp parallel for num_threads(P[0]) default(none) \
    firstprivate(n, x, nx, alpha, nalpha, beta, nbeta, dens, lg) private(i, ind) \
    reduction(||:naflag)

    for (i = 0; i < n; i++) {
        ind[0] = i % nx[0];
        ind[1] = i % nalpha[0];
        ind[2] = i % nbeta[0];

        /*Calculate whether alpha and beta are meaningful */
        if (alpha[ind[1]] <= 0 || beta[ind[2]] <= 0) {
            dens[i] = NAN;
        }
        /*Check whether x is greater than alpha */
        else if (x[ind[0]] <= alpha[ind[1]]) {
            dens[i] = log(0);
        }
        else {
            dens[i] =
                log(beta[ind[2]]) + beta[ind[2]] * log(alpha[ind[1]]) -
                (beta[ind[2]] + 1) * log(x[ind[0]]);
        }

        if (lg[0] == 0) {
            dens[i] = exp(dens[i]);
        }

        if (ISNAN(dens[i]))
            naflag = TRUE;

    }
    /*Check whether NaNs exist */
    if (naflag) {
        warning("NaNs produced");
    }

}

static void paretoquantDotC(double *p, int *np, double *alpha, int *nalpha,
                    double *beta, int *nbeta, double *qs, int *lt, int *lg)
{
    /*Calculate the maximum input length */
    int i, n = max(max(np[0], nalpha[0]), nbeta[0]), flag = 0, ind[3];
    /*double ldens[n];*/

    if (lg[0] == 1) {
        for (i = 0; i < np[0]; i++) {
            p[i] = exp(p[i]);
        }
    }

    for (i = 0; i < n; i++) {
        ind[0] = i % np[0];
        ind[1] = i % nalpha[0];
        ind[2] = i % nbeta[0];

        /*Calculate whether alpha and beta are meaningful */
        if (alpha[ind[1]] <= 0 || beta[ind[2]] <= 0) {
            qs[i] = NAN;
            flag++;
        }
        /*Check whether x is greater than alpha */
        else if (p[ind[0]] < 0 || p[ind[0]] >1) {
            qs[i] = NAN;
            flag++;
        }
        else if (lt[0] == 1) {
            qs[i] = alpha[ind[1]]/R_pow(1 - p[ind[0]], 1/beta[ind[2]]);
        }
        else {
            qs[i] = alpha[ind[1]]/R_pow(p[ind[0]], 1/beta[ind[2]]);
        }

    }
    /*Check whether NaNs exist */
    if (flag > 0) {
        warning("NaNs produced");
    }

}

void paretoquantDotC_p(double *p, int *np, double *alpha, int *nalpha,
                    double *beta, int *nbeta, double *qs, int *lt, int *lg, int *P)
{
    /*Calculate the maximum input length */
    int i, n = max(max(np[0], nalpha[0]), nbeta[0]), ind[3];
    /*double ldens[n];*/
    int naflag = FALSE;

    if (lg[0] == 1) {
        for (i = 0; i < np[0]; i++) {
            p[i] = exp(p[i]);
        }
    }

#pragma omp parallel for num_threads(P[0]) default(none) \
    firstprivate(n, p, np, alpha, nalpha, beta, nbeta, qs, lt) private(i, ind) \
    reduction(||:naflag)
    for (i = 0; i < n; i++) {
        ind[0] = i % np[0];
        ind[1] = i % nalpha[0];
        ind[2] = i % nbeta[0];

        /*Calculate whether alpha and beta are meaningful */
        if (alpha[ind[1]] <= 0 || beta[ind[2]] <= 0) {
            qs[i] = NAN;
        }
        /*Check whether x is greater than alpha */
        else if (p[ind[0]] < 0 || p[ind[0]] >1) {
            qs[i] = NAN;
        }
        else if (lt[0] == 1) {
            qs[i] = alpha[ind[1]]/R_pow(1 - p[ind[0]], 1/beta[ind[2]]);
        }
        else {
            qs[i] = alpha[ind[1]]/R_pow(p[ind[0]], 1/beta[ind[2]]);
        }
        if (ISNAN(qs[i]))
            naflag = TRUE;

    }
    /*Check whether NaNs exist */
    if (naflag) {
        warning("NaNs produced");
    }

}

void paretocdfDotC(double *q, int *nq, double *alpha, int *nalpha,
                    double *beta, int *nbeta, double *p, int *lt, int *lg)
{
    /*Calculate the maximum input length */
    int i, n = max(max(nq[0], nalpha[0]), nbeta[0]), flag = 0, ind[3];
    /*double lp[n];*/

    for (i = 0; i < n; i++) {
        ind[0] = i % nq[0];
        ind[1] = i % nalpha[0];
        ind[2] = i % nbeta[0];

        /*Calculate whether alpha and beta are meaningful */
        if (alpha[ind[1]] <= 0 || beta[ind[2]] <= 0) {
            p[i] = NAN;
            flag++;
        }
        /*Check whether x is greater than alpha */
        else if (q[ind[0]] <= alpha[ind[1]]) {
            p[i] = 0;
        }
        else {
            p[i] = beta[ind[2]] * (log(alpha[ind[1]]) - log(q[ind[0]]));
        }

    }

    if (lt[0] == 1) {
        if (lg[0] == 1) {
            for (i = 0; i < n; i++) {
                p[i] = log(1 - exp(p[i]));
            }
        }
        else {
            for (i = 0; i < n; i++) {
                p[i] = 1 - exp(p[i]);
            }
        }
    }
    else {
        if (lg[0] == 0) {
            for (i = 0; i < n; i++) {
                p[i] = exp(p[i]);
            }
        }
    }


    /*Check whether NaNs exist */
    if (flag > 0) {
        warning("NaNs produced");
    }

}



void paretocdfDotC_p(double *q, int *nq, double *alpha, int *nalpha,
                    double *beta, int *nbeta, double *p, int *lt, int *lg, int *P)
{
    /*Calculate the maximum input length */
    int i, n = max(max(nq[0], nalpha[0]), nbeta[0]), ind[3];
    /*double lp[n];*/
    int naflag = FALSE;

#pragma omp parallel for num_threads(P[0]) default(none) \
    firstprivate(n, q, nq, alpha, nalpha, beta, nbeta, p, lt, lg) private(i, ind) \
    reduction(||:naflag)
    for (i = 0; i < n; i++) {
        ind[0] = i % nq[0];
        ind[1] = i % nalpha[0];
        ind[2] = i % nbeta[0];

        /*Calculate whether alpha and beta are meaningful */
        if (alpha[ind[1]] <= 0 || beta[ind[2]] <= 0) {
            p[i] = NAN;
        }
        /*Check whether x is greater than alpha */
        else if (q[ind[0]] <= alpha[ind[1]]) {
            p[i] = 0;
        }
        else {
            p[i] = beta[ind[2]] * (log(alpha[ind[1]]) - log(q[ind[0]]));
        }
        if (ISNAN(p[i]))
            naflag = TRUE;
    }

    if (lt[0] == 1) {
        if (lg[0] == 1) {
            for (i = 0; i < n; i++) {
                p[i] = log(1 - exp(p[i]));
            }
        }
        else {
            for (i = 0; i < n; i++) {
                p[i] = 1 - exp(p[i]);
            }
        }
    }
    else {
        if (lg[0] == 0) {
            for (i = 0; i < n; i++) {
                p[i] = exp(p[i]);
            }
        }
    }


    /*Check whether NaNs exist */
    if (naflag) {
        warning("NaNs produced");
    }

}



/* This defines a data structure for registering the routine
   `addone'. It records the number of arguments, which allows some
   error checking when the routine is called. */
static R_CMethodDef DotCEntries[] = {
    {"dparetoDotC", (DL_FUNC) paretodensDotC, 8},
    {"dparetoDotC_p", (DL_FUNC) paretodensDotC_p, 9},
    {"qparetoDotC", (DL_FUNC) paretoquantDotC, 9},
    {"qparetoDotC_p", (DL_FUNC) paretoquantDotC_p, 10},
    {"pparetoDotC", (DL_FUNC) paretocdfDotC, 9},
    {"pparetoDotC_p", (DL_FUNC) paretocdfDotC_p, 10},
    {NULL}
};

/* This is called by the dynamic loader to register the routine. */
void R_init_pareto(DllInfo *info)
{
    R_registerRoutines(info, DotCEntries, NULL, NULL, NULL);
}

