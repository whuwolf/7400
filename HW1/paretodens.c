#include <stdio.h>
#include <math.h>

/*Define parto density function*/
double paretodens(double x, double alpha, double beta) {
    /*Check whether alpha and beta are positve*/
    if (alpha <=0 || beta <= 0){
        printf("Warning message: The parameters are not meaningful! NaN produced!\n");
        return NAN;
    }
    /*Check whether x is greater than alpha*/
    else if (x < alpha){
        return 0;
    }
    else {
        return beta*pow(alpha,beta)/pow(x,beta+1);
    }
}

int main() {
    double x,alpha,beta;

    printf("Input x:\n");
    scanf("%lf",&x);

    printf("Input alpha:\n");
    scanf("%lf",&alpha);

    printf("Input beta:\n");
    scanf("%lf",&beta);

    printf("The density is %lf.\n", paretodens(x,alpha,beta));
    return 0;
}
