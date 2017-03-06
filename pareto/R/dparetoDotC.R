dpareto <- function(x, alpha, beta, log = FALSE) {
    nx <- length(x)
    nalpha <- length(alpha)
    nbeta <- length(beta)
    n <- max(nx, nalpha, nbeta)
    
    result <- .C("dparetoDotC",
                 as.double(x), as.integer(nx),
                 as.double(alpha), as.integer(nalpha), 
                 as.double(beta), as.integer(nbeta),
                 dens = double(n), as.integer(log),PACKAGE="pareto")
    result$dens
}