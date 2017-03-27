dpareto <- function(x, alpha, beta, log = FALSE) {
    nx <- length(x)
    nalpha <- length(alpha)
    nbeta <- length(beta)
    n <- max(nx, nalpha, nbeta)
    
    result <- .C("dparetoDotC",
                 as.double(x), as.integer(nx),
                 as.double(alpha), as.integer(nalpha), 
                 as.double(beta), as.integer(nbeta),
                 dens = double(n), as.integer(log),NAOK = TRUE,
                 PACKAGE="pareto")
    result$dens
}

p.dpareto <- function(x, alpha, beta, log = FALSE, P = 1) {
    stopifnot(length(P) == 1)
    nx <- length(x)
    nalpha <- length(alpha)
    nbeta <- length(beta)
    n <- max(nx, nalpha, nbeta)
    
    result <- .C("dparetoDotC_p",
                 as.double(x), as.integer(nx),
                 as.double(alpha), as.integer(nalpha), 
                 as.double(beta), as.integer(nbeta),
                 dens = double(n), as.integer(log), 
                 as.integer(P), NAOK = TRUE, PACKAGE="pareto")
    result$dens
}
