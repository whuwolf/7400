rpareto <- function(n, alpha, beta) {
    qpareto(runif(n), alpha, beta)
}

rcpareto <- function(n, alpha, beta) {
    if (length(n) > 1) n <- length(n) else n <- floor(n)
    nalpha <- length(alpha)
    nbeta <- length(beta)
    
    result <- .C("rparetoDotC",
                 as.double(n), as.double(alpha), as.integer(nalpha), 
                 as.double(beta), as.integer(nbeta), r = double(n), 
                 NAOK = TRUE, PACKAGE="pareto")
    result$r    
}