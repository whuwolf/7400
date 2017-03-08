qpareto <- function(p, alpha, beta, lower.tail = TRUE, log.p = FALSE) {
    np <- length(p)
    nalpha <- length(alpha)
    nbeta <- length(beta)
    n <- max(np, nalpha, nbeta)
    
    result <- .C("qparetoDotC",
                 as.double(p), as.integer(np),
                 as.double(alpha), as.integer(nalpha), 
                 as.double(beta), as.integer(nbeta),
                 qs = double(n), as.integer(lower.tail), as.integer(log.p),
                 NAOK = TRUE, PACKAGE="pareto")
    result$qs    
}

