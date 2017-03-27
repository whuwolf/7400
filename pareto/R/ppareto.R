ppareto <- function(q, alpha, beta, lower.tail = TRUE, log.p = FALSE) {
    nq <- length(q)
    nalpha <- length(alpha)
    nbeta <- length(beta)
    n <- max(nq, nalpha, nbeta)
    
    result <- .C("pparetoDotC",
                 as.double(q), as.integer(nq),
                 as.double(alpha), as.integer(nalpha), 
                 as.double(beta), as.integer(nbeta),
                 p = double(n), as.integer(lower.tail), as.integer(log.p),
                 NAOK = TRUE, PACKAGE="pareto")
    result$p    
}

p.ppareto <- function(q, alpha, beta, lower.tail = TRUE, log.p = FALSE, P = 1) {
    stopifnot(length(P) == 1)
    nq <- length(q)
    nalpha <- length(alpha)
    nbeta <- length(beta)
    n <- max(nq, nalpha, nbeta)
    
    result <- .C("pparetoDotC_p",
                 as.double(q), as.integer(nq),
                 as.double(alpha), as.integer(nalpha), 
                 as.double(beta), as.integer(nbeta),
                 p = double(n), as.integer(lower.tail), as.integer(log.p),
                 as.integer(P), NAOK = TRUE, PACKAGE="pareto")
    result$p    
}

