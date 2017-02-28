ppareto <- function(q, alpha, beta, lower.tail = TRUE, log.p = FALSE) {
    
    # Calculate the maximum length of inputs
    L <- max(length(q), length(alpha), length(beta))
    
    # Align all the inputs
    q <- rep(q, length.out = L)
    alpha <- rep(alpha, length.out = L)
    beta <- rep(beta, length.out = L)
    
    lp <- ifelse(alpha <= 0 | beta <= 0, 
                   NaN, 
                   ifelse(q <= alpha, 0, beta * (log(alpha) - log(q))))
    
    # Check whether NaNs exist
    if (sum(is.nan(lp))) 
        warning("NaNs produced")
    
    if (lower.tail) {
        p <- exp(lp)
        if (log.p) log1p(-p) else 1 - p
    } else {
        if (log.p) lp else exp(lp)
    }
}