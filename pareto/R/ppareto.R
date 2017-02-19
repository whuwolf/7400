ppareto1 <- function(q, alpha, beta, lower.tail = TRUE, log.p = FALSE) {
    
    # Calculate the maximum length of inputs
    L <- max(length(q), length(alpha), length(beta))
    
    # Align all the inputs
    q <- rep(q, length.out = L)
    alpha <- rep(alpha, length.out = L)
    beta <- rep(beta, length.out = L)
    
    cdfs <- ifelse(alpha <= 0 | beta <= 0,
                   NaN,
                   ifelse(q <= alpha, 0, 1 - (alpha/q)^beta))
    
    # Check whether NaNs exist
    if (sum(is.nan(cdfs))) 
        warning("NaNs produced")
    
    if (lower.tail){
        if (log.p) log(cdfs) else cdfs
    }else{
        if (log.p) log(1 - cdfs) else 1-cdfs
    }
}