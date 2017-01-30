dpareto <- function(x, alpha, beta, log = FALSE) {
    
    # Calculate the maximum length of inputs
    L <- max(length(x), length(alpha), length(beta))
    
    # Align all the inputs
    x <- rep(x, length.out = L)
    alpha <- rep(alpha, length.out = L)
    beta <- rep(beta, length.out = L)
    
    logdens <- ifelse(alpha <= 0 | beta <= 0,
                      NaN,
                      ifelse(x <= alpha,
                             log(0),
                             log(beta) + beta * log(alpha) - (beta + 1) * log(x)))
    
    # Check whether NaNs exist
    if (sum(is.nan(logdens))) 
        warning("NaNs produced")
    
    if (log) logdens else exp(logdens)
}