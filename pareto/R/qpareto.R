qpareto <- function(p, alpha, beta, lower.tail = TRUE, log.p = FALSE) {
    
    # Calculate the maximum length of inputs
    L <- max(length(p), length(alpha), length(beta))
    
    # Align all the inputs
    p <- rep(p, length.out = L)
    alpha <- rep(alpha, length.out = L)
    beta <- rep(beta, length.out = L)
    
    if (log.p) p <- exp(p)
    
    qs <- ifelse(alpha <= 0 | beta <= 0, 
                 NaN, 
                 ifelse(p < 0 | p > 1, 
                        NaN, 
                        if (lower.tail) {
                            alpha / (1 - p)^(1/beta)
                        } else {
                            alpha / p^(1/beta)
                        }))
    
    # Check whether NaNs exist
    if (sum(is.nan(qs))) 
        warning("NaNs produced")
    qs
}