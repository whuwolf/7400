dpareto <- function(x,alpha,beta){
  
  #Calculate the maximum length of inputs
  L <- max(length(x), length(alpha), length(beta))
  
  #Align all the inputs
  x <- rep(x,length.out = L)
  alpha <- rep(alpha,length.out = L)
  beta <- rep(beta,length.out = L)
  
  #Initialize the densities with NaN
  dens <- rep(NaN, length.out = L)

  for (i in 1:L){
    if (alpha[i] <= 0 || beta[i] <= 0){
      warning("NaNs produced")
      next
    } else if (x[i] < alpha[i]){
      dens[i] <- 0
    } else{
      dens[i] <- beta[i]*alpha[i]^beta[i]/x[i]^(beta[i]+1)
    }
  }
  return(dens)
}


