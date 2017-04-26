# Problem 1 
#(a)
library(mgcv)
m <- function(x) {
    1 - sin(5 * x)^2 * exp(-4 * x)
}
gamsim <- function(R, n, s) {
    x <- (1 : n) / (n + 1)
    yhat <- sapply(seq_len(R), function(i) {
        y <- rnorm(n, m(x), s)
        gam(y ~ s(x))$fitted
    })
    eb <- rowMeans(yhat) - m(x)
    ese <- apply(yhat, 1, sd)
    cbind(eb, ese)
}

#(b)
library(parallel)
mergeBiases <- function(b, n, RR) rowMeans(matrix(b, n, length(RR)))
#mergeSDs <- function(s, RR) sqrt(sum(s^2 * ((RR - 1) / (sum(RR) - 1))))
mergeSDs <- function(s, n, RR) {
    sqrt(rowSums(matrix(s^2, n, length(RR))*(RR[1]-1))/(sum(RR) - 1))
}

pgamsim <- function(cl, R, n, s) {
    nw <- length(cl)
    RR <- rep(R / nw, nw)
    val <- do.call(rbind, parLapply(cl, RR, gamsim, n, s))
    eb <- mergeBiases(val[ ,"eb"], n, RR)
    ese <- mergeSDs(val[ ,"ese"], n, RR)
    cbind(eb, ese)
}

#(c)
cl <- makeCluster(2)
clusterExport(cl, c("m"))
clusterEvalQ(cl, library(mgcv))

clusterSetRNGStream(cl, 123)
system.time(val1 <- pgamsim(cl, 10000, 50, 0.2))
system.time(val <- gamsim(10000, 50, 0.2))

clusterSetRNGStream(cl, 123)
val2 <- pgamsim(cl, 10000, 50, 0.2)
identical(val1, val2)
stopCluster(cl)

