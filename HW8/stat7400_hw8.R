# Problem 1 
#(a)
library(mgcv)
n <- 50
sigma <- 0.1
N <- 10^4
i <- c(1:n)
x <- i/(n + 1)

m <- function(x) {
    1 - sin(5 * x)^2 * exp(-4 * x)
}

m.x <- m(x)

# Matrices to store the fitted values
fit.sm <- matrix(NA, N, n)
fit.low <- matrix(NA, N, n)
fit.sup <- matrix(NA, N, n)
fit.loe <- matrix(NA, N, n)
fit.gam <- matrix(NA, N, n)

set.seed(100)
for (j in 1:N) {
    eps <- rnorm(n, 0, sigma)
    y <- m.x + eps
    fit.sm[j, ] <- smooth.spline(x, y)$y
    fit.low[j, ] <- lowess(x, y)$y
    fit.sup[j, ] <- supsmu(x, y)$y
    fit.loe[j, ] <- loess(y ~ x)$fitted
    fit.gam[j, ] <- gam(y ~ s(x))$fitted
}

# Calculate bias
m.true <- matrix(m.x, N, n, byrow = TRUE)
fit.sm.bias <- colMeans(fit.sm - m.true)
fit.low.bias <- colMeans(fit.low - m.true)
fit.sup.bias <- colMeans(fit.sup - m.true)
fit.loe.bias <- colMeans(fit.loe - m.true)
fit.gam.bias <- colMeans(fit.gam - m.true)

# Plot bias
plot(x, fit.sm.bias, ylab = "Bias", main = "Bias of Estimates", 
     ylim = range(c(fit.sm.bias, fit.low.bias, fit.sup.bias, fit.loe.bias, 
                    fit.gam.bias)), type = "l")
lines(x, fit.low.bias, col = "red")
lines(x, fit.sup.bias, col = "blue")
lines(x, fit.loe.bias, col = "green")
lines(x, fit.gam.bias, col = "orange")
abline(0, 0, lty = 3)
legend("bottomright", legend = c("smooth.spline", "lowess", "supsmu", "loess", 
                                 "gam"), lty = 1, 
       col = c("black", "red", "blue", "green", "orange"))

# Calcualte standard error
fit.sm.se <- apply(fit.sm, 2, sd)
fit.low.se <- apply(fit.low, 2, sd)
fit.sup.se <- apply(fit.sup, 2, sd)
fit.loe.se <- apply(fit.loe, 2, sd)
fit.gam.se <- apply(fit.gam, 2, sd)

# Plot standard error
plot(x, fit.sm.se, ylab = "Standard Errors", 
     main = "Standard Errors of Estimates", 
     ylim = range(c(fit.sm.se, fit.low.se, fit.sup.se, fit.loe.se, fit.gam.se)), 
     type = "l")
lines(x, fit.low.se, col = "red")
lines(x, fit.sup.se, col = "blue")
lines(x, fit.loe.se, col = "green")
lines(x, fit.gam.se, col = "orange")
legend("topright", legend = c("smooth.spline", "lowess", "supsmu", "loess", 
                              "gam"), lty = 1, 
       col = c("black", "red", "blue", "green", "orange"))

# (b)
sigma <- seq(0.1, 2, 0.1)
N <- 10^3
fit.sm.amse <- rep(NA, length(sigma))
fit.low.amse <- rep(NA, length(sigma))
fit.sup.amse <- rep(NA, length(sigma))
fit.loe.amse <- rep(NA, length(sigma))
fit.gam.amse <- rep(NA, length(sigma))

# Matrices to store the fitted values
fit.sm <- matrix(NA, N, n)
fit.low <- matrix(NA, N, n)
fit.sup <- matrix(NA, N, n)
fit.loe <- matrix(NA, N, n)
fit.gam <- matrix(NA, N, n)
m.true <- matrix(m.x, N, n, byrow = TRUE)

# Calcualte aMSE
set.seed(100)
for (i in 1:length(sigma)) {
    for (j in 1:N) {
        eps <- rnorm(n, 0, sigma[i])
        y <- m.x + eps
        fit.sm[j, ] <- smooth.spline(x, y)$y
        fit.low[j, ] <- lowess(x, y)$y
        fit.sup[j, ] <- supsmu(x, y)$y
        fit.loe[j, ] <- loess(y ~ x)$fitted
        fit.gam[j, ] <- gam(y ~ s(x))$fitted
    }
    fit.sm.amse[i] <- mean(colMeans((fit.sm - m.true)^2))
    fit.low.amse[i] <- mean(colMeans((fit.low - m.true)^2))
    fit.sup.amse[i] <- mean(colMeans((fit.sup - m.true)^2))
    fit.loe.amse[i] <- mean(colMeans((fit.loe - m.true)^2))
    fit.gam.amse[i] <- mean(colMeans((fit.gam - m.true)^2))
}

# Plot aMSE
plot(sigma, fit.sm.amse, ylab = "aMSE", main = "aMSE of Estimates", 
     ylim = range(c(fit.sm.amse, fit.low.amse, fit.sup.amse, fit.loe.amse, 
                    fit.gam.amse)), type = "l")
lines(sigma, fit.low.amse, col = "red")
lines(sigma, fit.sup.amse, col = "blue")
lines(sigma, fit.loe.amse, col = "green")
lines(sigma, fit.gam.amse, col = "orange")
legend("topleft", legend = c("smooth.spline", "lowess", "supsmu", "loess", 
                             "gam"), lty = 1, 
       col = c("black", "red", "blue", "green", "orange"))

# Problem 2
library(pareto)
thd <- 2
n <- seq(1, 6, 0.1)
# dpareto
dpareto.time <- matrix(NA, length(n), 2)
for (i in 1:length(n)) {
    x <- seq(1, 100, length.out = 10^n[i])
    dpareto.time[i, 1] <- as.list(system.time(dpareto(x, 2, 1)))$elapsed
    dpareto.time[i, 2] <- as.list(system.time(p.dpareto(x, 2, 1, P = thd)))$elapsed
    
}
matplot(n, dpareto.time, type = "l", main = "dpareto Timings", 
        xlab = "log10(n)", ylab = "time")
legend("topleft", legend = c("P=1", "P=2"), lty = c(1, 2), col = c("black", "red"))

# ppareto
ppareto.time <- matrix(NA, length(n), 2)
for (i in 1:length(n)) {
    q <- seq(1, 100, length.out = 10^n[i])
    ppareto.time[i, 1] <- as.list(system.time(ppareto(q, 2, 1)))$elapsed
    ppareto.time[i, 2] <- as.list(system.time(p.ppareto(q, 2, 1, P = thd)))$elapsed
    
}
matplot(n, ppareto.time, type = "l", main = "ppareto Timings", 
        xlab = "log10(n)", ylab = "time")
legend("topleft", legend = c("P=1", "P=2"), lty = c(1, 2), col = c("black", "red"))

# qpareto
n <- seq(1, 6, 0.1)
qpareto.time <- matrix(NA, length(n), 2)
for (i in 1:length(n)) {
    p <- seq(0, 1, length.out = 10^n[i])
    qpareto.time[i, 1] <- as.list(system.time(qpareto(p, 2, 1)))$elapsed
    qpareto.time[i, 2] <- as.list(system.time(p.qpareto(p, 2, 1, P = thd)))$elapsed
    
}
matplot(n, qpareto.time, type = "l", main = "qpareto Timings", 
        xlab = "log10(n)", ylab = "time")
legend("topleft", legend = c("P=1", "P=2"), lty = c(1, 2), col = c("black", "red"))



