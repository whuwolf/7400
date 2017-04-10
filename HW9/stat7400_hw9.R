# Problem 1 (a)
library(mgcv)
library(rpart)
library(gbm)
library(randomForest)
library(nnet)
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
fit.gam <- matrix(NA, N, n)
fit.rp <- matrix(NA, N, n)
fit.gbm <- matrix(NA, N, n)
fit.rf <- matrix(NA, N, n)
fit.nnet <- matrix(NA, N, n)


set.seed(100)
for (j in 1:N) {
    eps <- rnorm(n, 0, sigma)
    y <- m.x + eps
    data <- as.data.frame(cbind(y, x))
    fit.gam[j, ] <- gam(y ~ s(x))$fitted
    fit.rp[j, ] <- predict(rpart(y ~ x), data)
    fit.gbm[j, ] <- predict(gbm(y ~ x, n.trees = 5000), data, n.trees = 5000)
    fit.rf[j, ] <- predict(randomForest(y ~ x), data)
    fit.nnet[j, ] <- predict(nnet(y ~ x, size = 10, entropy = TRUE, 
                                  decay = 0.001, maxit = 300), data)
}

# Calculate average of estimator
fit.gam.avg <- colMeans(fit.gam)
fit.rp.avg <- colMeans(fit.rp)
fit.gbm.avg <- colMeans(fit.gbm)
fit.rf.avg <- colMeans(fit.rf)
fit.nnet.avg <- colMeans(fit.nnet)


# Calculate bias
fit.gam.bias <- fit.gam.avg - m.x
fit.rp.bias <- fit.rp.avg - m.x
fit.gbm.bias <- fit.gbm.avg - m.x
fit.rf.bias <- fit.rf.avg - m.x
fit.nnet.bias <- fit.nnet.avg - m.x

# Plot bias
plot(x, fit.gam.bias, ylab = "Bias", main = "Bias of Estimates", 
     ylim = range(c(fit.gam.bias, fit.rp.bias, fit.gbm.bias, fit.rf.bias, 
                    fit.nnet.bias)), type = "l")
lines(x, fit.rp.bias, col = "red")
lines(x, fit.gbm.bias, col = "blue")
lines(x, fit.rf.bias, col = "green")
lines(x, fit.nnet.bias, col = "orange")
abline(0, 0, lty = 3)
legend("bottomright", legend = c("gam", "rpart", "gbm", "randomForest", "nnet"), 
       lty = 1, col = c("black", "red", "blue", "green", "orange"))

# Calcualte standard error
fit.gam.se <- apply(fit.gam, 2, sd)/sqrt(N)
fit.rp.se <- apply(fit.rp, 2, sd)/sqrt(N)
fit.gbm.se <- apply(fit.gbm, 2, sd)/sqrt(N)
fit.rf.se <- apply(fit.rf, 2, sd)/sqrt(N)
fit.nnet.se <- apply(fit.nnet, 2, sd)/sqrt(N)

# Plot standard error
plot(x, fit.gam.se, ylab = "Standard Errors", 
     main = "Standard Errors of Estimates", 
     ylim = range(c(fit.gam.se, fit.rp.se, fit.gbm.se, fit.rf.se, fit.nnet.se)), 
     type = "l")
lines(x, fit.rp.se, col = "red")
lines(x, fit.gbm.se, col = "blue")
lines(x, fit.rf.se, col = "green")
lines(x, fit.nnet.se, col = "orange")
legend("topright", legend = c("gam", "rpart", "gbm", "randomForest", "nnet"), 
       lty = 1, col = c("black", "red", "blue", "green", "orange"))

# Plot 95% confidence bands gam
plot(x, fit.gam.avg, ylab = "Estimate", main = "95% Confidence Band of gam", 
     ylim = range(m.x), type = "l")
polygon(c(x, rev(x)), c(fit.gam.avg - 2 * fit.gam.se, 
                        rev(fit.gam.avg + 2 * fit.gam.se)), col = "grey", 
        border = NA)
lines(x, m.x, lty = 2)
legend("bottomright", legend = c("gam", "true value"), lty = c(1, 2), 
       col = c("grey", "black"))

# rpart
plot(x, fit.rp.avg, ylab = "Estimate", main = "95% Confidence Band of rpart", 
     ylim = range(m.x), type = "l")
polygon(c(x, rev(x)), c(fit.rp.avg - 2 * fit.rp.se, 
                        rev(fit.rp.avg + 2 * fit.rp.se)), col = "red", 
        border = NA)
lines(x, m.x, lty = 2)
legend("bottomright", legend = c("rpart", "true value"), lty = c(1, 2), 
       col = c("red", "black"))

# gbm
plot(x, fit.gbm.avg, ylab = "Estimate", main = "95% Confidence Band of gbm", 
     ylim = range(m.x), type = "l")
polygon(c(x, rev(x)), c(fit.gbm.avg - 2 * fit.gbm.se, 
                        rev(fit.gbm.avg + 2 * fit.gbm.se)), col = "blue", 
        border = NA)
lines(x, m.x, lty = 2)
legend("bottomright", legend = c("gbm", "true value"), lty = c(1, 2), 
       col = c("blue", "black"))

# randomForest
plot(x, fit.rf.avg, ylab = "Estimate", 
     main = "95% Confidence Band of randomForest", ylim = range(m.x), 
     type = "l")
polygon(c(x, rev(x)), c(fit.rf.avg - 2 * fit.rf.se, 
                        rev(fit.rf.avg + 2 * fit.rf.se)), col = "green", 
        border = NA)
lines(x, m.x, lty = 2)
legend("bottomright", legend = c("randomForest", "true value"), lty = c(1, 2), 
       col = c("green", "black"))

# nnet
plot(x, fit.nnet.avg, ylab = "Estimate", main = "95% Confidence Band of nnet", 
     ylim = range(m.x), type = "l")
polygon(c(x, rev(x)), c(fit.nnet.avg - 2 * fit.nnet.se, 
                        rev(fit.nnet.avg + 2 * fit.nnet.se)), col = "orange", 
        border = NA)
lines(x, m.x, lty = 2)
legend("bottomright", legend = c("nnet", "true value"), lty = c(1, 2), 
       col = c("orange", "black"))

# (b)
sigma <- seq(0.1, 2, 0.1)
N <- 10^4
fit.gam.amse <- rep(NA, length(sigma))
fit.rp.amse <- rep(NA, length(sigma))
fit.gbm.amse <- rep(NA, length(sigma))
fit.rf.amse <- rep(NA, length(sigma))
fit.nnet.amse <- rep(NA, length(sigma))

# Matrices to store the fitted values
fit.gam <- matrix(NA, N, n)
fit.rp <- matrix(NA, N, n)
fit.gbm <- matrix(NA, N, n)
fit.rf <- matrix(NA, N, n)
fit.nnet <- matrix(NA, N, n)
m.true <- matrix(m.x, N, n, byrow = TRUE)

# Calcualte aMSE
set.seed(100)
for (i in 1:length(sigma)) {
    for (j in 1:N) {
        eps <- rnorm(n, 0, sigma[i])
        y <- m.x + eps
        data <- as.data.frame(cbind(y, x))
        fit.gam[j, ] <- gam(y ~ s(x))$fitted
        fit.rp[j, ] <- predict(rpart(y ~ x), data)
        fit.gbm[j, ] <- predict(gbm(y ~ x, n.trees = 5000), data, 
                                n.trees = 5000)
        fit.rf[j, ] <- predict(randomForest(y ~ x), data)
        fit.nnet[j, ] <- predict(nnet(y ~ x, size = 10, entropy = TRUE, 
                                      decay = 0.001, maxit = 300), data)
    }
    fit.gam.amse[i] <- mean(colMeans((fit.gam - m.true)^2))
    fit.rp.amse[i] <- mean(colMeans((fit.rp - m.true)^2))
    fit.gbm.amse[i] <- mean(colMeans((fit.gbm - m.true)^2))
    fit.rf.amse[i] <- mean(colMeans((fit.rf - m.true)^2))
    fit.nnet.amse[i] <- mean(colMeans((fit.nnet - m.true)^2))
}

# Plot aMSE
plot(sigma, fit.gam.amse, ylab = "aMSE", main = "aMSE of Estimates", 
     ylim = range(c(fit.gam.amse, fit.rp.amse, fit.gbm.amse, fit.rf.amse, 
                    fit.nnet.amse)), type = "l")
lines(sigma, fit.rp.amse, col = "red")
lines(sigma, fit.gbm.amse, col = "blue")
lines(sigma, fit.rf.amse, col = "green")
lines(sigma, fit.nnet.amse, col = "orange")
legend("topleft", legend = c("gam", "rpart", "gbm", "randomForest", "nnet"), 
       lty = 1, col = c("black", "red", "blue", "green", "orange"))


