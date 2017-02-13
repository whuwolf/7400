# 1(a)#######################################################
library(gmp)
lm.fit.exact <- function(x, y) {
    x.b <- as.matrix(as.bigq(x))
    y.b <- as.vector(as.bigq(y))
    x.bt <- t(x.b)
    as.double(solve(x.bt %*% x.b, x.bt %*% y.b))
}

# 1(b)#######################################################
lm.fit.chol <- function(x, y, center = FALSE) {
    x <- as.matrix(x)
    y <- as.vector(y)
    if (center) {
        x.m <- colMeans(x)
        x.c <- sweep(x, 2, x.m)
        y.m <- mean(y)
        y.c <- y - y.m
        b <- lm.fit.chol(x.c, y.c)
        c(y.m - crossprod(x.m, b), b)
        
    } else {
        R <- chol(crossprod(x))
        as.vector(backsolve(R, forwardsolve(t(R), crossprod(x, y))))
    }
}

# 1(c)#######################################################
X <- cbind(1, as.matrix(longley[, -7]))
y <- as.vector(longley[, 7])
X.b <- as.bigq(round(1000 * X))/as.bigq(1000)
y.b <- as.bigq(round(1000 * y))/as.bigq(1000)
as.vector(lm.fit.exact(X.b, y.b) - lm.fit(X, y)$coefficients)
lm.fit.exact(X.b, y.b) - lm.fit.chol(X, y)
lm.fit.exact(X.b, y.b) - lm.fit.chol(X[, -1], y, center = TRUE)

# 2(a)#######################################################
covMat <- function(n, a) {
    M <- diag(n)
    M[cbind(2:n, 1:(n - 1))] <- a
    M[cbind(1:(n - 1), 2:n)] <- a
    M
}

loglikD <- function(y, a) {
    y <- as.vector(y)
    C <- covMat(length(y), a)
    R <- chol(C)
    -sum(log(diag(R))) - 0.5 * sum(forwardsolve(t(R), y)^2)
}

# 2(b)#######################################################
library(Matrix)
loglikS <- function(y, a) {
    n <- length(y)
    C <- bandSparse(n, k = c(0, 1), diag = list(rep(1, n), rep(a, n - 1)), symm = TRUE)
    R <- chol(C)
    -sum(log(diag(R))) - 0.5 * sum(solve(t(R), y)^2)
}

# 2(c)#######################################################
#n=9
y <- c(rep(0, 3), rep(1, 3), rep(-1, 3))
system.time(for (i in 1:1000) loglikD(y, 0.5))
system.time(for (i in 1:1000) loglikS(y, 0.5))

#n=30
y <- c(rep(0, 10), rep(1, 10), rep(-1, 10))
system.time(for (i in 1:1000) loglikD(y, 0.5))
system.time(for (i in 1:1000) loglikS(y, 0.5))

#n=180
y <- c(rep(0, 60), rep(1, 60), rep(-1, 60))
system.time(for (i in 1:1000) loglikD(y, 0.5))
system.time(for (i in 1:1000) loglikS(y, 0.5))

#n=300
y <- c(rep(0, 100), rep(1, 100), rep(-1, 100))
system.time(for (i in 1:1000) loglikD(y, 0.5))
system.time(for (i in 1:1000) loglikS(y, 0.5))

