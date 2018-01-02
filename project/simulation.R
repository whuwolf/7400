#Data scenario 1################################################################
#Independent Normal
set.seed(123)
mu0 <- c(1,2)
mu1 <- c(3,4)
Sigma <- diag(c(1,4))
train <- 20
test <- 500
R <- 1000

#Comput test set
X0 <- mvrnorm(test, mu0, Sigma)
X1 <- mvrnorm(test, mu1, Sigma)
X <- rbind(X0, X1)
y <- c(rep(0, test), rep(1, test))

normsim <- function(train, Test, label){
    X0 <- mvrnorm(train, mu0, Sigma)
    X1 <- mvrnorm(train, mu1, Sigma)
    X <- rbind(X0, X1)
    y <- c(rep(0, train), rep(1, train))
    
    m1 <- classify(y, X, method = "logistic")
    m2 <- classify(y, X, method = "lda")
    m3 <- classify(y, X, method = "naiveBayes", indCont = c(1,2))
    
    y1 <- classify.predict(m1, Test)$class
    y2 <- classify.predict(m2, Test)$class
    y3 <- classify.predict(m3, Test)$class
    
    c(mean(y1!=label), mean(y2!=label), mean(y3!=label))
}

err1 <- colMeans(do.call(rbind, lapply(c(1:R), function(i){normsim(train, X, y)})))

#Data scenario 2################################################################
#Correlated Normal
set.seed(123)
mu0 <- c(1,2)
mu1 <- c(3,4)

#rho = 0.5
Sigma <- cbind(c(1,1), c(1,4))

train <- 20
test <- 500
R <- 1000

#Comput test set
X0 <- mvrnorm(test, mu0, Sigma)
X1 <- mvrnorm(test, mu1, Sigma)
X <- rbind(X0, X1)
y <- c(rep(0, test), rep(1, test))

err2 <- colMeans(do.call(rbind, lapply(c(1:R), function(i){normsim(train, X, y)})))



#Data scenario 3################################################################
#Independent t distribution
set.seed(123)
mu0 <- c(1,2)
mu1 <- c(3,4)
df <- c(1, 2)

train <- 20
test <- 500
R <- 1000

#Comput test set
X0 <- cbind(mu0[1] + rt(test, df[1]), mu0[2] + rt(test, df[2]))
X1 <- cbind(mu1[1] + rt(test, df[1]), mu1[2] + rt(test, df[2]))
X <- rbind(X0, X1)
y <- c(rep(0, test), rep(1, test))

tsim <- function(train, Test, label){
    X0 <- cbind(mu0[1] + rt(train, df[1]), mu0[2] + rt(train, df[2]))
    X1 <- cbind(mu1[1] + rt(train, df[1]), mu1[2] + rt(train, df[2]))
    X <- rbind(X0, X1)
    y <- c(rep(0, train), rep(1, train))
    
    m1 <- classify(y, X, method = "logistic")
    m2 <- classify(y, X, method = "lda")
    m3 <- classify(y, X, method = "naiveBayes", indCont = c(1,2))
    
    y1 <- classify.predict(m1, Test)$class
    y2 <- classify.predict(m2, Test)$class
    y3 <- classify.predict(m3, Test)$class
    
    c(mean(y1!=label), mean(y2!=label), mean(y3!=label))
}

err3 <- colMeans(do.call(rbind, lapply(c(1:R), function(i){tsim(train, X, y)})))


#Data scenario 4################################################################
#Independent Normal and Bernoulli
set.seed(123)
mu0 <- 0
mu1 <- 2
sigma <- 1
p <- 0.8

train <- 20
test <- 500
R <- 1000

#Comput test set
X0 <- cbind(rnorm(test, mu0, sigma), rbinom(test, 1, p))
X1 <- cbind(rnorm(test, mu1, sigma), rbinom(test, 1, 1-p))
X <- rbind(X0, X1)
y <- c(rep(0, test), rep(1, test))

mixIndsim <- function(train, Test, label){
    X0 <- cbind(rnorm(train, mu0, sigma), rbinom(train, 1, p))
    X1 <- cbind(rnorm(train, mu1, sigma), rbinom(train, 1, 1-p))
    X <- rbind(X0, X1)
    y <- c(rep(0, train), rep(1, train))
    
    m1 <- classify(y, X, method = "logistic")
    m2 <- classify(y, X, method = "lda")
    m3 <- classify(y, X, method = "naiveBayes", indCont = 1)
    
    y1 <- classify.predict(m1, Test)$class
    y2 <- classify.predict(m2, Test)$class
    y3 <- classify.predict(m3, Test)$class
    
    c(mean(y1!=label), mean(y2!=label), mean(y3!=label))
}

err4 <- colMeans(do.call(rbind, lapply(c(1:R), function(i){mixIndsim(train, X, y)})))


#Data scenario 5
#Correlated Normal and categorical

set.seed(123)

mu0 <- 0
mu1 <- 2
sigma <- 1
p <- 0.8

train <- 20
test <- 500
R <- 1000

#Comput test set
rho <- -0.5 # correlation coefficient
sigma.c <- matrix(c(1,rho,rho,1), ncol = 2)
s <- chol(sigma.c)

z0 <- s%*%matrix(rnorm(test*2), nrow = 2)
u0 <- pnorm(z0)
X0 <- cbind(qnorm(u0[1, ], mu0, sigma), ifelse(u0[2, ] > p, 0, 1))

z1 <- s%*%matrix(rnorm(test*2), nrow = 2)
u1 <- pnorm(z1)
X1 <- cbind(qnorm(u1[1, ], mu1, sigma), ifelse(u1[2, ] <= p, 0, 1))

X <- rbind(X0, X1)
y <- c(rep(0, test), rep(1, test))

mixCorsim <- function(train, Test, label){
    z0 <- s%*%matrix(rnorm(train*2), nrow = 2)
    u0 <- pnorm(z0)
    X0 <- cbind(qnorm(u0[1, ], mu0, sigma), ifelse(u0[2, ] > p, 0, 1))
    
    z1 <- s%*%matrix(rnorm(train*2), nrow = 2)
    u1 <- pnorm(z1)
    X1 <- cbind(qnorm(u1[1, ], mu1, sigma), ifelse(u1[2, ] <= p, 0, 1))
    
    X <- rbind(X0, X1)
    y <- c(rep(0, train), rep(1, train))
    
    m1 <- classify(y, X, method = "logistic")
    m2 <- classify(y, X, method = "lda")
    m3 <- classify(y, X, method = "naiveBayes", indCont = 1)
    
    y1 <- classify.predict(m1, Test)$class
    y2 <- classify.predict(m2, Test)$class
    y3 <- classify.predict(m3, Test)$class
    
    c(mean(y1!=label), mean(y2!=label), mean(y3!=label))
}

err5 <- colMeans(do.call(rbind, lapply(c(1:R), function(i){mixCorsim(train, X, y)})))







