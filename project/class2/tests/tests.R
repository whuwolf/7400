library(class2)
library(MASS)
set.seed(123)
mu0 <- c(1,2)
mu1 <- c(3,4)
Sigma <- diag(c(1,4))
train <- 20
test <- 500
R <- 10

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