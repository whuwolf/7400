#Problem 2
#(a)
readVol <- function(fname, dim = c(181, 217, 181)) {
    f<-gzfile(fname, open="rb")
    on.exit(close(f))
    b<-readBin(f,"integer",prod(dim),size = 1, signed = FALSE)
    array(b, dim)
}
T1 <- readVol("t1_icbm_normal_1mm_pn3_rf20.rawb")
x <- T1[91, , ]
y <- T1[, 109, ]
z <- T1[, , 91]
image(z = x, main = "Middle slice along with X-axis", 
      col = gray((0 : 255) / 255))
image(z = y, main = "Middle slice along with Y-axis",
      col = gray((0 : 255) / 255))
image(z = z, main = "Middle slice along with Z-axis",
      col = gray((0 : 255) / 255))
#(b)
mask <- readVol("mask.rawb")
T1.mask <- T1[mask == 1]
plot(density(T1.mask), xlab = "Intensity", main = "")

#(c)
EMmix1 <- function(x, theta){
    mu <- theta$mu
    sigma <- theta$sigma
    p <- theta$p
    M <- length(mu)
    
    ## E step
    Ez <- outer(x, 1 : M, function(x, i) p[i] * dnorm(x, mu[i], sigma[i]))
    Ez <- sweep(Ez, 1, rowSums(Ez), "/")
    colSums.Ez <- colSums(Ez)
    
    ## M step
    xp <- sweep(Ez, 1, x, "*")
    mu.new <- colSums(xp) / colSums.Ez
    
    sqRes <- outer(x, mu.new, function(x, m) (x - m) ^ 2)
    sigma.new <- sqrt(colSums(Ez * sqRes) / colSums.Ez)
    
    p.new <- colSums.Ez / sum(colSums.Ez)
    ## pack up result
    list(mu = mu.new, sigma = sigma.new, p = p.new)
}

diff <- function(x, y){
    max(abs(x - y)) / (1 + max(abs(x), abs(y)))
}

converge.check <- function(theta.new, theta,
                       err, iter, iter.max){
    diff.mu <- diff(theta.new$mu, theta$mu)
    diff.sigma <- diff(theta.new$sigma, theta$sigma)
    diff.p <- diff(theta.new$p, theta$p)
    flag <- 0
    if (diff.mu < err[1] && diff.sigma < err[2] 
        && diff.p < err[3] || iter > iter.max)
        flag <- 1
    flag
}

EMmix <- function(x, theta, err, iter.max){
    iter <- 0
    repeat {
        iter <- iter + 1
        theta.new <- EMmix1(x, theta)
        flag <- converge.check(theta.new, theta, err, iter, iter.max)
        if (flag) break
        theta <- theta.new
    }
    theta.new
}

library(mritc)
init <- initOtsu(T1.mask, 2)
theta <- list(mu=init$mu, sigma = init$sigma, p = init$prop)
err <- rep(1e-08, 3)
iter.max <- 200
theta.est1 <- EMmix(T1.mask, theta, err, iter.max)
data.frame(mu = theta.est1$mu, sigma = theta.est1$sigma, p = theta.est1$p)

#(d)
classify <- function(x, theta){
    mu <- theta$mu
    sigma <- theta$sigma
    p <- theta$p
    M <- length(mu)
    Ez <- outer(x, 1 : M, function(x, i) p[i] * dnorm(x, mu[i], sigma[i]))
    max.col(Ez, ties.method = "first")
}
T1.class <- array(0, dim = c(181, 217, 181))
T1.class[mask == 1] <- classify(T1.mask, theta)
x.class <- T1.class[91, , ]
y.class <- T1.class[, 109, ]
z.class <- T1.class[, , 91]
image(z = x.class, main = "Middle slice along with 
      X-axis", col = gray((0 : 255) / 255))
image(z = y.class, main = "Middle slice along with 
      Y-axis", col = gray((0 : 255) / 255))
image(z = z.class, main = "Middle slice along with 
      Z-axis", col = gray((0 : 255) / 255))

#(e)
Rprof(tmp <- tempfile())
theta.est1 <- EMmix(T1.mask, theta, err, iter.max)
Rprof()
summaryRprof(tmp)

#(f)
EMmix1c <- function(x.c, c, theta){
    mu <- theta$mu
    sigma <- theta$sigma
    p <- theta$p
    M <- length(mu)
    ## E step
    Ez <- outer(x.c, 1 : M, function(x, i) p[i] * 
                    dnorm(x, mu[i], sigma[i]))
    Ez <- sweep(Ez, 1, rowSums(Ez), "/")
    Ez.c <- sweep(Ez, 1, c, "*")
    colSums.Ez.c <- colSums(Ez.c)
    ## M step
    xp.c <- sweep(Ez.c, 1, x.c, "*")
    mu.new <- colSums(xp.c) / colSums.Ez.c
    sqRes.c <- outer(x.c, mu.new, function(x, m) (x - m) ^ 2)
    sigma.new <- sqrt(colSums(Ez.c * sqRes.c) / colSums.Ez.c)
    p.new <- colSums.Ez.c / sum(colSums.Ez.c)
    ## pack up result
    list(mu = mu.new, sigma = sigma.new, p = p.new)
}

EMmixc <- function(x, theta, err, iter.max){
    iter <- 0
    c <- table(x)
    x.c <- sort(unique(x))
    repeat {
        iter <- iter + 1
        theta.new <- EMmix1c(x.c, c, theta)
        flag <- converge.check(theta.new, theta, err, iter, iter.max)
        if (flag) break
        theta <- theta.new
    }
    theta.new
}
init <- initOtsu(T1.mask, 2)
theta <- list(mu=init$mu, sigma = init$sigma, p = init$prop)
err <- rep(1e-08, 3)
iter.max <- 200
theta.est2 <- EMmixc(T1.mask, theta, err, iter.max)
data.frame(mu.c = theta.est2$mu, sigma.c = theta.est2$sigma, 
           p.c = theta.est2$p)

Rprof(tmp <- tempfile())
theta.est2 <- EMmixc(T1.mask, theta, err, iter.max)
Rprof()
summaryRprof(tmp)
