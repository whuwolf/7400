h <- 2^seq(-1,-64,-1)
t <- c(2,10)
r <- matrix(NA,length(h),length(t))
#1)f_1(x) = sin(x) at x = 1 (cos(x))
f1 <- matrix(NA,length(h),2)
x <- 1
f1.true <- cos(x)

#1.1) forward
f1[,1] <- (sin(x+h)-sin(x))/h
for (i in 1:length(t)){
    f1.t <- (sin(x+h/t[i])-sin(x))/(h/t[i])
    r[,i] <- f1.t + (f1.t-f1[,1])/(t[i]-1)
}
matplot(-log2(h),r-f1.true,type="l", ylab="error", main="f(x) = sin(x) at x = 1")
lines(-log2(h), f1[,1]-f1.true, lty = 3, col = "green")
legend("bottomleft",legend=c("t=2","t=10","original forward"),lty=c(1,2,3),col=c("black","red","green"))

matplot(-log2(h[10:30]),r[10:30,]-f1.true,type="l", ylab="error", main="f(x) = sin(x) at x = 1")
lines(-log2(h[10:30]), f1[10:30,1]-f1.true, lty = 3, col = "green")
legend("bottomleft",legend=c("t=2","t=10","original forward"),lty=c(1,2,3),col=c("black","red","green"))

#1.2) central
f1[,2] <- (sin(x+h)-sin(x-h))/(2*h) 
for (i in 1:length(t)){
    f1.t <- (sin(x+h/t[i])-sin(x-h/t[i]))/(2*h/t[i])
    r[,i] <- f1.t + (f1.t-f1[,2])/(t[i]^2-1)
}
matplot(-log2(h),r-f1.true,type="l", ylab="error", main="f(x) = sin(x) at x = 1")
lines(-log2(h), f1[,2]-f1.true, lty = 3, col = "green")
legend("bottomleft",legend=c("t=2","t=10","original central"),lty=c(1,2,3),col=c("black","red","green"))

matplot(-log2(h[10:30]),r[10:30,]-f1.true,type="l", ylab="error", main="f(x) = sin(x) at x = 1")
lines(-log2(h[10:30]), f1[10:30,2]-f1.true, lty = 3, col = "green")
legend("topleft",legend=c("t=2","t=10","original central"),lty=c(1,2,3),col=c("black","red","green"))


#2)f_2(x) = 10000sin(x) at x = 1 (10000cos(x))
f2 <- matrix(NA,length(h),2)
x <- 1
f2.true <- 10000*cos(x)
#2.1) forward
f2[,1] <- 10000*(sin(x+h)-sin(x))/h
for (i in 1:length(t)){
    f2.t <- 10000*(sin(x+h/t[i])-sin(x))/(h/t[i])
    r[,i] <- f2.t + (f2.t-f2[,1])/(t[i]-1)
}
matplot(-log2(h),r-f2.true,type="l", ylab="error", main="f(x) = 10000sin(x) at x = 1")
lines(-log2(h), f2[,1]-f2.true, lty = 3, col = "green")
legend("bottomleft",legend=c("t=2","t=10","original forward"),lty=c(1,2,3),col=c("black","red","green"))

matplot(-log2(h[10:30]),r[10:30,]-f2.true,type="l", ylab="error", main="f(x) = 10000sin(x) at x = 1")
lines(-log2(h[10:30]), f2[10:30,1]-f2.true, lty = 3, col = "green")
legend("bottomleft",legend=c("t=2","t=10","original forward"),lty=c(1,2,3),col=c("black","red","green"))


#2.2) central
f2[,2] <- 10000*(sin(x+h)-sin(x-h))/(2*h) 
for (i in 1:length(t)){
    f2.t <- 10000*(sin(x+h/t[i])-sin(x-h/t[i]))/(2*h/t[i])
    r[,i] <- f2.t + (f2.t-f2[,2])/(t[i]^2-1)
}
matplot(-log2(h),r-f2.true,type="l", ylab="error", main="f(x) = 10000sin(x) at x = 1")
lines(-log2(h), f2[,2]-f2.true, lty = 3, col = "green")
legend("bottomleft",legend=c("t=2","t=10","original central"),lty=c(1,2,3),col=c("black","red","green"))

matplot(-log2(h[10:30]),r[10:30,]-f2.true,type="l", ylab="error", main="f(x) = 10000sin(x) at x = 1")
lines(-log2(h[10:30]), f2[10:30,2]-f2.true, lty = 3, col = "green")
legend("topleft",legend=c("t=2","t=10","original central"),lty=c(1,2,3),col=c("black","red","green"))


#3)f_3(x) = tan(x) at x = 1.59 (1/cos(x)^2)
f3 <- matrix(NA,length(h),2)
x <- 1.59
f3.true <- 1/cos(x)^2
#3.1) forward
f3[,1] <- (tan(x+h)-tan(x))/h 
for (i in 1:length(t)){
    f3.t <- (tan(x+h/t[i])-tan(x))/(h/t[i])
    r[,i] <- f3.t + (f3.t-f3[,1])/(t[i]-1)
}
matplot(-log2(h),r-f3.true,type="l", ylab="error", main="f(x) = tan(x) at x = 1.59")
lines(-log2(h), f3[,1]-f3.true, lty = 3, col = "green")
legend("bottomleft",legend=c("t=2","t=10","original forward"),lty=c(1,2,3),col=c("black","red","green"))

matplot(-log2(h[15:40]),r[15:40,]-f3.true,type="l", ylab="error", main="f(x) = tan(x) at x = 1.59")
lines(-log2(h[15:40]), f3[15:40,1]-f3.true, lty = 3, col = "green")
legend("topleft",legend=c("t=2","t=10","original forward"),lty=c(1,2,3),col=c("black","red","green"))

#3.2) central
f3[,2] <- (tan(x+h)-tan(x-h))/(2*h) 
for (i in 1:length(t)){
    f3.t <- (tan(x+h/t[i])-tan(x-h/t[i]))/(2*h/t[i])
    r[,i] <- f3.t + (f3.t-f3[,2])/(t[i]^2-1)
}
matplot(-log2(h),r-f3.true,type="l", ylab="error", main="f(x) = tan(x) at x = 1.59")
lines(-log2(h), f3[,2]-f3.true, lty = 3, col = "green")
legend("topright",legend=c("t=2","t=10","original central"),lty=c(1,2,3),col=c("black","red","green"))

matplot(-log2(h[10:30]),r[10:30,]-f3.true,type="l", ylim = range(-0.005,0.005), ylab="error", main="f(x) = tan(x) at x = 1.59")
lines(-log2(h[10:30]), f3[10:30,2]-f3.true, lty = 3, col = "green")
legend("topright",legend=c("t=2","t=10","original central"),lty=c(1,2,3),col=c("black","red","green"))


#4)f_4(x) = phi(x) at x = 0.5 (-x*phi(x))
f4 <- matrix(NA,length(h),2)
x <- 0.5
f4.true <- -x*dnorm(x)
#4.1) forward
f4[,1] <- (dnorm(x+h)-dnorm(x))/h
for (i in 1:length(t)){
    f4.t <- (dnorm(x+h/t[i])-dnorm(x))/(h/t[i])
    r[,i] <- f4.t + (f4.t-f4[,1])/(t[i]-1)
}
matplot(-log2(h),r-f4.true,type="l", ylab="error", main="f(x) = phi(x) at x = 0.5")
lines(-log2(h), f4[,1]-f4.true, lty = 3, col = "green")
legend("bottomleft",legend=c("t=2","t=10","original forward"),lty=c(1,2,3),col=c("black","red","green"))

matplot(-log2(h[10:30]),r[10:30,]-f4.true,type="l", ylab="error", main="f(x) = phi(x) at x = 0.5")
lines(-log2(h[10:30]), f4[10:30,1]-f4.true, lty = 3, col = "green")
legend("bottomleft",legend=c("t=2","t=10","original forward"),lty=c(1,2,3),col=c("black","red","green"))

#4.2) central
f4[,2] <- (dnorm(x+h)-dnorm(x-h))/(2*h)
for (i in 1:length(t)){
    f4.t <- (dnorm(x+h/t[i])-dnorm(x-h/t[i]))/(2*h/t[i])
    r[,i] <- f4.t + (f4.t-f4[,2])/(t[i]^2-1)
}
matplot(-log2(h),r-f4.true,type="l", ylab="error", main="f(x) = phi(x) at x = 0.5")
lines(-log2(h), f4[,2]-f4.true, lty = 3, col = "green")
legend("topleft",legend=c("t=2","t=10","original central"),lty=c(1,2,3),col=c("black","red","green"))

matplot(-log2(h[10:30]),r[10:30,]-f4.true,type="l", ylab="error", main="f(x) = phi(x) at x = 0.5")
lines(-log2(h[10:30]), f4[10:30,2]-f4.true, lty = 3, col = "green")
legend("bottomleft",legend=c("t=2","t=10","original central"),lty=c(1,2,3),col=c("black","red","green"))





