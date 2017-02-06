h <- 2^seq(-1,-64,-1)

#1)f_1(x) = sin(x) at x = 1 (cos(x))
f1 <- matrix(NA,length(h),3)
x <- 1
f1.true <- cos(x)
#1.1) forward
f1[,1] <- (sin(x+h)-sin(x))/h 
#1.2) central
f1[,2] <- (sin(x+h)-sin(x-h))/(2*h) 
#1.3) complex
f1[,3] <- Im(sin(complex(real = x, imaginary = h)))/h
#plot
matplot(-log2(h),f1-f1.true,type="l", ylab="error", main="f(x) = sin(x) at x = 1")
legend("bottomleft",legend=c("forward","central","complex"),lty=c(1,2,3),col=c("black","red","green"))



#2)f_2(x) = 10000sin(x) at x = 1 (10000cos(x))
f2 <- matrix(NA,length(h),3)
x <- 1
f2.true <- 10000*cos(x)
#2.1) forward
f2[,1] <- 10000*(sin(x+h)-sin(x))/h 
#2.2) central
f2[,2] <- 10000*(sin(x+h)-sin(x-h))/(2*h) 
#2.3) complex
f2[,3] <- Im(10000*sin(complex(real = x, imaginary = h)))/h
#plot
matplot(-log2(h),f2-f2.true,type="l", ylab="error", main="f(x) = 10000sin(x) at x = 1")
legend("bottomleft",legend=c("forward","central","complex"),lty=c(1,2,3),col=c("black","red","green"))



#3)f_3(x) = tan(x) at x = 1.59 (1/cos(x)^2)
f3 <- matrix(NA,length(h),3)
x <- 1.59
f3.true <- 1/cos(x)^2
#3.1) forward
f3[,1] <- (tan(x+h)-tan(x))/h 
#3.2) central
f3[,2] <- (tan(x+h)-tan(x-h))/(2*h) 
#3.3) complex
f3[,3] <- Im(tan(complex(real = x, imaginary = h)))/h
#plot
matplot(-log2(h),f3-f3.true,type="l", ylab="error", main="f(x) = tan(x) at x = 1.59")
legend("topright",legend=c("forward","central","complex"),lty=c(1,2,3),col=c("black","red","green"))






#4)f_4(x) = phi(x) at x = 0.5 (-x*phi(x))
f4 <- matrix(NA,length(h),3)
x <- 0.5
f4.true <- -x*dnorm(x)
#4.1) forward
f4[,1] <- (dnorm(x+h)-dnorm(x))/h 
#4.2) central
f4[,2] <- (dnorm(x+h)-dnorm(x-h))/(2*h)
#4.3) complex
f4[,3] <- Im(1/sqrt(2*pi)*exp(-0.5*complex(real = x, imaginary = h)^2))/h
matplot(-log2(h),f4-f4.true,type="l", ylab="error", main="f(x) = phi(x) at x = 0.5")
legend("bottomlef",legend=c("forward","central","complex"),lty=c(1,2,3),col=c("black","red","green"))




