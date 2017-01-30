x1 <- seq(0,1,0.01)
x2 <- seq(1.01,10,0.01)
y1 <- dpareto(x1,1,1)
y2 <- dpareto(x2,1,1)

x3 <- seq(0,2,0.01)
x4 <- seq(2.01,10,0.01)
y3 <- dpareto(x3,2,3)
y4 <- dpareto(x4,2,3)


plot(x1,y1,xlim=range(c(x1,x2,x3,x4)),ylim=range(c(y1,y2,y3,y4)),type="l",col="red",xlab="x",ylab="density",main="Pareto Distribution")
lines(x2,y2,col="red")
lines(x3,y3,col="blue",lty=2)
lines(x4,y4,col="blue",lty=2)
legend("topright",legend=c("scale=1,shape=1","scale=2,shape=3"),lty=c(1,2),col=c("red","blue"))
