# Problem 1 
#(a)############################################################################
library(pareto)

rpareto(1,3,-2)
rpareto(1,3,2)
rpareto(4,c(100,1),2)

#Check reproducability
set.seed(100)
a <- rpareto(c(NA,1,99), 3, 2)
set.seed(100)
b <- rpareto(3, 3, 2)
all.equal(a,b)
#Check qq-plot
qqplot(ppareto(rpareto(10^6,3,2),3,2), seq(0,1, length.out = 10^6), type = "l", 
       main = "QQ plot of rpareto")

#(b)############################################################################
rcpareto(1,3,-2)
rcpareto(1,3,2)
rcpareto(4,c(100,1),2)

#Check reproducability
set.seed(100)
a <- rcpareto(c(NA,1,99), 3, 2)
set.seed(100)
b <- rcpareto(3, 3, 2)
all.equal(a,b)
#Check qq-plot
qqplot(ppareto(rcpareto(10^6,3,2),3,2), seq(0,1, length.out = 10^6), type = "l", 
       main = "QQ plot of rcpareto")

#Compare efficiency of two functions
system.time(rpareto(10^6, 3, 2))
system.time(rcpareto(10^6, 3, 2))

