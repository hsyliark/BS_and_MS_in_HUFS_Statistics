################# density estimation #########################


library(MASS)
data(geyser)
x=geyser$duration
xd=density(x,kernel="gaussian")
xd1=density(x,kernel="epanechnikov")
xd2=density(x,kernel="rectangular")

plot(xd,main="Gaussian vs Epanechnikov vs Uniform",xlab="",lwd=3)
lines(xd1$x,xd1$y,col=4,lwd=3,lty=2)
lines(xd2$x,xd2$y,col=2,lwd=3,lty=3)
legend(0,0.5,c("Gaussian","Epanechnikov","Uniform"), col=c(1,4,2),lty=c(1,2,3),lwd=3)


xd1=density(x,kernel="epanechnikov",bw=0.1)
xd2=density(x,kernel="epanechnikov")
xd3=density(x,kernel="epanechnikov",bw=0.8)

plot(xd1,main="several bandwidths",col=4,xlab="",lwd=3,lty=2,xlim=c(-0.5,6))
lines(xd2$x,xd2$y,col=1,lwd=3,lty=1)
lines(xd3$x,xd3$y,col=2,lwd=3,lty=3)
legend(-0.5,0.8,c("h=0.1","h=0.33","h=0.8"), col=c(4,1,2),lty=c(2,1,3),lwd=3)



################# regression function estimation #########################


library("np")

data(cps71)
attach(cps71)

#### local constant (NW) estimation #######
#### epanechnikov kernel #####

h=4		# bandwidth
x=seq(21,65,0.01)	# x-grid 
kerh <- function(x,y,h){0.75*(1-((x-y)/h)^2)*I(abs(x-y)<h)} # define kernel
    
temp <- outer(age,x,FUN="kerh",h)			# n by gridsize matrix consisting of kernel weights
mhat <- colSums(temp*logwage)/colSums(temp)		# NW estimates

plot(age,logwage)
lines(x,mhat,col=2)

#### local constant (NW) and local linear estimation  by "npreg" function in "np" package #######
#### gaussian kernel #####

library(np)

plot(age,logwage)
lc <- npreg(logwage ~ age, regtype = "lc",bws=4)
lines(age,lc$mean,col=2)
ll <- npreg(logwage ~ age, regtype = "ll",bws=4)
lines(age,ll$mean,col=4)


####### example ###################

x=c(1,1,2,2,3,3,3.5,4)
y=c(1,2,2,3,2,4,3,3)
plot(x,y,pch=16,cex=2,ylim=c(0,4))
library(np)

h=1		# bandwidth
xg=c(2.5,3)	# x-grid 
kerh <- function(x,y,h){0.75*(1-((x-y)/h)^2)*I(abs(x-y)<h)}
    
temp <- outer(x,xg,FUN="kerh",h)			# n by gridsize matrix consisting of kernel weights
mhat1 <- colSums(temp*y)/colSums(temp)	

h=1.5		# bandwidth
xg=c(2.5,3)	# x-grid 
kerh <- function(x,y,h){0.75*(1-((x-y)/h)^2)*I(abs(x-y)<h)}
    
temp <- outer(x,xg,FUN="kerh",h)			# n by gridsize matrix consisting of kernel weights
mhat2 <- colSums(temp*y)/colSums(temp)	