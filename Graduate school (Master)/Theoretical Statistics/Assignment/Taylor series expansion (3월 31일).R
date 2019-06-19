# Taylor series expansion of e^x

x <- seq(-10,10,0.001)
f <- exp(x)
g0 <- x^0
g1 <- 1+x
g2 <- 1+x+(x^2)/2
g3 <- 1+x+(x^2)/2+(x^3)/6
g4 <- 1+x+(x^2)/2+(x^3)/6+(x^4)/24
g5 <- 1+x+(x^2)/2+(x^3)/6+(x^4)/24+(x^5)/120

plot(x,f,main="Taylor series expansion of e^x",xlim=c(-10,10),ylim=c(0,10))
lines(x,g0,col="red",lwd=1)
lines(x,g1,col="blue",lwd=1)
lines(x,g2,col="green",lwd=1)
lines(x,g3,col="yellow",lwd=1)
lines(x,g4,col="orange",lwd=1)
lines(x,g5,col="brown",lwd=1)