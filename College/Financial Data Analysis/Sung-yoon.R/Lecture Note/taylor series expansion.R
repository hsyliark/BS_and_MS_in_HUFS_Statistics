x <- seq(from=-0.5,to=0.5,by=0.001)
plot(x,log(1+x),type="l")
abline(0,1,col=2)
lines(x,x-(1/2)*x^2,col=3)
lines(x,x-(1/2)*x^2+(1/3)*x^3,col=4)
lines(x,x-(1/2)*x^2+(1/3)*x^3-(1/4)*x^4,col=5)
