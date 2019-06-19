## Ex 5.6

x <- seq(-4, 4, by=0.01)
plot(x, dnorm(x), type="l", 
     main="Density of N(0,1), t(1), t(3), and t(25)",
     xlab="x", ylab="Density")
lines(x, dt(x, df=1), lty=4, lwd=2)
lines(x, dt(x, df=3), lty=3, lwd=2)
lines(x, dt(x, df=25), lty=5, lwd=2)
legend(2.0, 0.4, cex=0.7,
       legend=c('N(0,1)','t(25)','t(3)','t(1)'),
       lty=c(1,5,3,4), lwd=c(1,2,2,2))



## Ex 5.7

data <- rf(200, 10, 15)
hist(data, probability=TRUE, ylim=c(0,1),
     main="F(10,15) 자료의 히스토그램과 확률밀도함수",
     ylab="Density", xlab="x")
xx <- seq(0, 5.5, by=0.01)
lines(xx, df(xx,10,15), lty=5, lwd=2)



## Ex 5.8

x <- seq(from=-5, to=5, by=1)
ifelse(x<0, 0, x)



## Ex 5.9

# Solution of (1)
ans <- 0
for (i in 1:100) {
  ans <- ans + i
}
ans

# Another solution of (1)
x <- seq(from=1, to=100, by=1)
ans1 <- 0
for (i in 1:100) {
  ans1 <- ans1+x[i]
}
ans1

# Solution of (2)
sqans <- 0
for (i in 1:10) {
  sqans <- sqans + i^2
}
sqans

# Solution of (2) using while()
x <- 1
sqans1 <- 0
while(x <= 10) {
  sqans1 <- sqans1 + x^2
  x <- x+1
}
sqans1

# Solution of (2) using sum()
sum((1:10)^2)