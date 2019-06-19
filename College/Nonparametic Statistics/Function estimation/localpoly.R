# Example 1: exp(x) near x=0

x <- seq(from=-1, to=1, by=0.01)
y <- exp(x)

plot(x, y, type='l', lwd=3, ylim=c(0,3))

y.0 <- rep(1, length(x))
y.1 <- y.0 + x
y.2 <- y.1 + (1/2)*x^2
y.3 <- y.2 + (1/6)*x^3

lines(x, y.0, lty=2, col=2)
lines(x, y.1, lty=2, col=3)
lines(x, y.2, lty=2, col=4)
lines(x, y.3, lty=2, col=5)

legend(locator(1), c("exp(x)", "Constant", "Linear", "Quadratic", "Cubic"), 
       lwd=c(3,1,1,1,1), col=1:5, lty=c(1, rep(2,4)))

exp.appr <- data.frame(x, y, y.0, y.1, y.2, y.3)


# Example 2: log(1+x) near x=0

x <- seq(from=-0.5, to=0.5, by=0.01)
y <- log(1+x)

plot(x, y, type='l', lwd=3)

y.0 <- rep(0, length(x))
y.1 <- y.0 + x
y.2 <- y.1 - (1/2)*x^2
y.3 <- y.2 + (1/3)*x^3

lines(x, y.0, lty=2, col=2)
lines(x, y.1, lty=2, col=3)
lines(x, y.2, lty=2, col=4)
lines(x, y.3, lty=2, col=5)

legend(locator(1), c("log(1+x)", "0", "x", "x-(1/2)*x^2", "x-(1/2)*x^2+(1/3)*x^3"), 
       lwd=c(3,1,1,1,1), col=1:5, lty=c(1, rep(2,4)))

log.appr <- data.frame(x, y, y.0, y.1, y.2, y.3)

