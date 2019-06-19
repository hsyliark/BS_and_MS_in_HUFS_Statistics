m.ftn <- function(x) -exp(-20*x^2) + x + x^2 * (x>0)

set.seed(-1)
n <- 100
X <- runif(n, min=-1, max=1)
eps <- rnorm(n, sd=.3)
Y <- m.ftn(X) + eps

par(mfrow=c(1,2))
plot(X, Y, ylim=c(-2,2))
x <- seq(from=-1, to=1, by=0.0025)
plot(x, m.ftn(x), type='l', xlab="x", ylab="", ylim=c(-2,2))

K <- function(x) 
  0.5*(abs(x)<=1)
#  0.75*(1-x*x)*(abs(x)<=1) 
#  0.9375*(1-x*x)*(1-x*x)*(abs(x)<=1) 
#  dnorm(x)
  
# Nadaraya-Watson estimate

NW <- nadaraya.watson(x, X, Y, K, h=0.25)

par(mfrow=c(1,1))
plot(X, Y)
lines(NW, col=2, lwd=3)
lines(x, m.ftn(x), lty=2, lwd=3, col="lightgray")

h.lb <- 0.0005*n^(-1/5)
h.ub <- 5*n^(-1/5)
h.nw.cv <- optimize(f=nw.cv, interval=c(h.lb, h.ub), X=X, Y=Y, K=K)
h <- h.nw.cv$minimum
print(h)

NW <- nadaraya.watson(x, X, Y, K, h)
plot(X, Y)
lines(NW, col=2, lwd=3)
lines(x, m.ftn(x), lty=2, lwd=3, col="lightgray")

# Local linear estimate

h.ll.cv <- optimize(f=ll.cv, interval=c(h.lb, h.ub), X=X, Y=Y, K=K)
h <- h.ll.cv$minimum
print(h)

LL <- local.linear(x, X, Y, K, h)
plot(X, Y)
lines(LL, col=3, lwd=3)
lines(x, m.ftn(x), lty=2, lwd=3, col="lightgray")
lines(NW, col=2, lwd=3)
