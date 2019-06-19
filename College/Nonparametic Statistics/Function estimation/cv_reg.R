nadaraya.watson <- function(x, X, Y, K, h)
{
	x <- as.vector(x)
	X <- as.vector(X)
	nx <- length(x)
	mx <- x
	eps <- .Machine$double.eps^0.25
	for ( i in 1:nx ) {
		tmp <- (X-x[i])/h
		s0 <- K(tmp)
		s1 <- Y*s0
		mx[i] <- sum(s1)/max(eps, sum(s0))
	}
	return(data.frame(x=x, mx=mx))
}

local.linear <- function(x, X, Y, K, h)
{
	x <- as.vector(x)
	X <- as.vector(X)
	nx <- length(x)
	mx <- x
	eps <- .Machine$double.eps^0.25
	for ( i in 1:nx ) {
		tmp <- (X-x[i])
		s0 <- K(tmp/h); s1 <- tmp*s0; s2 <- tmp*s1
		t0 <- Y*s0; t1 <- tmp*t0
		s0 <- sum(s0); s1 <- sum(s1); s2 <- sum(s2); 
		t0 <- sum(t0); t1 <- sum(t1);
		mx[i] <- (t0*s2-t1*s1)/(max(eps, s0*s2-s1*s1))
	}
	return(data.frame(x=x, mx=mx))
}

nw.cv <- function(h, X, Y, K)
{
	X <- as.vector(X)
	Y <- as.vector(Y)
	n <- length(X)
	if ( n != length(Y) ) {
		print("Error: X and Y are not of the same length.")
		return(0)
	}
	else {
		cv <- 0
		for ( i in 1:n ) 
			cv <- cv+(Y[i]-nadaraya.watson(X[i], X[-i], Y[-i], K, h)$mx)^2
		return(cv/n)		
	}
}

ll.cv <- function(h, X, Y, K)
{
	X <- as.vector(X)
	Y <- as.vector(Y)
	n <- length(X)
	if ( n != length(Y) ) {
		print("Error: X and Y are not of the same length.")
		return(0)
	}
	else {
		cv <- 0
		for ( i in 1:n )
			cv <- cv+(Y[i]-local.linear(X[i], X[-i], Y[-i], K, h)$mx)^2
		return(cv/n)		
	}
}

kernel <- function(x) 
#	0.5*(abs(x)<=1)
#	0.75*(1-x*x)*(abs(x)<=1)
	0.9375*(1-x*x)^2*(abs(x)<=1)
#	dnorm(x)

n <- 100
X <- sort(runif(n))
Y <- sin(2*pi*X)+rnorm(n)*0.2
plot(X, Y, xlim=c(0,1), ylim=c(-1.5, 1.5))

x <- seq(from=0, to=1, by=0.0025)
lines(x, sin(2*pi*x), lty=2)

h.lb <- 0.0005*n^(-1/5)
h.ub <- 5*n^(-1/5)
h.nw.cv <- optimize(f=nw.cv, interval=c(h.lb, h.ub), tol=.Machine$double.eps^0.25, X=X, Y=Y, K=kernel)
h.ll.cv <- optimize(f=ll.cv, interval=c(h.lb, h.ub), tol=.Machine$double.eps^0.25, X=X, Y=Y, K=kernel)

nw <- nadaraya.watson(x, X, Y, kernel, h=h.nw.cv$minimum)
lines(nw$x, nw$mx, lwd=3, col=3)
ll <- local.linear(x, X, Y, kernel, h=h.ll.cv$minimum)
lines(ll$x, ll$mx, lwd=3, col=4)
legend(0.6, 1.5, legend=c("Local Constant", "Local Linear", "True Curve"), 
		col=c(3, 4, 1), lwd=c(3,3,1), lty=c(1,1,2))

h.nw.cv
h.ll.cv

