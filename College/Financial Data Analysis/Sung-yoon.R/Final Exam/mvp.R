Minimum.variance.portfolio <- function(Returns, mu.p, plot=F, prt=F)
{
	n.p <- length(mu.p)
	mu <- apply(Returns, 2, mean)
	Sigma <- cov(Returns)
	Sigma.inv <- solve(Sigma)
	ones <- rep(1, dim(Returns)[2])
	
	Av <- (t(ones)%*%Sigma.inv%*%mu)[1,1]
	Bv <- (t(mu)%*%Sigma.inv%*%mu)[1,1]
	Cv <- (t(ones)%*%Sigma.inv%*%ones)[1,1]
	Dv <- Bv*Cv-Av*Av

	g <- (Bv*Sigma.inv%*%ones-Av*Sigma.inv%*%mu)/Dv
	h <- (Cv*Sigma.inv%*%mu-Av*Sigma.inv%*%ones)/Dv
	
	w.p <- sigma.p <- NULL
	for ( j in 1:n.p ) {
		w <- g+h*mu.p[j]
		w.p <- cbind(w.p, w)
		sigma.p <- c(sigma.p, sqrt((t(w)%*%Sigma%*%w)[1,1]))
	}
	
	if (plot) {
    	plot(sigma.p, mu.p, xlim=c(0, max(sigma.p)), xlab="volatility", ylab="mean return")
    	lines(sigma.p, mu.p, col=2)
	}
	if (prt) print(cbind(mu.p, sigma.p, t(w.p)))
	
	return(list(mu.p=mu.p, sigma.p=sigma.p, w.p=w.p))
}

Tangency.portfolio <- function(Returns, r.f, plot=F)
{
	mu <- apply(Returns, 2, mean)
	Sigma <- cov(Returns)
	Sigma.inv <- solve(Sigma)
	ones <- rep(1, dim(Returns)[2])
  tmp <- mu-r.f*ones
 	w.bar <- Sigma.inv%*%tmp

  lb <- 0.8*min(mu)
  ub <- 1.2*max(mu)
  mvp <- Minimum.variance.portfolio(Returns, mu.p=seq(from=lb, to=ub, len=20), plot=plot, prt=F)
	w.q <- w.bar/sum(w.bar)
  mu.q <- crossprod(w.q, mu)
  sigma.q <- sqrt((t(w.q)%*%Sigma%*%w.q)[1,1])
  if (plot) {
    	slope <- (mu.q-r.f)/sigma.q
    	abline(r.f, slope, col=3)
    	points(sigma.q, mu.q, pch=17, col=2)
  }
  
  return(list(mu.q=mu.q, sigma.q=sigma.q, w.q=w.q))
}

# Capital Asset Pricing Model

capm.measure <- function(port.return,bm.return,r.f,plot=F) {
  x <- bm.return-r.f
  y <- port.return-r.f
  plot(x,y,xlab="BM",ylab="")
  res <- lm(y~x)
  # summary(res)
  abline(res$coef,ylab="")
  
  mu <- mean(y)
  sigma <- sd(y)
  sharpe <- (mu-r.f)/sigma
  alpha <- res$coef[1]
  beta <- res$coef[2]
  treynor <- (mu-r.f)/beta
  
  return(list(mu=mu,sigma=sigma,sharpe=sharpe,alpha=alpha,beta=beta,treynor=treynor))
}

apple <- read.csv("apple.csv")
x <- apple$Close
n <- length(x)
apple.return <- x[2:n]/x[1:(n-1)] - 1

hp <- read.csv("hp.csv")
x <- hp$Close
n <- length(x)
hp.return <- x[2:n]/x[1:(n-1)] - 1

ibm <- read.csv("ibm.csv")
x <- ibm$Close
n <- length(x)
ibm.return <- x[2:n]/x[1:(n-1)] - 1

Returns <- cbind(apple.return, hp.return, ibm.return)
apply(Returns, 2, mean)
cov(Returns)

mu.p <- seq(from=0.002, to=0.035, by=0.0005)
mvp <- Minimum.variance.portfolio(Returns, mu.p, plot=T, prt=T)

tp <- Tangency.portfolio(Returns, r.f=0.002, plot=T)

