robust.reg <- function(x, y)
{
  xx <- sort(x)
  yy <- y[order(x)]
  n <- length(x)
  s <- NULL
  for ( i in 1:(n-1) ) {
    for ( j in (i+1):n ) {
      s <- c(s, (yy[j]-yy[i])/(xx[j]-xx[i]))
    }
  }
  beta <- median(s) # Sen estimate

  z <- y - beta*x
  w <- NULL
  for ( i in 1:n ) {
    for ( j in i:n ) {
      w <- c(w, (z[i]+z[j])/2)
    }
  }
  alpha <- median(w) # Hodges-Lehmann type estimate

  return(c(alpha, beta))
}

set.seed(-1)
n <- 20
x <- runif(n)
y <- 3 + 2*x + rt(n, 5)
y[which.max(x)] <- 20 # an artificial outlier
plot(x, y)
abline(3, 2, col=2, lty=2)
res <- robust.reg(x, y)
abline(res, lwd=3)
res.ls <- lsfit(x, y)
abline(res.ls$coef, col=3)
legend(locator(1), c("LSE", "Robust", "True"), col=c(3, 1, 2), lwd=c(1, 3, 1), lty=c(1, 1, 3))
