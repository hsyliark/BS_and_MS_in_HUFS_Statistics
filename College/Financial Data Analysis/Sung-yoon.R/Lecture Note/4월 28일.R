par(mfrow=c(1,2))

apple <- read.csv("apple.csv")
x <- apple$Close
n <- length(x)
apple.return <- x[2:n]/x[1:(n-1)] - 1
plot(density(apple.return))
x <- seq(from=min(apple.return), to=max(apple.return), by=0.001)
lines(x, dnorm(x, mean=mean(apple.return), sd=sd(apple.return)), col=2)
mean(apple.return); sd(apple.return)

hp <- read.csv("hp.csv")
x <- hp$Close
n <- length(x)
hp.return <- x[2:n]/x[1:(n-1)] - 1
plot(density(hp.return))
x <- seq(from=min(hp.return), to=max(hp.return), by=0.001)
lines(x, dnorm(x, mean=mean(hp.return), sd=sd(hp.return)), col=2)
mean(hp.return); sd(hp.return)

par(mfrow=c(1,1))
plot(apple.return, hp.return)
cor(apple.return, hp.return)

min.var.portfolio <- function(r1, r2)
{
  mu.1 <- mean(r1)
  mu.2 <- mean(r2)
  sigma.1 <- sd(r1)
  sigma.2 <- sd(r2)
  rho <- cor(r1, r2)
  w <- (sigma.2^2-rho*sigma.1*sigma.2)/(sigma.1^2+sigma.2^2-2*rho*sigma.1*sigma.2)

  mu.p <- w*mu.1+(1-w)*mu.2
  sigma.p <- sqrt(w^2*sigma.1^2+(1-w)^2*sigma.2^2+2*w*(1-w)*rho*sigma.1*sigma.2)

  return(list(w=w, mu.p=mu.p, sigma.p=sigma.p, mu.1=mu.1, sigma.1=sigma.1, mu.2=mu.2, sigma.2=sigma.2, rho=rho))
}

min.var.portfolio(apple.return, hp.return)
