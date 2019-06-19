# setwd("C:/Users/user/Desktop/Jeong")

dat <- read.csv("kospi_daily.csv")
kospi <- dat$Close
plot(kospi, type='l', main="KOSPI", xlab="t", ylab="KOSPI")
kospi.return <- diff(log(kospi))
plot(kospi.return, type='l', main="KOSPI returns", xlab="t", ylab="returns")

z <- scale(kospi.return)
skew.kospi <- mean(z^3)
kurt.kospi <- mean(z^4)
print(c(mean(kospi.return), sd(kospi.return), skew.kospi, kurt.kospi))

hist(kospi.return, prob=T, nclass=50)
x <- seq(from=-0.06, to=0.06, by=0.0001)

# nigFit(kospi.return)
hypFit(kospi.return)

lines(x, dnorm(x, mean=0.0001262907, sd=0.0112653874), col=2)
#lines(x, dnig(x, 85.599768740, -8.467061901,  0.010656143,  0.001185533), col=3)
lines(x, dhyp(x, 138.672114741,  -8.157580191,   0.004243470,   0.001125776), col=3)


#nig.volatility <- function(alpha, beta, delta, mu)
#{
#  gamma <- sqrt(alpha^2-beta^2)
#  ans <- sqrt(delta*alpha^2/gamma^3)
#  
#  return(ans)
#}

#nig.volatility(85.599768740, -8.467061901,  0.010656143,  0.001185533)
sd(kospi.return)

# NIG assumption
qnig(0.05, 85.599768740, -8.467061901,  0.010656143,  0.001185533)  # 95% VaR
qnig(0.01, 85.599768740, -8.467061901,  0.010656143,  0.001185533)  # 99% VaR
qnig(0.005, 85.599768740, -8.467061901,  0.010656143,  0.001185533) # 99.5% VaR

# Normal assumption
qnorm(0.05, mean=0.0001262907, sd=0.0112653874)    # 95% VaR
qnorm(0.01, mean=0.0001262907, sd=0.0112653874)    # 99% VaR
qnorm(0.005, mean=0.0001262907, sd=0.0112653874)   # 99.5% VaR

# HYP assumption
qhyp(0.05,  138.672114741,  -8.157580191,   0.004243470,   0.001125776)
qhyp(0.01,  138.672114741,  -8.157580191,   0.004243470,   0.001125776)
qhyp(0.005,  138.672114741,  -8.157580191,   0.004243470,   0.001125776)

# Excess probability

mean(kospi.return <= qnorm(0.05, mean=0.0001262907, sd=0.0112653874))
mean(kospi.return <= qnorm(0.01, mean=0.0001262907, sd=0.0112653874))
mean(kospi.return <= qnorm(0.005, mean=0.0001262907, sd=0.0112653874))

mean(kospi.return <= qhyp(0.05,  138.672114741,  -8.157580191,   0.004243470,   0.001125776))
mean(kospi.return <= qhyp(0.01,  138.672114741,  -8.157580191,   0.004243470,   0.001125776))
mean(kospi.return <= qhyp(0.005,  138.672114741,  -8.157580191,   0.004243470,   0.001125776))

mean(kospi.return <= qnig(0.05, 85.599768740, -8.467061901,  0.010656143,  0.001185533))
mean(kospi.return <= qnig(0.01, 85.599768740, -8.467061901,  0.010656143,  0.001185533))
mean(kospi.return <= qnig(0.005, 85.599768740, -8.467061901,  0.010656143,  0.001185533))


par(mfrow=c(1,2))
acf(kospi.return)
acf(abs(kospi.return))

library(tseries)
vol <- garch(kospi.return)
par(mfrow=c(3,1))
plot(kospi.return, type='l', main="KOSPI Returns")
plot(vol$fitted.values[,1], type='l', main="Estimated volatility")
plot(vol$residuals, type='l', main="Residuals")

