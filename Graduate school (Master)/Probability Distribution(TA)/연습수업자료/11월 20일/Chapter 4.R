## Ex 4.18

# 표준정규분포를 따르는 난수 생성
set.seed(1)
y <- rnorm(1000)

# 평균을 구하는 함수생성
new_mean <- function(x) {
  sum(x)/length(x)
}
new_mean(y)
mean(y)

# 분산을 구하는 새로운 함수생성
new_var <- function(x) {
  sum((x-new_mean(x))^2)/(length(x)-1)
}
new_var(y)
var(y)



## Ex 4.19

basic.stat <- function(x) {
  a <- min(x) ; b <- quantile(x, 0.25) ; c <- mean(x)
  d <- median(x) ; e <- quantile(x, 0.75) ; f <- max(x)
  return(list(min=a, Q1=b, mean=c, med=d, Q3=e,
              max=f))
}
xx <- rnorm(1000)
nstat <- basic.stat(xx)
attributes(nstat)
nstat$Q1
nstat$mean



## Ex. 4.20

data.plot <- function(x) {
  par(mfrow=c(2,2))
  hist(x, main="Histogram")
  boxplot(x, main="Boxplot")
  qqnorm(x, main="Normal Q-Q plot")
  qqline(x)
  plot(density(x), main="Density plot")
}
x <- rnorm(200) ; data.plot(x)



## Ex 4.21

fx <- function(x) {3*(1-x^2)/4}
integrate(fx, lower=-0.5, upper=1)

fx <- function(x) {exp(-x^2/2)/sqrt(2*pi)}
integrate(fx, lower=-Inf, upper=1.3)
pnorm(1.3)