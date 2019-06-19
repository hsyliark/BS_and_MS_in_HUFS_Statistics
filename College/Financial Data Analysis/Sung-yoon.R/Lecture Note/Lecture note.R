## 2014.3.19

setwd("C:/Users/student/Desktop/Sung-yoon.R/Lecture Note")

dat <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Lecture Note/kospi_daily.csv",sep=",",header=T)
kospi <- dat$Close
plot(kospi,type="l",main="KOSPI",xlab="t",ylab="KOSPI")

kospi.return <- diff(log(kospi)) 
# rt = log(St)-log(St-1) = log(St/St-1) = log(1+Rt) (log return)
# Rt= (St - St-1)/(St-1) = (St/St-1) - 1 (return) ==> rt =: Rt (0 근방에서 거의 비슷함)
plot(kospi.return,type="l",main="KOSPI returns",xlab="t",ylab="returns")

#Skewness, Kurtosis
z <- scale(kospi.return) # standardization
skew.kospi <- mean(z^3) ; kurt.kospi <- mean(z^4)
print(c(mean(kospi.return),sd(kospi.return),skew.kospi,kurt.kospi)) # leptokurtic (첨도 > 3)

## 2014.3.24

# 수익률의 분포에 대한 추정
hist(kospi.return,prob=T,nclass=50)
x <- seq(from=-0.06,to=0.04,by=0.0001)
lines(x,dnorm(x,mean=mean(kospi.return),sd=sd(kospi.return)),col=2) 
# 정규분포 모형과 잘 맞지는 않음 ~~!! 꼬리가 두꺼움.. (수익률은 정규분포와 차이가 난다.)

par(mfrow=c(1,2))
#자기상관함수 (음수 : 음의 상관관계, 양수 : 양의 상관관계)
acf(kospi.return) # lag : 시차 (이전날의 수익률이 오늘의 수익률과 관련이 거의 없음.) 
acf(abs(kospi.return)) # n일 전의 수익률이 크면 오늘의 수익률도 클 것이다.
# 수익률 자체는 분석하기 어렵지만 수익률의 크기를 분석하면 의미가 있을 수 있다. 
# Volatility Clustering (변동성을 분석하고 예측~~!)

#숙제 : 수요일(3.26)까지 기업을 5개 골라서(삼성, LG 등)
#개별주식의 주가를 최근 5년치 조사해서 오늘 배운 내용 그대로
#특징을 분석해서 markdown 형태로 제출~~!!
#5개 회사 하나하나씩 ~~!

## 2014.3.26

# 숙제 : Tukey라는 통계학자에 대해서 조사해볼 것 ~~! & 이상한 데이터 다시 만들 것~~!!

# Risk와 Danger는 전혀 다른 의미~~!!
# 금융에서의 표준편차는 변동성(volatility)라고 부른다. (%)
# Risk measure에 관한 측정을 하게 될 것임. 
# 수익률의 분포를 생각해볼 것이다. -> Barnedorff-Nielson (Generalized hyperbolic distribution)

## 2014.3.31

# Value-at-Risk(VaR(1-a) = a-quantile of Rt)와 volatility에 대한 추정
# Generalized Hyperbolic Distribution --> GH(lambda,alpha,beta,delta,mu)
# lambda=1 -> H(alpha,beta,delta,mu) Hyperbolic Distribution
# lambda=-(1/2) -> NIG(alpha,beta,delta,mu) Normal-Inverse Gaussian Distribution

par(mfrow=c(1,1))
install.packages("fBasics")
library(fBasics)

?nigFit
?hypFit
nigFit(kospi.return)
?qnig

hist(kospi.return,prob=T,nclass=50)
x <- seq(from=-0.06,to=0.04,by=0.0001)
lines(x,dnorm(x,mean=mean(kospi.return),sd=sd(kospi.return)),col=2) 
lines(x,dnig(x,86.002807860,-8.518704925,0.010703793,0.001191761),col=3)

nig.volatility <- function(alpha,beta,delta,mu) {
  gamma <- sqrt(alpha^2-beta^2)
  ans <- sqrt(delta*alpha^2/gamma^3)
  return(ans)
}
nig.volatility(86.002807860,-8.518704925,0.010703793,0.001191761)
sd(kospi.return)

# NIG assumption
qnig(0.01,86.002807860,-8.518704925,0.010703793,0.001191761) # 99% VaR
qnig(0.005,86.002807860,-8.518704925,0.010703793,0.001191761) # 99.5% VaR
qnig(0.05,86.002807860,-8.518704925,0.010703793,0.001191761) # 95% VaR

# Normal assumption
qnorm(0.05,mean=0.0001262907,sd=0.0112672356) # 95% VaR
qnorm(0.005,mean=0.0001262907,sd=0.0112672356) # 99.5% VaR
qnorm(0.01,mean=0.0001262907,sd=0.0112672356) # 99% VaR

# 과제 : 4월 2일까지 오늘 배운거 정리해서 Markdown으로 준비 ~~!! (히스토그램과 그래프, 분산, 3가지 VaR 추정치)

## 2014.4.2

# GH(lambda,alpha,beta,delta,mu)
# lambda=1 -> Hyperbolic Distribution

dat.1 <- read.csv("C:/Users/student/Desktop/Sung-yoon.R/Mid-term Exam/Samsung eletronics/samsung_daily.csv",sep=",",header=T)
samsung <- dat.1$Close
samsung.return <- diff(log(samsung))
plot(samsung,type="l",main="Samsung eletronics",xlab="t",ylab="Samsung")
plot(samsung.return,type="l",main="Samsung returns",xlab="t",ylab="returns")
install.packages("fBasics")
library(fBasics)
hypFit(samsung.return)
hist(samsung.return,prob=T,nclass=50)
x <- seq(-0.05,0.05,by=0.001)
lines(x,dnorm(x,mean(samsung.return),sd(samsung.return)),col=2)
lines(x,dhyp(x,98.369967865,6.818844969,0.013887717,-0.001726826),col=3)
# HYP assumption
qhyp(0.05,98.369967865,6.818844969,0.013887717,-0.001726826) # 95% VaR
qhyp(0.01,98.369967865,6.818844969,0.013887717,-0.001726826) # 99% VaR
qhyp(0.005,98.369967865,6.818844969,0.013887717,-0.001726826) # 99.5% VaR
# NIG assumption
qnig(0.05,78.239151023,7.288384337,0.024389717,-0.001864134) # 95% VaR
qnig(0.01,78.239151023,7.288384337,0.024389717,-0.001864134) # 99% VaR
qnig(0.005,78.239151023,7.288384337,0.024389717,-0.001864134) # 99.5% VaR
# Normal assumption
qnorm(0.05,mean(samsung.return),sd(samsung.return)) # 95% VaR
qnorm(0.01,mean(samsung.return),sd(samsung.return)) # 99% VaR
qnorm(0.005,mean(samsung.return),sd(samsung.return)) # 99.5% VaR

# Excess probability =: T/N
# (T : number of returns <= VaR , N : total number of returns)
# 적당한 분포가 어느 것인지 판별하는 과정.

# Normal assumption
mean(samsung.return <= qnorm(0.05,mean(samsung.return),sd(samsung.return)))
mean(samsung.return <= qnorm(0.01,mean(samsung.return),sd(samsung.return)))
mean(samsung.return <= qnorm(0.005,mean(samsung.return),sd(samsung.return)))

# HYP assumption
mean(samsung.return <= qhyp(0.05,98.369967865,6.818844969,0.013887717,-0.001726826))
mean(samsung.return <= qhyp(0.01,98.369967865,6.818844969,0.013887717,-0.001726826))
mean(samsung.return <= qhyp(0.005,98.369967865,6.818844969,0.013887717,-0.001726826))

# NIG assumption
mean(samsung.return <= qnig(0.05,78.239151023,7.288384337,0.024389717,-0.001864134))
mean(samsung.return <= qnig(0.01,78.239151023,7.288384337,0.024389717,-0.001864134))
mean(samsung.return <= qnig(0.005,78.239151023,7.288384337,0.024389717,-0.001864134))

# H0 : T ~ B(N,0.01) , H1 : Not H0 ..

# Generalized Autoregressive Conditional Heteroscedastic Model
# (GARCH Model) 
# (자기회귀 & 이분산성 (과거의 기록을 조건부로 이용))
# Engle -> Bollerslev
# Volatility 를 모형화하기 위한 model

# ARCH(p) --> 
# sigma(t)^2 = alpha(0) + alpha(1)*r(t-1)^2 +
# alpha(2)*r(t-2)^2 + ... + alpha(p)*r(t-p)^2 (t=1,2,...,T)

# GARCH(p,q) -->
# sigma(t)^2 = alpha(0) + alpha(1)*r(t-1)^2 +
# alpha(2)*r(t-2)^2 + ... + alpha(p)*r(t-p)^2 +
# beta(1)*sigma(t-1)^2 + beta(2)*sigma(t-2)^2 +
# beta(q)*sigma(t-q)^2 (t=1,2,...,T)
# 일반적으로 GARCH(1,1) 으로 충분하다고 알려져있음.

install.packages("tseries")
library(tseries)
?garch
vol <- garch(samsung.return)
par(mfrow=c(3,1))
plot(samsung.return,type="l",main="Samsung returns")
plot(vol$fitted.values[,1],type="l",main="Estimated volatility")
plot(vol$residuals,type="l",main="Residuals")

## 2014.4.7

# Finance time series model

# Conditional Heteroscedastic Model
# S(t) : asset price
# r(t) = log(S(t))-log(S(t-1)) : log-return or return
# r(t) = sigma(t)*e(t) , e(t) ~ WN(0,1) (e : epsilon)
# sigma(t)^2 = Var(r(t) | r(t-1), r(t-2), ...)
# = E(r(t)^2 | r(t-1), r(t-2), ...)
# GARCH 모형에서 모수를 추정 후 잔차를 이용하여 검진실시.
# (잔차에 대한 acf 그림을 그린다.
# 그리고 Box-Ljung test 실시. H0 : 상관관계 없음.)

library(tseries)
vol.samsung <- garch(samsung.return) # GARCH(1,1)
summary(vol.samsung) # Jarque Bera Test (H0 : Residual is Normal.)

par(mfrow=c(3,1))
plot(samsung.return,type="l",main="Samsung returns")
plot(vol.samsung$fitted.values[,1],type="l",main="Estimated volatility")
plot(vol.samsung$residuals,type="l",main="Residuals")

par(mfrow=c(2,2))
acf(samsung.return)
acf(samsung.return^2)
acf(vol.samsung$residuals[-1])
acf(vol.samsung$residuals[-1]^2)

? garch
attributes(vol.samsung)
# GARCH에서 order, coef, residuals, 그리고 fitted.values 를 
# 자주 사용하게 될 것임.

# Homework : 오늘 배운 분석내용 5개의 데이터에 적용해서 
# markdown 문서로 작성해서 준비.

## 2014.4.9

# If r ~ N(0,sigma^2) , it is not true..
# r(t) = sigma(t)*e(t) , e(t) ~ WN(0,1) (e : epsilon)
# e(t) is called random shocks..
# GARCH(1,1) -> sigma(t)^2 = alpha(0) + alpha(1)*r(t-1)^2
# + beta(1)*sigma(t-1)^2
# Refer to Jarque Bera Test, e(t) is nonnormal~~!!

# 잔차의 분포를 구할 필요성.
# 잔차에 관해서 plot을 그리고 정규성 확인

setwd("C:/Users/student/Desktop/Sung-yoon.R/Lecture Note")

dat.1 <- read.csv("C:/Users/student/Desktop/Sung-yoon.R/Mid-term Exam/Samsung eletronics/samsung_daily.csv",sep=",",header=T)
samsung <- dat.1$Close
samsung.return <- diff(log(samsung))

install.packages("tseries")
library(tseries)
vol.samsung <- garch(samsung.return)
res.samsung <- vol.samsung$residuals[-1]
hist(res.samsung,prob=T,nclass=30)
x <- seq(-4,2,by=0.001)
lines(x,dnorm(x),col=2)
summary(vol.samsung)

install.packages("fBasics")
library(fBasics)
nig.samsung <- nigFit(res.samsung)
nigFit(res.samsung)
summary(nig.samsung)

hist(res.samsung,prob=T,nclass=30)
x <- seq(-4,2,by=0.001)
x <- seq(-4,2,by=0.001)
lines(x,dnorm(x),col=2)
lines(x,dnig(x,1.5981890,0.1320987,1.5978737,-0.1163623),col=3)

# P(r(t) <= x ) = 0.01 (Value-at-Risk)
# P(sigma(t)*e(t) <= sigma(t)*y ) = 0.01 (Find y!!)

# Time varying VaR
q.05 <- qnig(0.05,1.5981890,0.1320987,1.5978737,-0.1163623)
q.01 <- qnig(0.01,1.5981890,0.1320987,1.5978737,-0.1163623)
q.005 <- qnig(0.005,1.5981890,0.1320987,1.5978737,-0.1163623)
sigma.t <- vol.samsung$fitted.values[-1,1]
VaR.95 <- sigma.t*q.05
VaR.99 <- sigma.t*q.01
VaR.995 <- sigma.t*q.005

png("VaR.png",height=600,width=1000)
n <- length(samsung.return)
plot(samsung.return,type="l",ylim=c(-0.1,0.1))
lines(2:n,VaR.95,col=2)
lines(2:n,VaR.99,col=3)
lines(2:n,VaR.995,col=4)
legend("topleft",c("95% VaR","99% VaR","99.5% VaR"),lty=rep(1,3),col=c(2,3,4))
dev.off()

# 적합성 확인
mean(samsung.return[-1] < VaR.95)
mean(samsung.return[-1] < VaR.99)
mean(samsung.return[-1] < VaR.995)

VaR.N95 <- sigma.t*qnorm(0.05)
VaR.N99 <- sigma.t*qnorm(0.01)
VaR.N995 <- sigma.t*qnorm(0.005)

mean(samsung.return[-1] < VaR.N95)
mean(samsung.return[-1] < VaR.N99)
mean(samsung.return[-1] < VaR.N995) # Risk를 과소평가 ...

# 과제 : 오늘 배운거 markdown으로 만들 것~~!!

## 2014.4.14

dat.1 <- read.csv("C:/Users/student/Desktop/Sung-yoon.R/Mid-term Exam/Samsung eletronics/samsung_daily.csv",sep=",",header=T)
samsung <- dat.1$Close
samsung.return <- diff(log(samsung))

# Backtest VaR (Value-at-Risk를 평가하는 방법)

# N : number of 100*(1-p)% VaR violations
# T : total number of observations
# H0 : N ~ B(T,p)
# LRT statistic = -2*log{(1-p)^(T-N)*p^N}+2*log{(1-N/T)^(T-N)*(N/T)^N}
# -->d(Convergence in distribution) 
# Chi-square distribution with df=1 under H0
# (LRT statistic = -2*log(L0/L))

install.packages("tseries")
library(tseries)
vol.samsung <- garch(samsung.return)
res.samsung <- vol.samsung$residuals[-1]
hist(res.samsung,prob=T,nclass=30)
x <- seq(-4,2,by=0.001)
lines(x,dnorm(x),col=2)
summary(vol.samsung)

install.packages("fBasics")
library(fBasics)
nig.samsung <- nigFit(res.samsung)
nigFit(res.samsung)
summary(nig.samsung)

hist(res.samsung,prob=T,nclass=30)
x <- seq(-4,2,by=0.001)
x <- seq(-4,2,by=0.001)
lines(x,dnorm(x),col=2)
lines(x,dnig(x,1.5981890,0.1320987,1.5978737,-0.1163623),col=3)

q.05 <- qnig(0.05,1.5981890,0.1320987,1.5978737,-0.1163623)
q.01 <- qnig(0.01,1.5981890,0.1320987,1.5978737,-0.1163623)
q.005 <- qnig(0.005,1.5981890,0.1320987,1.5978737,-0.1163623)
sigma.t <- vol.samsung$fitted.values[-1,1]
VaR.95 <- sigma.t*q.05
VaR.99 <- sigma.t*q.01
VaR.995 <- sigma.t*q.005

n <- length(samsung.return)
plot(samsung.return,type="l",ylim=c(-0.1,0.1))
lines(2:n,VaR.95,col=2)
lines(2:n,VaR.99,col=3)
lines(2:n,VaR.995,col=4)
legend("topleft",c("95% VaR","99% VaR","99.5% VaR"),lty=rep(1,3),col=c(2,3,4))

mean(samsung.return[-1] < VaR.95)
mean(samsung.return[-1] < VaR.99)
mean(samsung.return[-1] < VaR.995)

VaR.N95 <- sigma.t*qnorm(0.05)
VaR.N99 <- sigma.t*qnorm(0.01)
VaR.N995 <- sigma.t*qnorm(0.005)

mean(samsung.return[-1] < VaR.N95)
mean(samsung.return[-1] < VaR.N99)
mean(samsung.return[-1] < VaR.N995)

# Make function

backtest.VaR <- function(N.violations, N.total, p) {
  p.hat <- N.violations/N.total
  LR <- -2*log((1-p)^(N.total-N.violations)*p^N.violations)+2*log((1-p.hat)^(N.total-N.violations)*p.hat^N.violations)
  p.value <- 1-pchisq(LR,1)
  return(list(excess.prob=p.hat, LR=LR, p.value=p.value))
}

# NIG assumption
backtest.VaR(sum(samsung.return[-1] < VaR.95),length(samsung.return[-1]),0.05)
backtest.VaR(sum(samsung.return[-1] < VaR.99),length(samsung.return[-1]),0.01)
backtest.VaR(sum(samsung.return[-1] < VaR.995),length(samsung.return[-1]),0.005)
# Normal assumption
backtest.VaR(sum(samsung.return[-1] < VaR.N95),length(samsung.return[-1]),0.05)
backtest.VaR(sum(samsung.return[-1] < VaR.N99),length(samsung.return[-1]),0.01)
backtest.VaR(sum(samsung.return[-1] < VaR.N995),length(samsung.return[-1]),0.005)

## 2014.4.16

# Monthly data
# 월별자료에서는 수익률을 Rt=(St/S(t-1))-1 을 사용한다.
# Taylor에 의한 근사치가 정확하지 않음..

apple <- read.csv("C:/Users/student/Desktop/Sung-yoon.R/Final Exam/apple_month.csv",sep=",",header=T)
x <- apple$Close
n <- length(x)
apple.return <- x[2:n]/x[1:(n-1)]-1 # Rt=(St/S(t-1))-1
plot(density(apple.return))
x <- seq(-0.2,0.2,by=0.001)
lines(x,dnorm(x,mean(apple.return),sd(apple.return)),col=2)

# 월별수익률 데이터는 정규분포를 사용해도
# 무리가 없다고 알려져 있음. (By Central Limit Theorem)

mean(apple.return) # Expected return(기대수익)
sd(apple.return) # Volatility

hp <- read.csv("C:/Users/student/Desktop/Sung-yoon.R/Final Exam/hp_month.csv",sep=",",header=T)
x <- hp$Close
n <- length(x)
hp.return <- x[2:n]/x[1:(n-1)]-1 # Rt=(St/S(t-1))-1
plot(density(hp.return))
x <- seq(-0.4,0.4,by=0.001)
lines(x,dnorm(x,mean(hp.return),sd(hp.return)),col=2)
mean(hp.return)
sd(hp.return)

ibm <- read.csv("C:/Users/student/Desktop/Sung-yoon.R/Final Exam/ibm_month.csv",sep=",",header=T)
x <- ibm$Close
n <- length(x)
ibm.return <- x[2:n]/x[1:(n-1)]-1 # Rt=(St/S(t-1))-1
plot(density(ibm.return))
x <- seq(-0.1,0.1,by=0.001)
lines(x,dnorm(x,mean(ibm.return),sd(ibm.return)),col=2)
mean(ibm.return)
sd(ibm.return)

starbucks <- read.csv("C:/Users/student/Desktop/Sung-yoon.R/Final Exam/starbucks_month.csv",sep=",",header=T)
x <- starbucks$Close
n <- length(x)
starbucks.return <- x[2:n]/x[1:(n-1)]-1 # Rt=(St/S(t-1))-1
plot(density(starbucks.return))
x <- seq(-0.2,0.2,by=0.001)
lines(x,dnorm(x,mean(starbucks.return),sd(starbucks.return)),col=2)
mean(starbucks.return)
sd(starbucks.return)

volkswagen <- read.csv("C:/Users/student/Desktop/Sung-yoon.R/Final Exam/volkswagen_month.csv",sep=",",header=T)
x <- volkswagen$Close
n <- length(x)
volkswagen.return <- x[2:n]/x[1:(n-1)]-1 # Rt=(St/S(t-1))-1
plot(density(volkswagen.return))
x <- seq(-0.2,0.3,by=0.001)
lines(x,dnorm(x,mean(volkswagen.return),sd(volkswagen.return)),col=2)
mean(volkswagen.return)
sd(volkswagen.return)

# Correlation
plot(apple.return,hp.return)
cov(apple.return,hp.return)
cor(apple.return,hp.return)
cor.test(apple.return,hp.return)

# Mid-term
------------------------------------------------------------

## 2014.4.28

# 2004년 1월부터 2014년 4월까지의 월별 수익률..

# 월별수익률 데이터는 정규분포를 사용해도
# 무리가 없다고 알려져 있음. (By Central Limit Theorem)

par(mfrow=c(1,2))

apple <- read.csv("C:/Users/student/Desktop/Sung-yoon.R/Final Exam/apple_month.csv",sep=",",header=T)
x <- apple$Close
n <- length(x)
apple.return <- x[2:n]/x[1:(n-1)]-1 # Rt=(St/S(t-1))-1
plot(density(apple.return))
x <- seq(-0.4,0.4,by=0.001)
lines(x,dnorm(x,mean(apple.return),sd(apple.return)),col=2)
mean(apple.return) 
sd(apple.return) 

hp <- read.csv("C:/Users/student/Desktop/Sung-yoon.R/Final Exam/hp_month.csv",sep=",",header=T)
x <- hp$Close
n <- length(x)
hp.return <- x[2:n]/x[1:(n-1)]-1 # Rt=(St/S(t-1))-1
plot(density(hp.return))
x <- seq(-0.6,0.4,by=0.001)
lines(x,dnorm(x,mean(hp.return),sd(hp.return)),col=2)
mean(hp.return)
sd(hp.return)

# 월별 수익률은 일별 수익률에 비하여 값이 그렇게 작지 않다.
# 그러므로 log-return을 이용한 근사가 적절하지 않다.

# If X1, X2, X3, X4, X5, X6 ~ i.i.d N(mu,(sigma)^2) , then,
# Var(X1+X2+X3+X4+X5+X6) = Var(X1)+Var(X2)+Var(X3)+Var(X4)+Var(X5)+Var(X6)
# = 6*(sigma)^2
# -> sd(X1+X2+X3+X4+X5+X6) = sqrt(6)*sigma
# 월별 누적 수익률의 Volatility는 월의 수의 제곱근에 비례하여 증가한다.
# (단, 월별 수익률의 분포가 모두 같고 서로 독립이라고 가정했을 때..)

#포트폴리오 이론을 이용하면 risk를 최소화할 수 있다.

# 월별 수익률 : X (APPLE) ~ N((3%),(11%)^2) , Y (HP) ~ N((2%),(12%)^2)
# 만약에 비중을 apple은 1/3, hp는 2/3 정도로 투자한다고 하자. 그러면,
# (1/3)*X + (2/3)*Y ~ 
# N((1/3)*(3%)+(2/3)*(2%) ,
# (1/9)*(11%)^2+(4/9)*(12%)^2+2*(1/3)*(2/3)*Cov(X,Y))

cov(apple.return,hp.return)

# = N((2.3%), (8.8%)^2)

# R1 ~ N( mu1,(sigma1)^2 ) , R2 ~ N( mu2,(sigma2)^2 )
# 포트폴리오 수익률 : Rp = w*R1 + (1-w)*R2
# Rp ~ N( w*(mu1) + (1-w)*(mu2) , 
# w^2*(sigma1)^2 + (1-w)^2*(sigma2)^2 + 2*w*(1-w)*(rho)*(sigma1)*(sigma2))

# 그런데 상관계수 rho의 값은 -1과 1사이의 값이므로

# mu(p) = w*(mu1) + (1-w)*(mu2)
# sigma(p) = w^2*(sigma1)^2 + (1-w)^2*(sigma2)^2 + 2*w*(1-w)*(rho)*(sigma1)*(sigma2)
# <= w^2*(sigma1)^2 + (1-w)^2*(sigma2)^2 + 2*w*(1-w)*(sigma1)*(sigma2)
# = { w*(sigma1) + (1-w)*(sigma2) }^2 
# 개별 주식 risk의 합의 제곱보다는 작음 => 포트폴리오 효과
# sigma(p)를 w에 대하여 미분해서 값을 최소로 하는 w를 찾게 된다.
# 그 결과는 w* = ( (sigma2)^2 - (rho)*(sigma1)*(sigma2) )
# /( (sigma1)^2 + (sigma2)^2 - 2*(rho)*(sigma1)*(sigma2) )
# => (w* , 1-w*) : minimun variance portfolio

par(mfrow=c(1,1))
plot(apple.return,hp.return)
cor(apple.return,hp.return)

# 포트폴리오 구하는 함수
min.var.portfolio <- function(r1,r2) {
  mu.1 <- mean(r1)
  mu.2 <- mean(r2)
  sigma.1 <- sd(r1)
  sigma.2 <- sd(r2)
  rho <- cor(r1,r2)
  w <- (sigma.2^2-rho*sigma.1*sigma.2)/(sigma.1^2+sigma.2^2-2*rho*sigma.1*sigma.2)
  mu.p <- w*mu.1+(1-w)*mu.2
  sigma.p <- sqrt(w^2*sigma.1^2+(1-w)^2*sigma.2^2+2*w*(1-w)*rho*sigma.1*sigma.2)
  return(list(w=w , mu.p=mu.p , sigma.p=sigma.p , mu.1=mu.1 , mu.2=mu.2 , sigma.1=sigma.1 , sigma.2=sigma.2 , rho=rho))
}

min.var.portfolio(apple.return,hp.return)

# APPLE 주식에 대해서는 약 52% 정도 투자하는 것이 바람직함. (risk를 최소화.)
# 그러나 이것이 최선의 방법은 아님. (수익률은 어떻게 하는가?)

## 2014.4.30

# 5월 7일 수업 휴강

# 다수의 주식으로 구성한 포트폴리오 (w1,w2,...,wn)
# w1 + w2 + ... + wn = 1
# R1, R2, R3, ... , Rn
# Rp = R1 + R2 + ... + Rn = t(w)%*%(R)
# mu(p) = E(Rp) = t(w)%*%E(R) = t(w)%*%(MU)
# sigma(p)^2 = Var(Rp) = t(w)%*%Var(R)%*%(w)
# = t(w)%*%(SIGMA)%*%(w)
# MU : 평균벡터 , SIGMA : 공분산행렬
# 포트폴리오 수익률의 분포를 예측.

# MU와 SIGMA는 통제할 수 없으므로
# 벡터 (w)를 잘 결정해야 함.

# sigma(p)를 최소화하는 (w)를 찾는다. (라그랑주의 승수법)
# (단, t(w)%*%(MU) = mu(p) , t(w)%*%(1) = 1)

# (w)의 원소의 값이 음수인 경우는
# 주식을 빌려와서 팔아야 함. (Short selling : 공매)
# 주식들은 대부분 한 시장에서 거래되기 때문에
# correlation이 양수인 경우가 많다.

## 2014.5.12

# 특정한 수익률을 정했다면 이에 해당하는 포트폴리오 중
# 변동성(volatility)이 가장 작은 것을 선택하는 것이
# 타당하다. (즉, mu(p)가 정해져 있을 때 벡터 w를 적절하게 선택.)

# t(w)%*%(MU)=mu(p), t(w)%*%(1)=1 이라는 조건이 있을 때,
# t(w)%*%(SIGMA)%*%(w) 를 최소화하는 벡터 w를 찾을 것임. (Optimizing)
# MU : 평균벡터 , SIGMA : 공분산행렬

# 5개 기업의 월별 수익률(Returns)

apple <- read.csv("C:/Users/student/Desktop/Sung-yoon.R/Final Exam/apple_month.csv",sep=",",header=T)
x <- apple$Close
n <- length(x)
apple.return <- x[2:n]/x[1:(n-1)]-1

hp <- read.csv("C:/Users/student/Desktop/Sung-yoon.R/Final Exam/hp_month.csv",sep=",",header=T)
x <- hp$Close
n <- length(x)
hp.return <- x[2:n]/x[1:(n-1)]-1

ibm <- read.csv("C:/Users/student/Desktop/Sung-yoon.R/Final Exam/ibm_month.csv",sep=",",header=T)
x <- ibm$Close
n <- length(x)
ibm.return <- x[2:n]/x[1:(n-1)]-1

mcdonald <- read.csv("C:/Users/student/Desktop/Sung-yoon.R/Final Exam/mcdonald_month.csv",sep=",",header=T)
x <- mcdonald$Close
n <- length(x)
mcdonald.return <- x[2:n]/x[1:(n-1)]-1

starbucks <- read.csv("C:/Users/student/Desktop/Sung-yoon.R/Final Exam/starbucks_month.csv",sep=",",header=T)
x <- starbucks$Close
n <- length(x)
starbucks.return <- x[2:n]/x[1:(n-1)]-1

Returns <- cbind(apple.return,hp.return,ibm.return,mcdonald.return,starbucks.return)
summary(Returns)
apply(Returns,2,mean)
cov(Returns)

# e-class에 있는 'mvp.R' 파일에 있는
# 함수 'Minimum.variance.portfolio' 실행
# (기대수익률이 주어졌을 때 가장 작은 변동성을 가지는
# 벡터 w를 찾는 함수)

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

mu.p <- seq(from=0.001,to=0.05,by=0.0005)
mvp <- Minimum.variance.portfolio(Returns,mu.p,plot=T,prt=T)

# 이 결과는 월별에 대한 결과이므로 연별에 관한 기대수익률과 변동성(분산)은
# 각각의 결과에 12를 곱해서 계산.

## 2014.5.14 

# Tangency portfolio
# (Optimal portfolio with a risk-free asset)
# Sharpe ratio 를 최대화하는 포트폴리오를 선택함. (risk-adjusted excess return)
# Sharpe ratio = (mu-Rf)/(sigma) , Rf = risk-free asset
# 결국 접선의 기울기가 가장 최대가 된다. 그 점을 선택~~!!

# e-class에 있는 'mvp.R' 파일에 있는
# 함수 'Tangency.portfolio' 실행

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

tp <- Tangency.portfolio(Returns,r.f=0.002,plot=T)
tp

kospi <- read.csv("C:/Users/student/Desktop/Sung-yoon.R/Final Exam/kospi_month.csv",sep=",",header=T)
x <- kospi$Close
n <- length(x)
kospi.return <- x[2:n]/x[1:(n-1)]-1

apple <- read.csv("C:/Users/student/Desktop/Sung-yoon.R/Final Exam/apple_month.csv",sep=",",header=T)
x <- apple$Close
n <- length(x)
apple.return <- x[2:n]/x[1:(n-1)]-1

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
  
cor(kospi.return,apple.return)
capm.measure(apple.return,kospi.return,r.f=0.002,plot=T)
capm.measure(hp.return,kospi.return,r.f=0.002,plot=T)
capm.measure(ibm.return,kospi.return,r.f=0.002,plot=T)
capm.measure(mcdonald.return,kospi.return,r.f=0.002,plot=T)
capm.measure(starbucks.return,kospi.return,r.f=0.002,plot=T)

tangency <- Returns%*%tp$w.q
tangency1 <- Returns%*%rep(1/5,5)
capm.measure(tangency,kospi.return,r.f=0.002,plot=T)
capm.measure(tangency1,kospi.return,r.f=0.002,plot=T)
# kospi가 증가하면 apple도 증가하는 추세를 보임.
# kospi가 없어도 apple은 약 2.58%의 수익률을 보임. (크면 좋음.)
# 기울기가 1에 가까워야 바람직함. (위험에 대한 측도 : beta coefficient)
# --> 두가지 주식의 흐름이 비슷하다. (Jensen's alpha)
# Treynor index = (mu-Rf)/(beta)
# Sharpe ratio와 Treynor index는 값이 클 수록 바람직함.
# Jensen's alpha는 양수로서 커야함.
# beta coefficient는 1보다 약간 큰 정도가 바람직함.

# Homework : 우리나라에 있는 기업 3개를 찾아 KOSPI를 벤치마킹해서
# tangency portfolio와 동일가중평균(c(1/3,1/3,1/3))의 분석결과를 
# markdown으로 만들어서 준비. (5월 19일까지)

## 2014.5.19

# 기말과제 :
# 1. 우리나라 기업들 중 10개 선택한 다음
# 월별 수익률을 구해서 presentation으로 만들기. 
# (설명 첨부, 4월 30일 ~ 5월 14일 내용)

# 생산성 분석
# 기업을 평가할 때 투입 대비 산출이 어느 정도인지가 중요.
# ex. 매출액/임대료 = 30 (임대료에 30배만큼 매출을 내고있다.)
# --> 산출/투입 : 생산 효율성 (productivity or efficiency)
# 산출(월매출, 위생점수, 불만점수, 방문객수 ..) ,
# 투입(임대료, 직원수, 유지비, 재료비 ..)
# 임대료 대비 매출액의 최대값에 관심..
# frontier function (y=g(x)) -> Y=g(x)-u (u>=0, inefficiency factor)

## 2014.5.21

# DEA (Data Envelopment Analysis)
# Estimate of frontier function
install.packages("Benchmarking")
library(Benchmarking)
?dea # ORIENTATION : input 또는 output 결정

# example
X <- c(5,10,15,20,25,30,25,15) # Number of staffs (input)
Y <- c(100,250,300,280,330,380,290,250) # Number of cups (output)
plot(X,Y)

# VRS (Variable Returns-to-scale)
# input
res.in <- dea(X,Y,RTS="vrs") 
cbind(X,Y,res.in$eff)
# output
res.out <- dea(X,Y,RTS="vrs",ORIENTATION="out")
cbind(X,Y,res.in$eff,res.out$eff) # 보는 관점에 따라 순위가 달라짐.
dea.plot.frontier(X,Y,RTS="vrs") # 그림 그리기

## 2014.5.26

# FDH (Free Disposal Hull)
# data에 이상점이 존재하는 경우
# convexity라는 가정을 버리고 사용하는 방법.

# example
X <- c(5,10,15,20,25,30,25,15) # Number of staffs (input)
Y <- c(100,250,300,280,330,380,290,250) # Number of cups (output)
plot(X,Y)

# input
res.in <- dea(X,Y,RTS=0) 
cbind(X,Y,res.in$eff)
# output
res.out <- dea(X,Y,RTS=0,ORIENTATION="out")
cbind(X,Y,res.in$eff,res.out$eff) # 보는 관점에 따라 순위가 달라짐.
dea.plot.frontier(X,Y,RTS="fdh") # 그림 그리기

# CRS (Constant Returns-to-scale)
# input
res.in <- dea(X,Y,RTS="crs") 
cbind(X,Y,res.in$eff)
# output
res.out <- dea(X,Y,RTS="crs",ORIENTATION="out")
cbind(X,Y,res.in$eff,res.out$eff) # 보는 관점에 따라 순위가 달라짐.
dea.plot.frontier(X,Y,RTS="crs") # 그림 그리기

# DEA(VRS) + FDH => LFDH (Linearly Interpolated FDH)

# 과제 : 인터넷에서 DEA와 관련된 data 찾아볼 것. (input이 여러개인 것.)
# 배운 이론을 사용해서 분석을 한 다음 markdown으로 만들어 준비..
# (단, 먼저 찾은 data가 어느 경우에 맞는지 생각해볼 것.)
# 인터넷 창에 DEA라고 검색할 것. 

# 2014.5.28

# NBER data
dat09 <- read.csv("C:/Users/student/Desktop/Sung-yoon.R/Final Exam/final assignment 2/nber2009.csv",sep=",",header=T)
?dea
X <- dat09[,c(3,4,5,6,7,9,13)] # input
Y <- dat09[,10] # output
install.packages("Benchmarking")
library(Benchmarking)
dea.res <- dea(X,Y,RTS="vrs",ORIENTATION="in")
summary(dea.res)
cbind(dat09$naics,dea.res$eff) # 각 산업별에 따른 efficiency

# matcost -> vadd
X <- dat09[,9] # input
Y <- dat09[,10] # output
dea.plot.frontier(X,Y,RTS="vrs") # 이상점이 있으므로 로그변환
X <- log(dat09[,9]) # input
Y <- log(dat09[,10]) # output
dea.plot.frontier(X,Y,RTS="vrs")

# energy -> vadd
X <- log(dat09[,13]) # input
Y <- log(dat09[,10]) # output
dea.plot.frontier(X,Y,RTS="vrs")

# matcost, energy -> vadd
X <- log(dat09[,c(9,13)]) # input
Y <- log(dat09[,10]) # output
dea.res <- dea(X,Y,RTS="vrs",ORIENTATION="in")
cbind(dat09$naics,dea.res$eff)
X[1,] ; Y[1]
# 고양이와 개의 사료를 만드는 사업에서의 결과
# efficiency의 값이 81.98601% 가 나옴.
# 이 말은 재료비와 에너지의 투입이 과다했다는 이야기.
# 이를 82% 정도까지 줄이는 것이 구현 가능하다는 말.
X*dea.res$eff # 목표치 산출

# CRS vs VRS
X <- log(dat09[,c(9,13)]) # input
Y <- log(dat09[,10]) # output
dea.crs <- dea(X,Y,RTS="crs",ORIENTATION="in")
dea.vrs <- dea(X,Y,RTS="vrs",ORIENTATION="in")
cbind(dat09$naics,dea.crs$eff,dea.vrs$eff)
# 차이가 심하므로 VRS가 더 적합하다고 여겨짐.

# 과제 : 2000년부터 2009년까지 연도별로 원하는 사업(5가지)을 선정해서
# 적합한 방법론을 선택해서 efficiency의 변화를 조사해볼 것.
# input 요소를 matcost, energy 이외의 것으로 할 것. 
# (markdown으로 6월 2일까지)

# 기말과제 : portfolio(기말과제1), 위에 적힌 과제,
# NBER 데이터 중 특정년도를 잡아서 
# 모든 사업에 대해서 분석할 것.(input, output은 알아서 설정.) 
# (ppt로 만들어서 6월 13일까지 e-class에 제출.) 