## Searching Auto data in ISLR package

install.packages("ISLR")
library(ISLR)
data(Auto)
dim(Auto)
View(Auto)

## Fitting Linear Regression Model

attach(Auto)
origin.lm <- lm(mpg~horsepower)
summary(origin.lm) # s.e(beta0)=0.717499 , s.e(beta1)=0.006446
attributes(origin.lm)
(origin.coef <- origin.lm$coefficients)
(origin.std <- c(0.717499,0.006446))

## Using Bootstrap

N <- 2000
bs.beta0 <- NULL ; bs.beta1 <- NULL
for (i in 1:N) {
  select.bs <- sample(1:392,392,replace=TRUE)
  bs.Auto <- Auto[select.bs,]
  bs.mpg <- bs.Auto$mpg 
  bs.horsepower <- bs.Auto$horsepower
  boot.lm <- lm(bs.mpg~bs.horsepower)
  bs.beta0[i] <- boot.lm$coefficients[1]
  bs.beta1[i] <- boot.lm$coefficients[2]
}
head(bs.beta0)
head(bs.beta1)
(origin.std <- c(0.717499,0.006446))
(boot.std <- c(sd(bs.beta0),sd(bs.beta1)))

## Drawing some graphs for distribution of regression coefficients

par(mfrow=c(1,2))
# Beta0
hist(bs.beta0,col="red",xlab="bootstrap beta0",ylab="density",prob=TRUE,
     main="Bootstrap result for regression coefficient beta0")
abline(v=origin.coef[1],col="green",lwd=2)
boxplot(bs.beta0,col="yellow",main="Boxplot of beta0 from bootstrap",
        xlab="bootstrap beta0")
abline(h=origin.coef[1],col="green",lwd=2)
# Beta1
hist(bs.beta1,col="blue",xlab="bootstrap beta1",ylab="density",prob=TRUE,
     main="Bootstrap result for regression coefficient beta1")
abline(v=origin.coef[2],col="green",lwd=2)
boxplot(bs.beta1,col="yellow",main="Boxplot of beta1 from bootstrap",
        xlab="bootstrap beta1")
abline(h=origin.coef[2],col="green",lwd=2)

### Another example

n <- 100
sig <- 2
x <- rnorm(n,0,2)

B <- 100000
boot <- NULL
for (b in 1:B) {
  boot.x <- x[sample(1:length(x),length(x),replace=TRUE)]
  boot <- c(boot,mean(boot.x))
}

sd(boot)
sig/sqrt(n)