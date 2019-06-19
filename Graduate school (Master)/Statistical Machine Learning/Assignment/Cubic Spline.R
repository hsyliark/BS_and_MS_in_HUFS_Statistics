# Making algorithm of cubic splines to Wage data in package 'ISLR'

library(ISLR)
data(Wage)
dim(Wage)
dat1 <- data.frame(age=Wage$age,wage=Wage$wage)
head(dat1)

attach(dat1)
age1 <- age ; age2 <- age^2 ; age3 <- age^3
age4 <- ifelse(age>25,(age-25)^3,0) ; age5 <- ifelse(age>40,(age-40)^3,0)
age6 <- ifelse(age>60,(age-60)^3,0)
dat1 <- cbind(dat1,age1,age2,age3,age4,age5,age6)
View(dat1)

attach(dat1)
cubic.spline.lm <- lm(wage~.-age,data=dat1)
cubic.spline.lm
summary(cubic.spline.lm)
range(age)

x <- seq(range(age)[1],range(age)[2],length=nrow(dat1))
x1 <- x ; x2 <- x^2 ; x3 <- x^3
x4 <- ifelse(x>25,(x-25)^3,0) ; x5 <- ifelse(x>40,(x-40)^3,0)
x6 <- ifelse(x>60,(x-60)^3,0)
preds <- predict(cubic.spline.lm,newdata=list(age1=x1,age2=x2,age3=x3,age4=x4,age5=x5,age6=x6),se=TRUE)
se.bands <- cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)

plot(age,wage,main="Cubic spline",xlab="Age",ylab="Wage",pch=20,col="darkgrey")
abline(v=25,lty=2) ; abline(v=40,lty=2) ; abline(v=60,lty=2)
lines(x,preds$fit,col="blue",lwd=3)
matlines(x,se.bands,lwd=2,col="red",lty=2)
legend("topright",legend=c("Cubic Spline","95% prediction interval"),col=c("blue","red"),lty=c(1,2))

# Using package 'splines'

library(splines)

agelims <- range(age)
age.grid <- seq(from=agelims[1],to=agelims[2])
fit <- lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
pred <- predict(fit,newdata=list(age=age.grid),se=T)

plot(age,wage,col="gray",main="Cubic Spline")
lines(age.grid,pred$fit,lwd=2,col="blue")
lines(age.grid,pred$fit+2*pred$se,lty="dashed",col="red")
lines(age.grid,pred$fit-2*pred$se,lty="dashed",col="red")
abline(v=25,lty=2) ; abline(v=40,lty=2) ; abline(v=60,lty=2)
legend("topright",legend=c("Cubic Spline","95% prediction interval"),col=c("blue","red"),lty=c(1,2))