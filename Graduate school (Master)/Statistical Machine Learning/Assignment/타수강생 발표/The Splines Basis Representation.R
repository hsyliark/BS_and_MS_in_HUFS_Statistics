##data 불러오기
library(ISLR)
data(Wage)
attach(Wage)
head(Wage)
##basis function b(k+3) 생성
knot.f<-function(x,knot){
  ifelse(x>knot,(x-knot)^3,0)
}

##cubic spline with 3 knots can be modeled as
qubic.fit<-lm(wage~age+I(age^2)+I(age^3)+knot.f(age,25)+knot.f(age,40)+knot.f(age,60))
age.grid<-unique(sort(age))
pred<-predict(qubic.fit,newdata=data.frame(age=age.grid),se=TRUE)
? bs
##plotting
par(mfrow=c(1,2))
plot(age,wage,col="gray",ylab="Wage",xlab="Age")
lines(age.grid,pred$fit,lwd=2,col="blue")
signi<-c(0.95,0.975,0.995)
c.value<-qt(signi[2],pred$df)
lines(age.grid,pred$fit+c.value*pred$se,lwd=2,,lty=3,col="red")
lines(age.grid,pred$fit-c.value*pred$se,lwd=2,,lty=3,col="red")
abline(v=c(25,40,60),lty=2)
legend("topright",legend=c("Cubic spline","Confidence Interval"),col=c("blue","red"),lty=c(1,2))

##lecture 7

library(splines)
fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
summary(fit)
agelims=range(age)
age.grid1=seq(from=agelims[1],to=agelims[2])
pre=predict(fit,newdata=list(age=age.grid1),se=T)
plot(age,wage,col="gray")
lines(age.grid1,pre$fit,lwd=2,col="blue")
lines(age.grid1,pre$fit+2*pre$se,lty="dashed",col="red")
lines(age.grid1,pre$fit-2*pre$se,lty="dashed",col="red")
dim(bs(age,knots=c(25,40,60)))
dim(bs(age,df=6))

## knot 수 변화

par(mfrow=c(3,3))
for(i in 6:14){
  fit=lm(wage~bs(age,df=i),data=Wage)
  summary(fit)
  agelims=range(age)
  age.grid=seq(from=agelims[1],to=agelims[2])
  pre=predict(fit,newdata=list(age=age.grid),se=T)
  plot(age,wage,col="gray",main=i)
  lines(age.grid,pre$fit,lwd=2)
  lines(age.grid,pre$fit+2*pre$se,lty="dashed",col="red")
  lines(age.grid,pre$fit-2*pre$se,lty="dashed",col="red")
}

##knot 수 변화 3,4,5,6
par(mfrow=c(2,2))
knot<-list(knot1<-c(25,40,60),knot2<-c(25,30,40,60),knot3<-c(25,30,40,50,60),knot4<-c(25,30,40,50,60,70))
for(i in 1:length(knot)){
  fit=lm(wage~bs(age,knots=knot[[i]]),data=Wage)
  agelims=range(age)
  age.grid=seq(from=agelims[1],to=agelims[2])
  pre=predict(fit,newdata=list(age=age.grid),se=T)
  plot(age,wage,col="gray",main=i+2)
  lines(age.grid,pre$fit,lwd=2)
  lines(age.grid,pre$fit+2*pre$se,lty="dashed",col="red")
  lines(age.grid,pre$fit-2*pre$se,lty="dashed",col="red")
  knots=knot[[i]]
  abline(v=knots,lty=2)
}

