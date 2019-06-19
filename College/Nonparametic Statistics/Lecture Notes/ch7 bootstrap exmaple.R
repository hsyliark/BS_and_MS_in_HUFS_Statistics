###### page 10 in Chapter 7 ###############

n=10
B=5000
x=rexp(n,1/50)
bm=c()
xb=matrix(0,B,n)
for (i in 1:B)
	{
	xb[i,]=sample(x,replace=TRUE)
	bm[i]=mean(xb[i,])
	}
plot(density(bm),main="",xlab="",ylab="",lwd=2,ylim=c(0,0.03))
lines(seq(0,150,0.01),dgamma(seq(0,150,0.01),shape=n,scale=50/n),lwd=2,lty=2,col=2)
lines(seq(0,150,0.01),dgamma(seq(0,150,0.01),shape=n,scale=mean(x)/n),lwd=2,lty=3,col=3)
lines(seq(0,150,0.01),dnorm(seq(0,150,0.01),mean(x),sqrt(var(x)/n)),lwd=2,lty=4,col=4)
legend(70,0.03,c("TRUE","Estimated gamma","Normal approximation","Bootstrap"),col=c(1,2,3,4),lty=c(1,2,3,4),lwd=2)

quantile(bm,c(0.025,0.975))
2*mean(x)-quantile(bm,c(0.025,0.975))[1]

2*mean(x)-quantile(bm,c(0.025,0.975))[2]

mean(x)-1.96*sqrt(var(bm))
mean(x)+1.96*sqrt(var(bm))
mean(bm)
var(bm)
1/n


###### exmaple : coefficient of variation ###############

x=rchisq(20,2)

bx=matrix(0,100,20)
for( i in 1:100)
{
bx[i,]=sample(x,replace=TRUE)
}
m=rowMeans(bx)
v=rowSums(bx^2)/20-(rowSums(bx)/20)^2

cv=sqrt(v)/m

library(MASS)

###### page 19 in Chapter 7 ###############

n=100
x=mvrnorm(n,c(0,0),matrix(c(4,1.5,1.5,1),2,2))
B=5000
br=c()
for( i in 1:B)
{

bi=sample(1:20,replace=TRUE)
bx=x[bi,]
br[i]=cor(bx)[1,2]
}
bm=mean(br)
bv=sum(br^2)/B-(sum(br)/B)^2

r=cor(x)[1,2]
par(mfrow=c(1,2))
hist(br,nclass=25,main="Histogram of bootstrap estimators",col=4)
bd=density(br)
plot(bd,xlab="",col=4,lwd=2,main="Density estimation")

c(r-1.96*sqrt(bv),r+1.96*sqrt(bv))
c(2*r-quantile(br,0.975),2*r-quantile(br,0.025))
c(quantile(br,0.025),quantile(br,0.975))


### real data : duration times in Chapter 7 ###############

x=c(1,5,12,15,20,26,78,145,158,358)
B=5000

###(3) parametric bootstrap
barx=mean(x);sx=sd(x)
B=5000
bx=matrix(0,B,length(x))
for( i in 1:B)
{
bx[i,]=rexp(length(x),1/barx)
}
p_bm=rowMeans(bx)
p_m=mean(p_bm)
p_v=var(p_bm)

###(3) nonparametric bootstrap
B=5000
bx=matrix(0,B,length(x))
for( i in 1:B)
{
bx[i,]=sample(x,replace=TRUE)
}
n_bm=rowMeans(bx)

n_m=mean(n_bm)
n_v=var(n_bm)

xg=seq(-50,300,1)
plot(xg,dgamma(xg,shape=length(x),scale=barx/length(x)),main="Distributions of the sample mean",xlab="",ylab="",type="l",lwd=3,ylim=c(0,0.017))
lines(xg,dnorm(xg,barx,sqrt(barx^2/length(x))),lwd=3,lty=2,col=2)
lines(density(p_bm),main="",xlab="",ylab="",lwd=3,col=3,lty=3)
lines(xg,dnorm(xg,barx,sqrt(sx^2/length(x))),lwd=3,lty=4,col=4)
lines(density(n_bm),main="",xlab="",ylab="",lwd=3,lty=5,col=6)

legend(150,0.016,c("gamma","norm with exp","Para- Bootstrap","CLT","Bootstrap"),col=c(1,2,3,4,6),lty=c(1,2,3,4,5),lwd=3)

### Confidence interval : real example

c(barx/qgamma(0.975,shape=length(x),scale=1/length(x)),barx/qgamma(0.025,shape=length(x),scale=1/length(x)))

c(barx-qnorm(0.975)*barx/sqrt(length(x)),barx+qnorm(0.975)*barx/sqrt(length(x)))

c(2*barx-quantile(p_bm,0.975),2*barx-quantile(p_bm,0.025))
c(quantile(p_bm,0.025),quantile(p_bm,0.975))

c(barx-qnorm(0.975)*sx/sqrt(length(x)),barx+qnorm(0.975)*sx/sqrt(length(x)))

c(2*barx-quantile(n_bm,0.975),2*barx-quantile(n_bm,0.025))
c(quantile(n_bm,0.025),quantile(n_bm,0.975))



