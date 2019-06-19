#### Making simulation data



### beta1=2, beta2=1, beta3=3

x <- runif(200,-3,3)
y <- 2*x+1*x^2+3*x^3+rnorm(200,5,15)
x <- as.matrix(x) ; y <- as.matrix(y)



### Gaussian kernel 

p <- ncol(x)
sigma <- 1/p
n <- nrow(x)
K <- matrix(NA,n,n)
D <- as.matrix(dist(x,method="euclidean",p=2)) # Euclidean distance 
kernel <- function(m) exp(-sigma*m^2)

for (i in 1:n) {
  for (j in 1:n) {
    K[i,j] <- kernel(D[i,j])
  }
}  # Making kernel matrix



### Input some Lambda for penalty

## Input lambda 

par(mfrow=c(1,1))

# When lambda=2
lambda <- 2

d <- solve(K+lambda*diag(n))%*%y
yhat <- K%*%d
  
z <- seq(from=min(x),to=max(x),by=0.001)
  
plot(x,y,main="cubic trend (Lambda = 2)",xlab="x",ylab="y")
for (i in 1:n) {
  points(x[i],yhat[i],col="red",pch=20)
}                                         # Kernel estimation
lines(z,2*z+1*z^2+3*z^3,lty=2,col="blue") # Real type
legend("topleft",c("Kernel estimation","Real type"),col=c("red","blue"),lty=1)


# When lambda=1.5
lambda <- 1.5

d <- solve(K+lambda*diag(n))%*%y
yhat <- K%*%d

z <- seq(from=min(x),to=max(x),by=0.001)

plot(x,y,main="cubic trend (Lambda = 1.5)",xlab="x",ylab="y")
for (i in 1:n) {
  points(x[i],yhat[i],col="red",pch=20)
}                                         # Kernel estimation
lines(z,2*z+1*z^2+3*z^3,lty=2,col="blue") # Real type
legend("topleft",c("Kernel estimation","Real type"),col=c("red","blue"),lty=1)


# When lambda=1
lambda <- 1

d <- solve(K+lambda*diag(n))%*%y
yhat <- K%*%d

z <- seq(from=min(x),to=max(x),by=0.001)

plot(x,y,main="cubic trend (Lambda = 1)",xlab="x",ylab="y")
for (i in 1:n) {
  points(x[i],yhat[i],col="red",pch=20)
}                                         # Kernel estimation
lines(z,2*z+1*z^2+3*z^3,lty=2,col="blue") # Real type
legend("topleft",c("Kernel estimation","Real type"),col=c("red","blue"),lty=1)


# When lambda=0.5
lambda <- 0.5

d <- solve(K+lambda*diag(n))%*%y
yhat <- K%*%d

z <- seq(from=min(x),to=max(x),by=0.001)

plot(x,y,main="cubic trend (Lambda = 0.5)",xlab="x",ylab="y")
for (i in 1:n) {
  points(x[i],yhat[i],col="red",pch=20)
}                                         # Kernel estimation
lines(z,2*z+1*z^2+3*z^3,lty=2,col="blue") # Real type
legend("topleft",c("Kernel estimation","Real type"),col=c("red","blue"),lty=1)


# When lambda=0.2
lambda <- 0.2

d <- solve(K+lambda*diag(n))%*%y
yhat <- K%*%d

z <- seq(from=min(x),to=max(x),by=0.001)

plot(x,y,main="cubic trend (Lambda = 0.2)",xlab="x",ylab="y")
for (i in 1:n) {
  points(x[i],yhat[i],col="red",pch=20)
}                                         # Kernel estimation
lines(z,2*z+1*z^2+3*z^3,lty=2,col="blue") # Real type
legend("topleft",c("Kernel estimation","Real type"),col=c("red","blue"),lty=1)


# When lambda=0.1
lambda <- 0.1

d <- solve(K+lambda*diag(n))%*%y
yhat <- K%*%d

z <- seq(from=min(x),to=max(x),by=0.001)

plot(x,y,main="cubic trend (Lambda = 0.1)",xlab="x",ylab="y")
for (i in 1:n) {
  points(x[i],yhat[i],col="red",pch=20)
}                                         # Kernel estimation
lines(z,2*z+1*z^2+3*z^3,lty=2,col="blue") # Real type
legend("topleft",c("Kernel estimation","Real type"),col=c("red","blue"),lty=1)


# When lambda=0.05
lambda <- 0.05

d <- solve(K+lambda*diag(n))%*%y
yhat <- K%*%d

z <- seq(from=min(x),to=max(x),by=0.001)

plot(x,y,main="cubic trend (Lambda = 0.05)",xlab="x",ylab="y")
for (i in 1:n) {
  points(x[i],yhat[i],col="red",pch=20)
}                                         # Kernel estimation
lines(z,2*z+1*z^2+3*z^3,lty=2,col="blue") # Real type
legend("topleft",c("Kernel estimation","Real type"),col=c("red","blue"),lty=1)


# When lambda=0.01
lambda <- 0.01

d <- solve(K+lambda*diag(n))%*%y
yhat <- K%*%d

z <- seq(from=min(x),to=max(x),by=0.001)

plot(x,y,main="cubic trend (Lambda = 0.01)",xlab="x",ylab="y")
for (i in 1:n) {
  points(x[i],yhat[i],col="red",pch=20)
}                                         # Kernel estimation
lines(z,2*z+1*z^2+3*z^3,lty=2,col="blue") # Real type
legend("topleft",c("Kernel estimation","Real type"),col=c("red","blue"),lty=1)


# When lambda=0.007
lambda <- 0.007

d <- solve(K+lambda*diag(n))%*%y
yhat <- K%*%d

z <- seq(from=min(x),to=max(x),by=0.001)

plot(x,y,main="cubic trend (Lambda = 0.007)",xlab="x",ylab="y")
for (i in 1:n) {
  points(x[i],yhat[i],col="red",pch=20)
}                                         # Kernel estimation
lines(z,2*z+1*z^2+3*z^3,lty=2,col="blue") # Real type
legend("topleft",c("Kernel estimation","Real type"),col=c("red","blue"),lty=1)


# When lambda=0.005
lambda <- 0.005

d <- solve(K+lambda*diag(n))%*%y
yhat <- K%*%d

z <- seq(from=min(x),to=max(x),by=0.001)

plot(x,y,main="cubic trend (Lambda = 0.005)",xlab="x",ylab="y")
for (i in 1:n) {
  points(x[i],yhat[i],col="red",pch=20)
}                                         # Kernel estimation
lines(z,2*z+1*z^2+3*z^3,lty=2,col="blue") # Real type
legend("topleft",c("Kernel estimation","Real type"),col=c("red","blue"),lty=1)


# When lambda=0.002
lambda <- 0.002

d <- solve(K+lambda*diag(n))%*%y
yhat <- K%*%d

z <- seq(from=min(x),to=max(x),by=0.001)

plot(x,y,main="cubic trend (Lambda = 0.002)",xlab="x",ylab="y")
for (i in 1:n) {
  points(x[i],yhat[i],col="red",pch=20)
}                                         # Kernel estimation
lines(z,2*z+1*z^2+3*z^3,lty=2,col="blue") # Real type
legend("topleft",c("Kernel estimation","Real type"),col=c("red","blue"),lty=1)


# When lambda=0.001
lambda <- 0.001

d <- solve(K+lambda*diag(n))%*%y
yhat <- K%*%d

z <- seq(from=min(x),to=max(x),by=0.001)

plot(x,y,main="cubic trend (Lambda = 0.001)",xlab="x",ylab="y")
for (i in 1:n) {
  points(x[i],yhat[i],col="red",pch=20)
}                                         # Kernel estimation
lines(z,2*z+1*z^2+3*z^3,lty=2,col="blue") # Real type
legend("topleft",c("Kernel estimation","Real type"),col=c("red","blue"),lty=1)



## Simulation
par(mfrow=c(2,4))
lambda <- seq(2,0.01,length=100)
head(lambda)
tail(lambda)

for (i in 1:length(lambda)) {
  d <- solve(K+lambda[i]*diag(n))%*%y
  yhat <- K%*%d
  
  z <- seq(from=min(x),to=max(x),by=0.001)
  
  plot(x,y,main="cubic trend",xlab="x",ylab="y")
  for (j in 1:n) {
    points(x[j],yhat[j],col="darkgreen",pch=20)
  }                                         # Kernel estimation
  lines(z,2*z+1*z^2+3*z^3,lty=2,col="blue") # Real type
  legend("topleft",c("Kernel estimation","Real type"),col=c("darkgreen","blue"),lty=1)
  
  cat(i,"\n")
}