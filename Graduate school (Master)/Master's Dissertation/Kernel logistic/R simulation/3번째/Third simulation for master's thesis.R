#### Making function of Kernel-trick regression and classification



### Simulation data

## Data for training
x1 <- runif(200,-3,3)
x2 <- runif(200,-3,3)
x3 <- runif(200,-3,3)
x4 <- runif(200,-3,3)
x5 <- runif(200,-3,3)
y <- 1*x1+2*x2^2+3*x3^3+4*x4^4+5*x5^5+rnorm(200,5,15)
x1 <- as.matrix(x1) ; x2 <- as.matrix(x2) ; x3 <- as.matrix(x3)
x4 <- as.matrix(x4) ; x5 <- as.matrix(x5) ; y <- as.matrix(y)

dat1 <- cbind(y,x1,x2,x3,x4,x5)
colnames(dat1) <- c('y','x1','x2','x3','x4','x5')
dat1 <- as.data.frame(dat1)




### Making function for K-fold crossvalidation

grid.l <- 10^seq(-10,10,length=300)

cv.kernel <- function(data,k,grid.l,a) {
  
  check <- (grid.l > 0)
  n.check <- length(check)
  
  if(sum(check) != n.check){
    cat("Some of lambda's values are non-positive.
        Please insert positive values of lambda vector...","\n")
  }
  
  else {
    lambda <- grid.l
    r <- length(lambda)
    mse <- NULL
    cv.mse <- NULL
    
    data1 <- as.data.frame(data)
    X <- as.matrix(data1[,-a]) # Independent variables
    y <- as.matrix(data1[,a]) # Dependent variable
    n <- nrow(X)
    
    
    cat("K-fold crossvalidation is start...","\n")
    
    for (j in 1:r) {
      for (i in 0:(k-1)) {
        cv.index <- sample(1:n,n,replace=F)
        test.index <- cv.index[(1:n)%/%k==i]
        X.train <- X[-test.index,] ; X.test <- X[test.index,]
        y.train <- y[-test.index,] ; y.test <- y[test.index,]
        test.size <- length(test.index)
        X1 <- rbind(X.train,X.test)
        
        p <- ncol(X1)
        sigma <- 1/p
        n1 <- nrow(X1)
        K <- matrix(NA,n1,n1)
        D <- as.matrix(dist(X1,method="euclidean",p=2)) # Euclidean distance 
        G.kernel <- function(m) exp(-sigma*m^2) # Gaussian kernel
        
        K <- G.kernel(D)
        K.train <- K[1:(n1-test.size),1:(n1-test.size)]
        K.test <- K[1:(n1-test.size),(n1-test.size+1):n1]
        
        d.train <- solve(K.train+lambda[j]*diag(n1-test.size))%*%y.train
        predict.test <- t(K.test)%*%d.train
        
        mse[i] <- sum((y.test-predict.test)^2)/test.size
      }
      cv.mse[j] <- sum(mse)/k
      cat(j,",")
    }
    
    cat("\n","K-fold crossvalidation complete...")
    cat("\n","When test MSE is ",min(cv.mse),", it is the best condition.","\n")
    
    result <- list(lambda=grid.l,cv.mse=cv.mse)
    plot(log(grid.l),cv.mse,xlab="log(lambda)",ylab="Test MSE"
         ,main="K-fold crossvalidation",type="b")
    
    return(result) # Return some result to use
  }
}

(h <- cv.kernel(dat1,10,grid.l,1)) # 10-fold crossvalidation
attributes(h)


  
### Making function for fitting

(best.lam <- h$lambda[h$cv.mse==min(h$cv.mse)])

fit.kernel <- function(dat.train,lambda,a) {
  
  if(lambda <= 0){
    cat("Lambda is non-positive value. Please insert positive value of lambda.")
  }
  
  else {
    data1 <- as.data.frame(dat.train)
    X <- as.matrix(data1[,-a]) # Independent variables
    y <- as.matrix(data1[,a]) # Dependent variable
    
    
    p <- ncol(X)
    sigma <- 1/p
    n <- nrow(X)
    K.fit <- matrix(NA,n,n)
    D.fit <- as.matrix(dist(X,method="euclidean",p=2)) # Euclidean distance 
    G.kernel <- function(m) exp(-sigma*m^2) # Gaussian kernel
    
    K.fit <- G.kernel(D.fit)
    
    d.hat <- solve(K.fit+lambda*diag(n))%*%y
    y.hat <- K.fit%*%d.hat
    res <- y-y.hat
    std.res <- scale(y-y.hat)
    
    result <- list(data=dat.train,d.hat=d.hat,y.hat=y.hat,res=res,
                   std.res=std.res,K.fit=K.fit)
    
    return(result) #Return some result
  }
}

(h1 <- fit.kernel(dat1,best.lam,1))
attributes(h1)
h1$K.fit[1:5,1:5]



### Making function for predict

d.hat <- h1$d.hat

## Data for testing
z1 <- runif(10,-3,3)
z2 <- runif(10,-3,3)
z3 <- runif(10,-3,3)
z4 <- runif(10,-3,3)
z5 <- runif(10,-3,3)
z1 <- as.matrix(z1) ; z2 <- as.matrix(z2) ; z3 <- as.matrix(z3)
z4 <- as.matrix(z4) ; z5 <- as.matrix(z5)
dat2 <- cbind(z1,z2,z3,z4,z5)
colnames(dat2) <- c('x1','x2','x3','x4','x5')
dat2 <- as.data.frame(dat2)


## Making function  
pred.kernel <- function(dat.train,dat.test,d.hat,a) {
  
  data1 <- as.data.frame(dat.train)
  data2 <- as.data.frame(dat.test)
  n <- nrow(data1)
  
  # Training data
  X.train <- as.matrix(data1[,-a])
  y.train <- as.matrix(data1[,a])
  # Test data
  X.test <- as.matrix(data2)
  
  X1 <- rbind(X.train,X.test)

  
  p <- ncol(X1)
  sigma <- 1/p
  n1 <- nrow(X1)
  K <- matrix(NA,n1,n1)
  D <- as.matrix(dist(X1,method="euclidean",p=2)) # Euclidean distance 
  G.kernel <- function(m) exp(-sigma*m^2) # Gaussian kernel
  
  K <- G.kernel(D)
  K.train <- K[1:n,1:n]
  K.test <- K[1:n,(n+1):n1]
  predict.new <- t(K.test)%*%d.hat
  
  result <- list(dat.train=data1,dat.test=data2,predict=predict.new
                 ,K=K,K.train=K.train,K.test=K.test)
  
  return(result)
}

(h2 <- pred.kernel(dat1,dat2,d.hat,1))
attributes(h2)
h2$predict  
h2$K.train[1:5,1:5]
h2$K.test[1:5,1:5]
  



### Choosing packages

install.packages("ridge")
library(ridge)
?logisticRidge

install.packages("rms")
library(rms)
?lrm

install.packages("penalized")
library(penalized)
?penalized


    
    