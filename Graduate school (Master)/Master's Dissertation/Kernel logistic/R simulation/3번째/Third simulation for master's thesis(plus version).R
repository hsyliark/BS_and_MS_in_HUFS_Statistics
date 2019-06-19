##### Making function of Kernel-trick regression and classification




### German credit data

## Data for training

blood.train <- read.csv("D:/수업자료 (대학, 대학원)/대학원/석사졸업논문/논문주제선정/Kernel logistic/R simulation/Blood Transfusion.csv",header=T,sep=",")
View(blood.train)

## Data for testing

select <- sample(1:nrow(blood.train),size=round(nrow(blood.train)/10,digits=0),replace=T)
blood.train1 <- as.matrix(blood.train)
blood.test <- as.data.frame(blood.train1[select,-5])
View(blood.test)



### Package 'glmnet'

install.packages("glmnet")
library(glmnet)



#### Kernel ridge logistic classification with Gaussian kernel


### Making function for K-fold crossvalidation

grid.l <- 10^seq(-10,10,length=200)

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
    miss <- NULL
    cv.miss <- NULL
    
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
        
        penal.mod <- glmnet(K.train,y.train,family="binomial",lambda=lambda[j],alpha=0)
        predict.test <- predict(penal.mod,newx=t(K.test),type="class")
        
        miss[i] <- sum(predict.test != as.character(y.test))/test.size # misclassification rate
        
      }
      cv.miss[j] <- sum(miss)/k
      cat(j,",")
    }
    
    cat("\n","K-fold crossvalidation complete...")
    cat("\n","When average misclassification rate is ",min(cv.miss),", it is the best condition.","\n")
    
    result <- list(lambda=grid.l,cv.miss=cv.miss)
    plot(log(grid.l),cv.miss,xlab="log(lambda)",ylab="Average misclassification rate"
         ,main="K-fold crossvalidation",type="b")
    
    return(result) # Return some result to use
  }
}

(h <- cv.kernel(blood.train,5,grid.l,5)) # 5-fold crossvalidation
attributes(h)



### Making function for fitting

(best.lam <- h$lambda[h$cv.miss==min(h$cv.miss)])

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
    
    penal.mod <- glmnet(K.fit,y,family="binomial",lambda=lambda,alpha=0)
    y.hat <- predict(penal.mod,newx=K.fit,type="class")
    y.origin <- as.character(y)
    
    result <- list(data=dat.train,y.origin=y.origin,y.hat=y.hat,K.fit=K.fit)
    
    return(result) #Return some result
  }
}

(h1 <- fit.kernel(blood.train,best.lam,5))
attributes(h1)
mean(h1$y.origin==h1$y.hat)
h1$K.fit[1:5,1:5]



### Making function for predict


pred.kernel <- function(dat.train,dat.test,lambda,a) {
  
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
  penal.mod <- glmnet(K.train,y.train,family="binomial",lambda=lambda,alpha=0)
  predict.new <- predict(penal.mod,newx=t(K.test),type="class")
  
  result <- list(dat.train=data1,dat.test=data2,predict=predict.new
                 ,K=K,K.train=K.train,K.test=K.test)
  
  return(result)
}

(h2 <- pred.kernel(blood.train,blood.test,best.lam,5))
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
