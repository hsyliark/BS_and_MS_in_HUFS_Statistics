#### Simulation data


x1 <- runif(200,-10,10) ; x2 <- runif(200,-10,10) ; x3 <- runif(200,-10,10)
p <- exp(1+0.1*x1-2*x2^2+0.5*x3^3)/(1+exp(1+0.1*x1-2*x2^2+0.5*x3^3))
y <- NULL
for (i in 1:length(p)) {
  y[i] <- rbinom(1,1,p[i])
}
train.sim <- cbind(y,x1,x2,x3)
colnames(train.sim) <- c('y','x1','x2','x3')

z1 <- runif(10,-10,10) ; z2 <- runif(10,-10,10) ; z3 <- runif(10,-10,10)
test.sim <- cbind(z1,z2,z3)
colnames(test.sim) <- c('z1','z2','z3')




### Calculate coefficients using theory


## Maximum Likelihood Estimation + Newton-Raphson algorithm
## Iteratively Reweighted Least Squares (IRLS)


my.kernel.logistic <- function(y,K,lambda) {
  
  y <- as.matrix(y) ; K <-as.matrix(K)
  
  D <- as.matrix(rep(0,ncol(K)))
  diff <- NULL
  
  for (i in 1:100) {
    
    
    p <- matrix(NA,nrow(K),1)
    p1 <- matrix(NA,nrow(K),1)
    
    C <- (K)%*%D[,i]
    
    my.prob1 <- function(m) exp(m)/(1+exp(m))
    my.prob2 <- function(m) (exp(m)/(1+exp(m)))*(1-exp(m)/(1+exp(m)))
    
    p <- my.prob1(C)
    p1 <- my.prob2(C)
    
    W <- diag(as.vector(p1))
    
    new.d <- solve(K+lambda*solve(W))%*%(K%*%D[,i]+solve(W)%*%(y-p))
    D <- cbind(D,new.d)
    
    diff[i] <- sqrt(sum((new.d-D[,i])^2))/sqrt(sum((D[,i])^2))
    
    cat(i,",")
    
    if (diff[i] < 10^(-4)) break
    
  }
  
  cat("\n","Algorithm converged...","\n")
  
  d.hat <- D[,min(100,length(diff))]
  
  logit.hat <- K%*%d.hat
  pi.hat <- exp(logit.hat)/(1+exp(logit.hat))
  y.pred <- rep(1,length(logit.hat))
  y.pred[pi.hat < 0.5] <- 0
  
  res <- list(d.hat=d.hat,y.pred=y.pred)
}


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
        
        
        g <- my.kernel.logistic(y.train,K.train,lambda[j])
        
        d.train <- g$d.hat 
        
        test.logit.hat <- t(K.test)%*%d.train
        test.pi.hat <- exp(test.logit.hat)/(1+exp(test.logit.hat))
        
        predict.test <- rep(1,length(test.logit.hat))
        predict.test[test.pi.hat < 0.5] <- 0
        
        mse[i] <- mean(y.test != predict.test)
      }
      cv.mse[j] <- sum(mse)/k
      cat(j,"\n","\n")
    }
    
    cat("\n","K-fold crossvalidation complete...")
    cat("\n","When test MSE is ",min(cv.mse),", it is the best condition.","\n")
    
    
    plot(log(grid.l),cv.mse,xlab="log(lambda)",ylab="Test MSE"
         ,main="K-fold crossvalidation",type="b")
    result <- list(lambda=grid.l,cv.mse=cv.mse)
    
  }
}

h <- cv.kernel(train.sim,10,grid.l,1) # 10-fold crossvalidation
attributes(h)
cbind(h$lambda,h$cv.mse)



### Making function for fitting

(best.lam <- h$lambda[h$cv.mse==min(h$cv.mse)][1])

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
    

    g <- my.kernel.logistic(y,K.fit,lambda)
    
    d.hat <- g$d.hat
    y.pred <- g$y.pred
    
    
    result <- list(data=dat.train,d.hat=d.hat,y.pred=y.pred)
    
  }
}

h1 <- fit.kernel(train.sim,best.lam,1)
attributes(h1)
mean(y != h1$y.pred) # misclassification rate
cbind(y,h1$y.pred)


### Making function for predict

sim.d.hat <- h1$d.hat


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
  
  logit.hat <- t(K.test)%*%d.hat
  pi.hat <- exp(logit.hat)/(1+exp(logit.hat))
  
  predict.new <- rep(1,length(logit.hat))
  predict.new[pi.hat < 0.5] <- 0
  
  
  result <- list(dat.train=data1,dat.test=data2,predict.new=predict.new
                 ,K=K,K.train=K.train,K.test=K.test)
  
}

h2 <- pred.kernel(train.sim,test.sim,sim.d.hat,1)
attributes(h2)
h2$predict.new  
h2$K.train[1:5,1:5]
h2$K.test[1:5,1:5]




#### Real data (German credit)

german <- read.csv("D:/수업자료 (대학, 대학원)/대학원/석사졸업논문/논문주제선정/Kernel logistic/R simulation/4번째/germancredit.csv",sep=",",header=T)

attributes(german)

select <- sample(1:nrow(german),size=round(nrow(german)/10,digits=0),replace=F)
german.train <- german[-select,]
german.test <- german[select,-1]

v <- cv.kernel(german,10,grid.l,1) ## Too long time is required...
(best.lam.german <- v$lambda[v$cv.mse==min(v$cv.mse)])
v1 <- fit.kernel(german.train,best.lam.german,1)
(german.d.hat <- h1$d.hat)
v2 <- pred.kernel(german.train,german.test,german.d.hat,1)
