#### Simulation data

dat.gen <- function(n, beta, seed){
  if(!is.null(seed)) set.seed(seed)
  x1 <- runif(n,-10,10) ; x2 <- runif(n,-10,10) ; x3 <- runif(n,-10,10)
  x <- cbind(1, x1, x2, x3)
  p <- exp(x%*%beta) / (1+exp(x%*%beta))
  y <- apply(as.matrix(p), c(1,2), rbinom, n=1, size=1)
  dat <- data.frame(y=y, x1=x1, x2=x2, x3=x3)
  return( dat )
}

beta <- c(1, 0.1, -2, 0.5)
train.sim <- dat.gen(500, beta, seed=1)
test.sim <- dat.gen(2000, beta, seed=12)



#------------------------#------------------------#------------------------#------------------------





#### Making some functions






### 1. Calculate coefficients using theory


## Maximum Likelihood Estimation + Newton-Raphson algorithm
## Iteratively Reweighted Least Squares (IRLS)


my.kernel.logistic <- function(y, K, lambda) {
  
  # y : response variable
  # K : matrix from Gaussian kernel transformation
  # lambda : penalty parameter
  
  y <- as.matrix(y) ; K <-as.matrix(K)
  
  D <- as.matrix(rep(0,ncol(K))) # The initial value of coefficient vector
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
    
    new.d <- solve(K + lambda*solve(W))%*%(K%*%D[,i] + solve(W)%*%(y-p))
    D <- cbind(D,new.d)
    
    diff[i] <- sqrt(sum((new.d - D[,i])^2))/sqrt(sum((D[,i])^2))
    
    cat(i,",")
    
    if (diff[i] < 10^(-4)) break
    
  }
  
  cat("\n","Algorithm converged...","\n")
  
  d.hat <- D[,min(100,length(diff))]
  
  logit.hat <- K%*%d.hat
  pi.hat <- exp(logit.hat)/(1+exp(logit.hat))
  y.pred <- rep(1,length(logit.hat))
  y.pred[pi.hat < 0.5] <- 0
  
  res <- list(d.hat=d.hat, y.pred=y.pred)
}







### 2. Making function for fitting


fit.kernel <- function(dat, lambda) {
  
  # dat : Data frame with a response variable is appeared in the first column...
  # lambda : penalty parameter
  
  if(lambda <= 0){
    cat("Lambda is non-positive value. Please insert positive value of lambda.")
  }
  
  else {
    data1 <- as.data.frame(dat)
    X <- as.matrix(data1[,-1]) # Independent variables
    y <- as.matrix(data1[,1]) # Dependent variable
    
    
    p <- ncol(X)
    sigma <- 1/p
    n <- nrow(X)
    K.fit <- matrix(NA,n,n)
    D.fit <- as.matrix(dist(X,method="euclidean",p=2)) # Euclidean distance 
    G.kernel <- function(m) exp(-sigma*m^2) # Gaussian kernel
    
    K.fit <- G.kernel(D.fit)
    
    
    g <- my.kernel.logistic(y, K.fit, lambda)
    
    d.hat <- g$d.hat
    y.pred <- g$y.pred
    
    
    result <- list(data=dat, d.hat=d.hat, y.pred=y.pred)
    
  }
}







### 3. Making function for predict

  
pred.kernel <- function(dat.train, dat.test, d.hat) {
  
  # dat.train : Data frame for training with a response variable is appeared in the first column...
  # dat.test : Data frame for testing with a response variable is appeared in the first column...
  
  
  data1 <- as.data.frame(dat.train)
  data2 <- as.data.frame(dat.test)
  n <- nrow(data1)
  
  # Training data
  X.train <- as.matrix(data1[,-1])
  y.train <- as.matrix(data1[,1])
  
  # Test data
  X.test <- as.matrix(data2[,-1])
  y.test <- as.matrix(data2[,1])
  
  X1 <- rbind(X.train, X.test)
  
  
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
  
  rate.miss <- mean(predict.new != y.test)
  
  
  result <- list(dat.train=data1, dat.test=data2, predict.new=predict.new,
                 K=K, K.train=K.train, K.test=K.test,
                 misclassification.rate=rate.miss)
  
}







### 4. Making function for K-fold crossvalidation


cv.kernel <- function(data, k, grid.l) {
  
  # data : Data frame with a response variable is appeared in the first column...
  # k : number of criterion for K-fold crossvalidation
  # grid.l : The row of penalty parameter lambda
  
  check <- (grid.l > 0)
  n.check <- length(check)
  
  if(sum(check) != n.check){
    cat("Some of lambda's values are non-positive.
        Please insert positive values of lambda vector...","\n")
  }
  
  else {
    lambda <- grid.l
    r <- length(lambda)
    
    logL <- NULL # minus log-likelihood
    cv.logL <- NULL
    
    data1 <- as.data.frame(data)
    X <- as.matrix(data1[,-1]) # Independent variables
    y <- as.matrix(data1[,1]) # Dependent variable
    n <- nrow(X)
    
    
    cat("K-fold crossvalidation is start...","\n")
    
    for (j in 1:r) {
      for (i in 0:(k-1)) {
        cv.index <- sample(1:n,n,replace=F)
        test.index <- cv.index[(1:n)%/%k==i]
        
        X.train <- X[-test.index,] ; X.test <- X[test.index,]
        y.train <- y[-test.index,] ; y.test <- y[test.index,]
        test.size <- length(test.index)
        
        train.data <- cbind(y.train, X.train)
        test.data <- cbind(y.test, X.test) 
              
        
        a1 <- fit.kernel(train.data,lambda[j])
        train.d.hat <- a1$d.hat
        
        a2 <- pred.kernel(train.data,test.data,train.d.hat)
        K.test <- a2$K.test
        
        
        test.logit.hat <- t(K.test)%*%train.d.hat
        test.pi.hat <- exp(test.logit.hat)/(1 + exp(test.logit.hat))
        
        predict.test <- rep(1,length(test.logit.hat))
        predict.test[test.pi.hat < 0.5] <- 0
        

        logL[i] <- -sum(y.test*test.logit.hat - log(1+exp(test.logit.hat)))
                  
        
      }
      
      cv.logL[j] <- sum(logL)/k
      cat(j,"\n","\n")
    }
    
    cat("\n","K-fold crossvalidation complete...")
    cat("\n","When the value of minus log-likelihood is ",min(cv.logL),
        ", it is the best condition.","\n")
    
    
    plot(log(grid.l),cv.logL,xlab="log(lambda)",ylab="Minus log-likelihood"
         ,main="K-fold crossvalidation",type="b")
    
    result <- list(lambda=grid.l, cv.logL=cv.logL)
    
  }
}






#------------------------#------------------------#------------------------#------------------------







## 10-fold crossvalidation

grid.l <- 10^seq(-10,10,length=200)

h <- cv.kernel(train.sim, 10, grid.l) 
attributes(h)
cbind(h$lambda, h$cv.logL)


## Fitting 

(best.lam <- h$lambda[h$cv.logL == min(h$cv.logL)][1])
h1 <- fit.kernel(train.sim, best.lam)
attributes(h1)
mean(train.sim$y != h1$y.pred) # misclassification rate
cbind(train.sim$y, h1$y.pred)


## Predicting

sim.d.hat <- h1$d.hat

h2 <- pred.kernel(train.sim, test.sim, sim.d.hat)
attributes(h2)
h2$predict.new  
h2$misclassification.rate
cbind(test.sim$y, h2$predict.new)

h2$K.train[1:5,1:5]
h2$K.test[1:5,1:5]



























