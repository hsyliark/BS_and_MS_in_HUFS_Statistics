#### Simulation data

dat.gen <- function(n, beta, seed=NULL){
  if(!is.null(seed)) set.seed(seed)
  x1 <- runif(n,-10,10) ; x2 <- runif(n,-10,10) ; x3 <- runif(n,-10,10)
  x <- cbind(1, x1, x2, x3)
  linpred <- x%*%beta
  p <- exp(x%*%beta) / (1+exp(x%*%beta))
  y <- apply(as.matrix(p), c(1,2), rbinom, n=1, size=1)
  dat <- data.frame(y=y, x1=x1, x2=x2, x3=x3, linpred=linpred)
  return( dat )
}




#------------------------#------------------------#------------------------#------------------------





#### Making some functions






### 1. Making kernel matrix


my.kernel.matrix <- function(dat.train, dat.test) {
  
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
  D <- as.matrix(dist(X1,method="euclidean",p=2)) # Euclidean distance 
  G.kernel <- function(m) exp(-sigma*m^2) # Gaussian kernel
  
  K <- G.kernel(D)
  K.train <- K[1:n,1:n]
  K.test <- K[1:n,(n+1):n1]
  
  result <- list(K.train=K.train, K.test=K.test,
                 y.train=y.train, y.test=y.test)
  
}
  
  
  
  
  



### 2. Calculate coefficients using theory


## Maximum Likelihood Estimation + Newton-Raphson algorithm
## Iteratively Reweighted Least Squares (IRLS)


my.kernel.logistic <- function(y, K, lambda) {
  
  # y : response variable
  # K : matrix from Gaussian kernel transformation
  # lambda : penalty parameter
  
  if(lambda <= 0) 
    stop("Lambda is non-positive value. Please insert positive value of lambda.")
  
  y <- as.matrix(y) ; K <-as.matrix(K)
  
  d.old <- rep(1,ncol(K)) # The initial value of coefficient vector
  d.old <- d.old/sqrt(sum(d.old^2))

  for (i in 1:100) {
    
    
    p <- matrix(NA,nrow(K),1)
    p1 <- matrix(NA,nrow(K),1)
    
    V <- K%*%d.old
    
    my.prob1 <- function(m) exp(m)/(1+exp(m))
    my.prob2 <- function(m) (exp(m)/(1+exp(m)))*(1-exp(m)/(1+exp(m)))
    
    p <- my.prob1(V)
    p1 <- my.prob2(V)
    
    W.inverse <- diag(1/as.vector(p1))
    
    d.new <- solve(K + lambda*W.inverse)%*%(K%*%d.old + W.inverse%*%(y-p))
    
    new.logit.hat <- K%*%d.new ; old.logit.hat <- K%*%d.old
    
    log.like <- function(y, logit.hat) {
      sum(y*logit.hat - log(1+exp(logit.hat)))
    } # Log-likelihood function
    diff <- abs(log.like(y, new.logit.hat) - log.like(y, old.logit.hat))  
    
    # diff <- sqrt(sum((d.new - d.old)^2))/sqrt(sum(d.old^2))
    
    cat("( iteration , difference ) = (", i, ",", diff, ")\n")
    
    if (diff < 1E-8) break
    
    d.old <- d.new
    
  }
  
  cat("Algorithm converged...","\n\n")
  

  logit.hat <- K%*%d.new
  pi.hat <- exp(logit.hat)/(1+exp(logit.hat))
  y.pred <- rep(1,length(logit.hat))
  y.pred[pi.hat < 0.5] <- 0
  
  res <- list(d.hat=d.new, y.pred=y.pred)

}







### 3. Making function for fitting


fit.kernel <- function(y.train, K.train, lambda) {
  
  # y.train : Dependent variable of training data
  # K.train : Kernel matrix from training data 
  # lambda : penalty parameter
  
  if(lambda <= 0) 
    stop("Lambda is non-positive value. Please insert positive value of lambda.")
  
  
  y.train <- as.matrix(y.train)
  K.train <- as.matrix(K.train) 
         
  g <- my.kernel.logistic(y.train, K.train, lambda)
    
  d.hat <- g$d.hat
  y.pred <- g$y.pred
  
  rate.miss <- mean(y.pred != y.train)
    
    
  result <- list(d.hat=d.hat, y.train=y.train, y.pred=y.pred,
                 misclassification.rate=rate.miss)
    
}







### 4. Making function for predict

  
pred.kernel <- function(y.test, K.test, d.hat) {
  
  # y.test : Dependent variable of test data
  # K.test : Kernel matrix from test data 
  # d.hat : Estimator of vector d from training data
  
  
  y.test <- as.matrix(y.test) 
  K.test <- as.matrix(K.test)
  d.hat <- as.matrix(d.hat)
  
  
  logit.hat <- t(K.test)%*%d.hat
  pi.hat <- exp(logit.hat)/(1+exp(logit.hat))
  
  predict.new <- rep(1,length(logit.hat))
  predict.new[pi.hat < 0.5] <- 0
  
  rate.miss <- mean(predict.new != y.test)
  
  
  result <- list(y.test=y.test, predict.new=predict.new, logit.hat=logit.hat,
                 misclassification.rate=rate.miss)
  
}







### 5. Making function for K-fold crossvalidation


cv.kernel <- function(y.train, K.train, k, grid.l) {
  
  
  # y.train : Dependent variable of training data
  # K.train : Kernel matrix from training data 
  # k : number of criterion for K-fold crossvalidation
  # grid.l : The row of penalty parameter lambda
  
  check <- (grid.l > 0)
  n.check <- length(check)
  
  if(sum(check) != n.check)
    stop("Some of lambda's values are non-positive.
        Please insert positive values of lambda vector...","\n")
  

  lambda <- grid.l
  r <- length(lambda)
    
  
  K.sim <- as.matrix(K.train)
  y.sim <- as.matrix(y.train)
  n <- nrow(K.sim)
  
  cv.index <- sample(1:n,n,replace=F)  
  cv.logL <- NULL   
    
  cat("K-fold crossvalidation is start...","\n")
  
  
  for (j in 1:r) {
    
    logL <- NULL # minus log-likelihood
    
            
    for (i in 0:(k-1)) {
      
      
      test.index <- cv.index[(1:n)%/%k==i]
        
      K.sim.train <- K.sim[-test.index, -test.index] ; K.sim.test <- K.sim[-test.index, test.index]
      y.sim.train <- y.sim[-test.index,] ; y.sim.test <- y.sim[test.index,]
      test.size <- length(test.index)
        
              
      a1 <- fit.kernel(y.sim.train, K.sim.train, lambda[j])
      train.d.hat <- a1$d.hat
        
      a2 <- pred.kernel(y.sim.test, K.sim.test, train.d.hat)
      test.logit.hat <- a2$logit.hat      
        

      logL <- c(logL, -sum(y.sim.test*test.logit.hat - log(1+exp(test.logit.hat))) )
                  
        
    }
      
    cv.logL <- rbind(cv.logL, logL)
    cat(j,"\n","\n")
    }
    
  cat("\n","K-fold crossvalidation complete...")
  
  se.logL <- apply(cv.logL, 1, sd)
  mean.logL <- rowMeans(cv.logL)
  idx <- which.min(mean.logL)
        
  plot(log(grid.l, base=10),rowMeans(cv.logL),xlab="log10(lambda)",ylab="Minus log-likelihood"
      ,main="K-fold crossvalidation",type="b")
  abline(h=mean.logL[idx]+se.logL[idx], col="red", lty=2)
  abline(v=log(max(lambda[ mean.logL < mean.logL[idx]+se.logL[idx] ]), base=10), col="blue", lty=2)
    
  result <- list(lambda=grid.l, cv.logL=cv.logL)
    

}






#------------------------#------------------------#------------------------#------------------------


## Making simulation data


beta <- c(1, 0.1, -2, 0.5)
train.sim <- dat.gen(500, beta)
test.sim <- dat.gen(3000, beta)

train.linpred <- train.sim[,5]; train.sim <- train.sim[,-5]
test.linpred <- test.sim[,5]; test.sim <- test.sim[,-5]

table(train.sim$y)
table(test.sim$y)


## 10-fold crossvalidation


u <- my.kernel.matrix(train.sim, test.sim)
K.train <- u$K.train ; K.test <- u$K.test  
y.train <- u$y.train ; y.test <- u$y.test

grid.l <- 10^seq(-6,1,length=300)

h <- cv.kernel(y.train, K.train, 10, grid.l) 
attributes(h)
dim(h$cv.logL)



## Fitting 


se.logL <- apply(h$cv.logL, 1, sd)
mean.logL <- rowMeans(h$cv.logL)
(idx <- which.min(mean.logL))
h$lambda[ mean.logL < mean.logL[idx]+se.logL[idx] ]
(best.lam <- max(h$lambda[ mean.logL < mean.logL[idx]+se.logL[idx] ]))


h1 <- fit.kernel(y.train, K.train, best.lam)
attributes(h1)
h1$misclassification.rate # misclassification rate
cbind(train.sim$y, h1$y.pred)



## Predicting


sim.d.hat <- h1$d.hat

h2 <- pred.kernel(y.test, K.test, sim.d.hat)
attributes(h2)
h2$predict.new  
h2$misclassification.rate
cbind(test.sim$y, h2$predict.new)



