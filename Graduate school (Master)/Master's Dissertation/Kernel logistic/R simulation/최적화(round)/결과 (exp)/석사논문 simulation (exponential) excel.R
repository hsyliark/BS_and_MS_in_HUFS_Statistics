#### Simulation data



dat.gen <- function(n, p, seed=NULL, plot=FALSE){
  if(!is.null(seed)) set.seed(seed)
  x <- matrix( rnorm( n*p, mean=0 , sd=10 ), nrow=n , ncol=p )
  prob <- exp( - rowSums(x[,1:round(p/2)]^2)/((p/2)*(10^2)) )
  y <- apply(as.matrix(prob), c(1,2), rbinom, n=1, size=1)
  dat <- cbind( y , x )
  colnames( dat ) <- c( "y" , paste0("x", 1:p) )
  if( plot == TRUE ){
    pchs <- ifelse( y==0 , 21 , 19 )
    plot( x[,1] , x[,2] , pch=pchs , xlim=c(-22,22) , ylim=c(-22,22) )
    grid <- seq( -2*pi , 2*pi , length=100 )
    lines( 10*cos(grid) , 10*sin(grid) , lty=3 , lwd=2 )
  }
  return( as.data.frame(dat) )
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
  K <- exp( -sigma * D^2 )  # Gaussian kernel
  K.train <- K[1:n,1:n]
  K.test <- K[1:n,(n+1):n1]
  
  return(list(K.train=K.train, K.test=K.test, K=K, y.train=y.train, y.test=y.test))
  
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
  
  d.old <- rep(0,ncol(K)) # The initial value of coefficient vector
  
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
    
    
    # Convergence criterion using Log-likelihood function
    diff <- abs(sum(y*new.logit.hat - log(1+exp(new.logit.hat))) - sum(y*old.logit.hat - log(1+exp(old.logit.hat))))
    
    cat("( iteration , difference ) = (", i, ",", diff, ")\n")
    
    if (diff < 1E-4) break
    
    d.old <- d.new
    
  }
  
  cat("Algorithm converged...","\n\n")
  
  logit.hat <- K%*%d.new
  pi.hat <- exp(logit.hat)/(1+exp(logit.hat))
  y.pred <- rep(1,length(logit.hat))
  y.pred[pi.hat < 0.5] <- 0
  
  return(list(d.hat=d.new, y.pred=y.pred, pi.hat=pi.hat))
  
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
  pi.hat <- g$pi.hat
  
  rate.miss <- mean(y.pred != y.train)
  
  
  return(list(d.hat=d.hat, y.train=y.train, y.pred=y.pred, 
              misclassification.rate=rate.miss, pi.hat=pi.hat))
  
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
  
  
  return(list(y.test=y.test, predict.new=predict.new, logit.hat=logit.hat,
              misclassification.rate=rate.miss, pi.hat.new=pi.hat))
  
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
  
  mean.logL <- rowMeans(cv.logL)
  idx <- which.min(mean.logL)
  
  # plot(log(grid.l, base=10),rowMeans(cv.logL),xlab="log10(lambda)",ylab="Minus log-likelihood"
  #      ,main="K-fold crossvalidation",type="b")
  # abline(v=log(lambda[ mean.logL == mean.logL[idx] ], base=10), col="blue", lty=2)
  
  return(list(lambda=grid.l, cv.logL=cv.logL))
  
  
}





#------------------------#------------------------#------------------------#------------------------
#------------------------#------------------------#------------------------#------------------------




#### 8 method simulation





fit.ftn <- function(n.train, p) {
  
  
  for (i in 1:100) {
    
    
    #### Test Misclassification Rate
    
    test.MR <- c(rep(0,8))
    
    
    #### Making simulation data
    
    train.sim <- dat.gen(n.train, p, seed=i)
    test.sim <- dat.gen(1000, p, seed=i)
    
    n.train <- n.train
    p <- p
    
    
    ### 1. Kernel ridge logistic regression classification (KRLC)
    
     
      ## 5-fold crossvalidation
      
      u <- my.kernel.matrix(train.sim, test.sim)
      K.train <- u$K.train ; K.test <- u$K.test  
      y.train <- u$y.train ; y.test <- u$y.test
      
      grid.l <- 10^seq(-3,2,length=10)
      
      h <- cv.kernel(y.train, K.train, 5, grid.l) 
      
      ## Fitting 
      
      mean.logL <- rowMeans(h$cv.logL)
      idx <- which.min(mean.logL)
      best.lam <- max(h$lambda[ mean.logL == mean.logL[idx] ])
      
      h1 <- fit.kernel(y.train, K.train, best.lam)
      
      ## Calculate test misclassification rate
      
      sim.d.hat <- h1$d.hat
      
      h2 <- pred.kernel(y.test, K.test, sim.d.hat)
      test.MR[1] <- h2$misclassification.rate # KRLC
      

    
    
    ### 2. Kernel ridge logistic regression classification using Sub-sampling (KRLCS)
    
    
      
      u <- my.kernel.matrix(train.sim, test.sim)
      K.train <- u$K.train ; y.train <- u$y.train 
      K <- u$K ; y.test <- u$y.test
      
    
      ## Choosing optimal training data and lambda
            
      n <- nrow(train.sim) ; n1 <- sum(nrow(train.sim), nrow(test.sim))
      res.rate.c <- NULL ; res.lam.c <- NULL ; res.index.c <- NULL
      
      for (j in 1:500) {
        
        n <- nrow(train.sim)
        index.Kc <- sample(1:n, round(0.7*n), replace=F)
        Kc.train <- K.train[index.Kc,index.Kc] ; Kc.test <- K.train[index.Kc,-index.Kc]
        yc.train <- y.train[index.Kc] ; yc.test <- y.train[-index.Kc]
        
        grid.l <- 10^seq(-3,2,length=10)
        
        hc <- cv.kernel(yc.train, Kc.train, 5, grid.l) # 5-fold crossvalidation 
        
        mean.logL.c <- rowMeans(hc$cv.logL)
        idx.c <- which.min(mean.logL.c)
        best.lam.c <- max(hc$lambda[ mean.logL.c == mean.logL.c[idx.c] ])
        
        h1.c <- fit.kernel(yc.train, Kc.train, best.lam.c) # Fitting
        
        sim.d.hat.c <- h1.c$d.hat
        
        h2.c <- pred.kernel(yc.test, Kc.test, sim.d.hat.c) # Testing
        rate.miss.c <- h2.c$misclassification.rate
        
        res.rate.c <- c(res.rate.c, rate.miss.c)
        res.lam.c <- c(res.lam.c, best.lam.c)
        res.index.c <- cbind(res.index.c, index.Kc)
        
      }
      
      ## Fitting  
      
      res.lambda <- res.lam.c[which.min(res.rate.c)]
      res.index <- res.index.c[, which.min(res.rate.c)]
      res.K.train <- K[res.index, res.index]
      res.y.train <- y.train[res.index]
      K.test <- K[res.index, (n+1):n1]
      
      h1 <- fit.kernel(res.y.train, res.K.train, res.lambda)
      
      res.d.hat <- h1$d.hat
      
      ## Calculate test misclassification rate
      
      h2 <- pred.kernel(y.test, K.test, res.d.hat)
      test.MR[2] <- h2$misclassification.rate # KRLCS

    
   
    ### 3. Kernel ridge logistic regression classification using Bagging (KRLCB1, KRLCB2, KRLCB3)
    
      
      test.sim.y <- test.sim[,1] 
      
      u <- my.kernel.matrix(train.sim, test.sim)
      K.train <- u$K.train 
      y.train <- u$y.train ; y.test <- u$y.test 
      K <- u$K
      n <- nrow(train.sim)
      n1 <- sum(nrow(train.sim), nrow(test.sim))
      
      
      ## Choosing best lambda
      
      res.rate.miss <- NULL
      grid.l <- 10^seq(-3,2,length=10)
      
      for (s in 1:50) {
        
        boot.index <- sample(1:n, n, replace=T)
        boot.K.train <- K.train[boot.index, boot.index]
        boot.K.test <- K.train[boot.index, -boot.index]
        boot.y.train <- y.train[boot.index]
        boot.y.test <- y.train[-boot.index]
        
        boot.rate.miss <- c(rep(0,10))
        
        for (j in 1:10) {
          
          h1.boot <- fit.kernel(boot.y.train, boot.K.train, grid.l[j])
          boot.d.hat <- h1.boot$d.hat
          h2.boot <- pred.kernel(boot.y.test, boot.K.test, boot.d.hat)
          boot.rate.miss[j] <- h2.boot$misclassification.rate
          
        }
        
        res.rate.miss <- rbind(res.rate.miss, boot.rate.miss)
        
      }
      
      best.lambda <- max(grid.l[colMeans(res.rate.miss) == min(colMeans(res.rate.miss))])
      
      ## Bootstraping
      
      boots.logit <- NULL
      boots.pi <- NULL
      boots.predict.new <- NULL
      
      for (r in 1:500) {
        
        boots.index <- sample(1:n, n, replace=T)
        boots.K.train <- K[boots.index, boots.index]
        boots.K.test <- K[boots.index, (n+1):n1]
        boots.y.train <- y.train[boots.index]
        boots.y.test <- y.test 
        
        h1.boots <- fit.kernel(boots.y.train, boots.K.train, best.lambda)
        boots.d.hat <- h1.boots$d.hat
        h2.boots <- pred.kernel(boots.y.test, boots.K.test, boots.d.hat)
        
        boots.logit.hat <- h2.boots$logit.hat
        boots.logit <- cbind(boots.logit, boots.logit.hat)
        
        boots.pi.hat <- h2.boots$pi.hat.new
        boots.pi <- cbind(boots.pi, boots.pi.hat)
        
        boots.predict <- h2.boots$predict.new 
        boots.predict.new <- rbind(boots.predict.new, boots.predict)
        
      }
      
      ## Bagging estimate with logit estimate
      
      logit.hat.bag <- rowMeans(boots.logit)
      pi.hat.bag1 <- exp(logit.hat.bag)/(1+exp(logit.hat.bag))
      
      y.hat.bag1 <- c(rep(1,n1-n))
      y.hat.bag1[pi.hat.bag1 < 0.5] <- 0
      y.hat.bag1[pi.hat.bag1 == 0.5] <- rbinom(length(y.hat.bag1[pi.hat.bag1 == 0.5]), 1, 0.5)
      
      test.MR[3] <- mean(y.hat.bag1 != y.test) # KRLCB1
      
      ## Bagging estimate with probability estimate
      
      pi.hat.bag2 <- rowMeans(boots.pi)
      
      y.hat.bag2 <- c(rep(1,n1-n))
      y.hat.bag2[pi.hat.bag2 < 0.5] <- 0
      y.hat.bag2[pi.hat.bag2 == 0.5] <- rbinom(length(y.hat.bag2[pi.hat.bag2 == 0.5]), 1, 0.5)
      
      test.MR[4] <- mean(y.hat.bag2 != y.test) # KRLCB2
      
      ## Bagging estimate with majority vote
      
      mean.hat.bag <- apply(boots.predict.new, 2, mean)
      y.hat.bag3 <- c(rep(1,n1-n))
      y.hat.bag3[mean.hat.bag < 0.5] <- 0
      y.hat.bag3[mean.hat.bag == 0.5] <- rbinom(length(y.hat.bag3[mean.hat.bag == 0.5]), 1, 0.5)
      
      test.MR[5] <- mean(y.hat.bag3 != y.test) # KRLCB3
      
    
        
    ### 4. Kernel ridge logistic regression classification using Random Forest (KRLCR1, KRLCR2, KRLCR3)   
      
      
      train.sim.y <- train.sim[,1] ; test.sim.y <- test.sim[,1]
      train.sim.X <- train.sim[,-1] ; test.sim.X <- test.sim[,-1]
      
      n <- nrow(train.sim)
      n1 <- sum(nrow(train.sim), nrow(test.sim))
      
      
      ## Choosing best lambda
      
      res.rate.miss <- NULL
      grid.l <- 10^seq(-3,2,length=10)
      
      for (s in 1:50) {
        
        rf.index <- sample(1:p, round(sqrt(p)), replace=F)
        boot.index <- sample(1:n, n, replace=T)
        train.sim1 <- cbind(train.sim.y[boot.index], train.sim.X[boot.index, rf.index])
        test.sim1 <- cbind(train.sim.y[-boot.index], train.sim.X[-boot.index, rf.index])
        
        u1 <- my.kernel.matrix(train.sim1, test.sim1)
        boot.K.train <- u1$K.train ; boot.K.test <- u1$K.test
        boot.y.train <- u1$y.train ; boot.y.test <- u1$y.test
        
        boot.rate.miss <- c(rep(0,10))
        
        for (j in 1:10) {
          
          h1.boot <- fit.kernel(boot.y.train, boot.K.train, grid.l[j])
          boot.d.hat <- h1.boot$d.hat
          h2.boot <- pred.kernel(boot.y.test, boot.K.test, boot.d.hat)
          boot.rate.miss[j] <- h2.boot$misclassification.rate
          
        }
        
        res.rate.miss <- rbind(res.rate.miss, boot.rate.miss)
        
      }
      
      best.lambda <- max(grid.l[colMeans(res.rate.miss)==min(colMeans(res.rate.miss))])
      
      ## Bootstraping
      
      boots.logit <- NULL
      boots.pi <- NULL
      boots.predict.new <- NULL
      
      for (r in 1:500) {
        
        rfs.index <- sample(1:p, round(sqrt(p)), replace=F)
        boots.index <- sample(1:n, n, replace=T) 
        train.sim2 <- cbind(train.sim.y[boots.index], train.sim.X[boots.index, rfs.index])
        test.sim2 <- cbind(test.sim.y, test.sim.X[, rfs.index])
        
        u2 <- my.kernel.matrix(train.sim2, test.sim2)
        boots.K.train <- u2$K.train ; boots.K.test <- u2$K.test
        boots.y.train <- u2$y.train ; boots.y.test <- u2$y.test
        
        h1.boots <- fit.kernel(boots.y.train, boots.K.train, best.lambda)
        boots.d.hat <- h1.boots$d.hat
        h2.boots <- pred.kernel(boots.y.test, boots.K.test, boots.d.hat)
        
        boots.logit.hat <- h2.boots$logit.hat
        boots.logit <- cbind(boots.logit, boots.logit.hat)
        
        boots.pi.hat <- h2.boots$pi.hat.new
        boots.pi <- cbind(boots.pi, boots.pi.hat)
        
        boots.predict <- h2.boots$predict.new
        boots.predict.new <- rbind(boots.predict.new, boots.predict)
        
      }
      
      ## Random Forest estimate with logit estimate
      
      logit.hat.rf <- rowMeans(boots.logit)
      pi.hat.rf1 <- exp(logit.hat.rf)/(1+exp(logit.hat.rf))
      
      y.hat.rf1 <- c(rep(1,n1-n))
      y.hat.rf1[pi.hat.rf1 < 0.5] <- 0
      y.hat.rf1[pi.hat.rf1 == 0.5] <- rbinom(length(y.hat.rf1[pi.hat.rf1 == 0.5]), 1, 0.5)
      
      test.MR[6] <- mean(y.hat.rf1 != test.sim.y) # KRLCR1
      
      ## Random Forest estimate with probability estimate
      
      pi.hat.rf2 <- rowMeans(boots.pi)
      
      y.hat.rf2 <- c(rep(1,n1-n))
      y.hat.rf2[pi.hat.rf2 < 0.5] <- 0
      y.hat.rf2[pi.hat.rf2 == 0.5] <- rbinom(length(y.hat.rf2[pi.hat.rf2 == 0.5]), 1, 0.5)
      
      test.MR[7] <- mean(y.hat.rf2 != test.sim.y) # KRLCR2
      
      ## Random Forest estimate with majority vote
      
      mean.hat.rf <- apply(boots.predict.new, 2, mean)
      y.hat.rf3 <- c(rep(1,n1-n))
      y.hat.rf3[mean.hat.rf < 0.5] <- 0
      y.hat.rf3[mean.hat.rf == 0.5] <- rbinom(length(y.hat.rf3[mean.hat.rf == 0.5]), 1, 0.5)
      
      test.MR[8] <- mean(y.hat.rf3 != test.sim.y) # KRLCR3
      
    
   ## Writing text file for analysis
   
   write( test.MR , file=paste0("Simulation_n=", n.train, "_", "p=", p, ".txt") , append= TRUE, ncolumns=8 )
   
  }
    
}







#### Making data for analysis




method <- c(rep("m1(KRLC)",100), rep("m2(KRLCS)",100), rep("m3(KRLCB1)",100), rep("m4(KRLCB2)",100), 
            rep("m5(KRLCB3)",100), rep("m6(KRLCR1)",100), rep("m7(KRLCR2)",100), rep("m8(KRLCR3)",100))
number.50 <- c(rep("1(50)",800))
number.100 <- c(rep("2(100)",800))
number.200 <- c(rep("3(200)",800))
number.400 <- c(rep("4(400)",800))









### Final result (p=3)

fit.ftn(50, 3) # Choose when n.train=50
dat10 <- read.table("C:/Users/user2/Desktop/석사논문 모의실험/Simulation_n=50_p=3.txt", sep=" ")
dat10 <- as.numeric(c(dat10[,1], dat10[,2], dat10[,3], dat10[,4], dat10[,5], dat10[,6], dat10[,7], dat10[,8]))
dat10 <- as.data.frame(list(test.MR=dat10, method=method, number=number.50))

fit.ftn(100, 3) # Choose when n.train=100
dat20 <- read.table("C:/Users/user2/Desktop/석사논문 모의실험/Simulation_n=100_p=3.txt", sep=" ")
dat20 <- as.numeric(c(dat20[,1], dat20[,2], dat20[,3], dat20[,4], dat20[,5], dat20[,6], dat20[,7], dat20[,8]))
dat20 <- as.data.frame(list(test.MR=dat20, method=method, number=number.100))

fit.ftn(200, 3) # Choose when n.train=200
dat30 <- read.table("C:/Users/user2/Desktop/석사논문 모의실험/Simulation_n=200_p=3.txt", sep=" ")
dat30 <- as.numeric(c(dat30[,1], dat30[,2], dat30[,3], dat30[,4], dat30[,5], dat30[,6], dat30[,7], dat30[,8]))
dat30 <- as.data.frame(list(test.MR=dat30, method=method, number=number.200))

fit.ftn(400, 3) # Choose when n.train=400
dat40 <- read.table("C:/Users/user2/Desktop/석사논문 모의실험/Simulation_n=400_p=3.txt", sep=" ")
dat40 <- as.numeric(c(dat40[,1], dat40[,2], dat40[,3], dat40[,4], dat40[,5], dat40[,6], dat40[,7], dat40[,8]))
dat40 <- as.data.frame(list(test.MR=dat40, method=method, number=number.400))

final.dat3 <- rbind(dat10, dat20, dat30, dat40)






### Final result (p=5)

fit.ftn(50, 5) # Choose when n.train=50
dat_1 <- read.table("C:/Users/user2/Desktop/석사논문 모의실험/Simulation_n=50_p=5.txt", sep=" ")
dat_1 <- as.numeric(c(dat_1[,1], dat_1[,2], dat_1[,3], dat_1[,4], dat_1[,5], dat_1[,6], dat_1[,7], dat_1[,8]))
dat_1 <- as.data.frame(list(test.MR=dat_1, method=method, number=number.50))

fit.ftn(100, 5) # Choose when n.train=100
dat_2 <- read.table("C:/Users/user2/Desktop/석사논문 모의실험/Simulation_n=100_p=5.txt", sep=" ")
dat_2 <- as.numeric(c(dat_2[,1], dat_2[,2], dat_2[,3], dat_2[,4], dat_2[,5], dat_2[,6], dat_2[,7], dat_2[,8]))
dat_2 <- as.data.frame(list(test.MR=dat_2, method=method, number=number.100))

fit.ftn(200, 5) # Choose when n.train=200
dat_3 <- read.table("C:/Users/user2/Desktop/석사논문 모의실험/Simulation_n=200_p=5.txt", sep=" ")
dat_3 <- as.numeric(c(dat_3[,1], dat_3[,2], dat_3[,3], dat_3[,4], dat_3[,5], dat_3[,6], dat_3[,7], dat_3[,8]))
dat_3 <- as.data.frame(list(test.MR=dat_3, method=method, number=number.200))

fit.ftn(400, 5) # Choose when n.train=400
dat_4 <- read.table("C:/Users/user2/Desktop/석사논문 모의실험/Simulation_n=400_p=5.txt", sep=" ")
dat_4 <- as.numeric(c(dat_4[,1], dat_4[,2], dat_4[,3], dat_4[,4], dat_4[,5], dat_4[,6], dat_4[,7], dat_4[,8]))
dat_4 <- as.data.frame(list(test.MR=dat_4, method=method, number=number.400))

final.dat5 <- rbind(dat_1, dat_2, dat_3, dat_4)





### Final result (p=10)

fit.ftn(50, 10) # Choose when n.train=50
dat1.1 <- read.table("C:/Users/user2/Desktop/석사논문 모의실험/Simulation_n=50_p=10.txt", sep=" ")
dat1.1 <- as.numeric(c(dat1.1[,1], dat1.1[,2], dat1.1[,3], dat1.1[,4], dat1.1[,5], dat1.1[,6], dat1.1[,7], dat1.1[,8]))
dat1.1 <- as.data.frame(list(test.MR=dat1.1, method=method, number=number.50))

fit.ftn(100, 10) # Choose when n.train=100
dat1.2 <- read.table("C:/Users/user2/Desktop/석사논문 모의실험/Simulation_n=100_p=10.txt", sep=" ")
dat1.2 <- as.numeric(c(dat1.2[,1], dat1.2[,2], dat1.2[,3], dat1.2[,4], dat1.2[,5], dat1.2[,6], dat1.2[,7], dat1.2[,8]))
dat1.2 <- as.data.frame(list(test.MR=dat1.2, method=method, number=number.100))

fit.ftn(200, 10) # Choose when n.train=200
dat1.3 <- read.table("C:/Users/user2/Desktop/석사논문 모의실험/Simulation_n=200_p=10.txt", sep=" ")
dat1.3 <- as.numeric(c(dat1.3[,1], dat1.3[,2], dat1.3[,3], dat1.3[,4], dat1.3[,5], dat1.3[,6], dat1.3[,7], dat1.3[,8]))
dat1.3 <- as.data.frame(list(test.MR=dat1.3, method=method, number=number.200))

fit.ftn(400, 10) # Choose when n.train=400
dat1.4 <- read.table("C:/Users/user2/Desktop/석사논문 모의실험/Simulation_n=400_p=10.txt", sep=" ")
dat1.4 <- as.numeric(c(dat1.4[,1], dat1.4[,2], dat1.4[,3], dat1.4[,4], dat1.4[,5], dat1.4[,6], dat1.4[,7], dat1.4[,8]))
dat1.4 <- as.data.frame(list(test.MR=dat1.4, method=method, number=number.400))

final.dat10 <- rbind(dat1.1, dat1.2, dat1.3, dat1.4)





### Final result (p=20)

fit.ftn(50, 20) # Choose when n.train=50
dat2.1 <- read.table("C:/Users/user2/Desktop/석사논문 모의실험/Simulation_n=50_p=20.txt", sep=" ")
dat2.1 <- as.numeric(c(dat2.1[,1], dat2.1[,2], dat2.1[,3], dat2.1[,4], dat2.1[,5], dat2.1[,6], dat2.1[,7], dat2.1[,8]))
dat2.1 <- as.data.frame(list(test.MR=dat2.1, method=method, number=number.50))

fit.ftn(100, 20) # Choose when n.train=100
dat2.2 <- read.table("C:/Users/user2/Desktop/석사논문 모의실험/Simulation_n=100_p=20.txt", sep=" ")
dat2.2 <- as.numeric(c(dat2.2[,1], dat2.2[,2], dat2.2[,3], dat2.2[,4], dat2.2[,5], dat2.2[,6], dat2.2[,7], dat2.2[,8]))
dat2.2 <- as.data.frame(list(test.MR=dat2.2, method=method, number=number.100))

fit.ftn(200, 20) # Choose when n.train=200
dat2.3 <- read.table("C:/Users/user2/Desktop/석사논문 모의실험/Simulation_n=200_p=20.txt", sep=" ")
dat2.3 <- as.numeric(c(dat2.3[,1], dat2.3[,2], dat2.3[,3], dat2.3[,4], dat2.3[,5], dat2.3[,6], dat2.3[,7], dat2.3[,8]))
dat2.3 <- as.data.frame(list(test.MR=dat2.3, method=method, number=number.200))

fit.ftn(400, 20) # Choose when n.train=400
dat2.4 <- read.table("C:/Users/user2/Desktop/석사논문 모의실험/Simulation_n=400_p=20.txt", sep=" ")
dat2.4 <- as.numeric(c(dat2.4[,1], dat2.4[,2], dat2.4[,3], dat2.4[,4], dat2.4[,5], dat2.4[,6], dat2.4[,7], dat2.4[,8]))
dat2.4 <- as.data.frame(list(test.MR=dat2.4, method=method, number=number.400))

final.dat20 <- rbind(dat2.1, dat2.2, dat2.3, dat2.4)





### Final result (p=40)

fit.ftn(50, 40) # Choose when n.train=50
dat3.1 <- read.table("C:/Users/user2/Desktop/석사논문 모의실험/Simulation_n=50_p=40.txt", sep=" ")
dat3.1 <- as.numeric(c(dat3.1[,1], dat3.1[,2], dat3.1[,3], dat3.1[,4], dat3.1[,5], dat3.1[,6], dat3.1[,7], dat3.1[,8]))
dat3.1 <- as.data.frame(list(test.MR=dat3.1, method=method, number=number.50))

fit.ftn(100, 40) # Choose when n.train=100
dat3.2 <- read.table("C:/Users/user2/Desktop/석사논문 모의실험/Simulation_n=100_p=40.txt", sep=" ")
dat3.2 <- as.numeric(c(dat3.2[,1], dat3.2[,2], dat3.2[,3], dat3.2[,4], dat3.2[,5], dat3.2[,6], dat3.2[,7], dat3.2[,8]))
dat3.2 <- as.data.frame(list(test.MR=dat3.2, method=method, number=number.100))

fit.ftn(200, 40) # Choose when n.train=200
dat3.3 <- read.table("C:/Users/user2/Desktop/석사논문 모의실험/Simulation_n=200_p=40.txt", sep=" ")
dat3.3 <- as.numeric(c(dat3.3[,1], dat3.3[,2], dat3.3[,3], dat3.3[,4], dat3.3[,5], dat3.3[,6], dat3.3[,7], dat3.3[,8]))
dat3.3 <- as.data.frame(list(test.MR=dat3.3, method=method, number=number.200))

fit.ftn(400, 40) # Choose when n.train=400
dat3.4 <- read.table("C:/Users/user2/Desktop/석사논문 모의실험/Simulation_n=400_p=40.txt", sep=" ")
dat3.4 <- as.numeric(c(dat3.4[,1], dat3.4[,2], dat3.4[,3], dat3.4[,4], dat3.4[,5], dat3.4[,6], dat3.4[,7], dat3.4[,8]))
dat3.4 <- as.data.frame(list(test.MR=dat3.4, method=method, number=number.400))

final.dat40 <- rbind(dat3.1, dat3.2, dat3.3, dat3.4)





### Final result (p=80)

fit.ftn(50, 80) # Choose when n.train=50
dat4.1 <- read.table("C:/Users/user2/Desktop/석사논문 모의실험/Simulation_n=50_p=80.txt", sep=" ")
dat4.1 <- as.numeric(c(dat4.1[,1], dat4.1[,2], dat4.1[,3], dat4.1[,4], dat4.1[,5], dat4.1[,6], dat4.1[,7], dat4.1[,8]))
dat4.1 <- as.data.frame(list(test.MR=dat4.1, method=method, number=number.50))

fit.ftn(100, 80) # Choose when n.train=100
dat4.2 <- read.table("C:/Users/user2/Desktop/석사논문 모의실험/Simulation_n=100_p=80.txt", sep=" ")
dat4.2 <- as.numeric(c(dat4.2[,1], dat4.2[,2], dat4.2[,3], dat4.2[,4], dat4.2[,5], dat4.2[,6], dat4.2[,7], dat4.2[,8]))
dat4.2 <- as.data.frame(list(test.MR=dat4.2, method=method, number=number.100))

fit.ftn(200, 80) # Choose when n.train=200
dat4.3 <- read.table("C:/Users/user2/Desktop/석사논문 모의실험/Simulation_n=200_p=80.txt", sep=" ")
dat4.3 <- as.numeric(c(dat4.3[,1], dat4.3[,2], dat4.3[,3], dat4.3[,4], dat4.3[,5], dat4.3[,6], dat4.3[,7], dat4.3[,8]))
dat4.3 <- as.data.frame(list(test.MR=dat4.3, method=method, number=number.200))

fit.ftn(400, 80) # Choose when n.train=400
dat4.4 <- read.table("C:/Users/user2/Desktop/석사논문 모의실험/Simulation_n=400_p=80.txt", sep=" ")
dat4.4 <- as.numeric(c(dat4.4[,1], dat4.4[,2], dat4.4[,3], dat4.4[,4], dat4.4[,5], dat4.4[,6], dat4.4[,7], dat4.4[,8]))
dat4.4 <- as.data.frame(list(test.MR=dat4.4, method=method, number=number.400))

final.dat80 <- rbind(dat4.1, dat4.2, dat4.3, dat4.4)






#### Drawing boxplot


install.packages("ggplot2")
library(ggplot2)



ggplot(final.dat3, aes(x = number, y = test.MR, fill = number)) + geom_boxplot() + facet_wrap(~ method, ncol=8)

ggplot(final.dat5, aes(x = number, y = test.MR, fill = number)) + geom_boxplot() + facet_wrap(~ method, ncol=8)

ggplot(final.dat10, aes(x = number, y = test.MR, fill = number)) + geom_boxplot() + facet_wrap(~ method, ncol=8)

ggplot(final.dat20, aes(x = number, y = test.MR, fill = number)) + geom_boxplot() + facet_wrap(~ method, ncol=8)

ggplot(final.dat40, aes(x = number, y = test.MR, fill = number)) + geom_boxplot() + facet_wrap(~ method, ncol=8)

ggplot(final.dat80, aes(x = number, y = test.MR, fill = number)) + geom_boxplot() + facet_wrap(~ method, ncol=8)
