#### Making function of Kernel-trick regression and classification


### Simulation data

## Data for modeling
x1 <- runif(100,-3,3)
x2 <- runif(100,-3,3)
x3 <- runif(100,-3,3)
y <- 2*x1+1*x2^2+3*x3^3+rnorm(100,5,15)
x1 <- as.matrix(x1) ; x2 <- as.matrix(x2) ; x3 <- as.matrix(x3) ; y <- as.matrix(y)

dat1 <- cbind(y,x1,x2,x3)
colnames(dat1) <- c('y','x1','x2','x3')
dat1 <- as.data.frame(dat1)
dim(dat1) ; head(dat1)

## Data for testing
z1 <- runif(10,-3,3)
z2 <- runif(10,-3,3)
z3 <- runif(10,-3,3)
z1 <- as.matrix(z1) ; z2 <- as.matrix(z2) ; z3 <- as.matrix(z3)
dat.new <- cbind(z1,z2,z3)
colnames(dat.new) <- c('x1','x2','x3')
dat.new <- as.data.frame(dat.new)

### Leave-one-out cross-validation

g <- 10^seq(-10,10,length=200)
n <- nrow(dat1)
r <- length(g)

my.cross <- function(data,a,n,r,g) {
  
  lambda <- g
  cv.mse <- NULL
  U <- matrix(NA,r,2)
  colnames(U) <- c("lambda","Test MSE")
  
  cat("Leave-one-out crossvalidation is start...","\n")
  
  for (i in 1:r) {
    U[i,1] <- lambda[i]
  }
  
  for (j in 1:r) {
    for (k in 1:n) {
      data1 <- as.data.frame(data)
      test.data <- data1[k,]
      train.data <- data1[-k,]
      
      X.test <- as.matrix(test.data[,-a])
      X.train <- as.matrix(train.data[,-a]) # Independent variables
      y.test <- as.matrix(test.data[,a])
      y.train <- as.matrix(train.data[,a]) # Dependent variable
      
      ## Transformation of train data
      p <- ncol(X.train)
      sigma <- 1/p
      n.train <- nrow(X.train)
      K.train <- matrix(NA,n.train,n.train)
      D.train <- as.matrix(dist(X.train,method="euclidean",p=2)) # Euclidean distance 
      kernel <- function(m) exp(-sigma*m^2) # Gaussian kernel 
      
      for (s in 1:n.train) {
        for (t in 1:n.train) {
          K.train[s,t] <- kernel(D.train[s,t])
        }
      } # Making kernel matrix
      
      ## Transformation of test data
      n.test <- nrow(X.test)
      K.test <- matrix(NA,n.train,n.test)
      
      for (u in 1:n.train) {
        for (v in 1:n.test) {
          K.test[u,v] <- kernel(sqrt(sum((X.test[v,]-X.train[u,])^2)))
        }
      }
      
      ## Calculate MSE
      d.train <- solve(K.train+lambda[j]*diag(n.train))%*%y.train
      predict.test <- t(K.test)%*%d.train
      
      cv.mse[k] <- (y.test-predict.test)^2
      
      
    }
    U[j,2] <- sum(cv.mse)/n
    cat(j,",")
    
  }
  
  cat("\n","LOOCV complete...")
  
  U <- as.data.frame(U)
  cat("\n","When test MSE is ",min(U[,2]),", it is the best condition.","\n")
  plot(log(U[,1]),U[,2],main="Leave-one-out crossvalidation"
       ,xlab="log(lambda)",ylab="Test MSE",type="b")
  
  return(U)
}

(c <- my.cross(dat1,1,n,r,g))
attributes(c)



### Function 'my.kernel' for KtRC with Gaussian kernel

my.kernel <- function(data,a,l,dat.new) {
  
  if(l <= 0){
    cat("Lambda is non-positive value. Please insert positive value of lambda.")
  }
  
  else {
    data1 <- as.data.frame(data)
    X <- as.matrix(data1[,-a]) # Independent variables
    y <- as.matrix(data1[,a]) # Dependent variable
    
    p <- ncol(X)
    sigma <- 1/p
    n <- nrow(X)
    K <- matrix(NA,n,n)
    D <- as.matrix(dist(X,method="euclidean",p=2)) # Euclidean distance 
    kernel <- function(m) exp(-sigma*m^2) # Gaussian kernel 
    
    
    ## Making kernel matrix
    K <- kernel(D)
#    for (i in 1:n) {
#      for (j in 1:n) {
#        K[i,j] <- kernel(D[i,j])
#      }
#    } 
    
    ## Transformation of new data for predict
    dat.new1 <- as.data.frame(dat.new)
    X.new <- as.matrix(dat.new1)
    n.new <- nrow(X.new)
    K.new <- matrix(NA,n,n.new)
    
    for (i in 1:n) {
      for (j in 1:n.new) {
        K.new[i,j] <- kernel(sqrt(sum((X.new[j,]-X[i,])^2)))
      }
    }
    
    ## Predict
    lambda <- l
    d <- solve(K+lambda*diag(n))%*%y
    yhat <- K%*%d
    predict.new <- t(K.new)%*%d
    
    ## Drawing some pictures
    par(mfrow=c(1,1))
    
    for (i in 1:p) {
      plot(X[,i],y,main="Kernel-trick regression and classification",
           xlab="independent",ylab="dependent")
      # Kernel estimation
      for (j in 1:n) {
        points(X[j,i],yhat[j],col="red",pch=20)
      }     
      for (k in 1:n.new) {
        points(X.new[k,i],predict.new[k],col="blue",pch=20)
      }
      legend("topleft",c("Kernel estimation","Real data","Predict for new data"),
             col=c("red","black","blue"),pch=20)
    }
    
    
    ## Residual plot
    res <- y-yhat # Residual
    plot(y,res,xlab="Real data",ylab="Residual",main="Residual plot",pch=20,col="blue")
    abline(h=0,lty=2,col="black")
    std.res <- scale(y-yhat) # Standardized residual
    plot(y,std.res,xlab="Real data",ylab="Standardized residual",
         main="Standardized residual plot",pch=20,col="darkgreen")
    abline(h=0,lty=2,col="black")
    
    result <- list(dat=dat, lam=l, yhat=yhat, resid=res, stdres=std.res)
    
    #V <- cbind(yhat,res,std.res)
    #colnames(V) <- c("Predicted values of dependent variable","Residual", "Standardized residual")
    #V <- as.data.frame(V)
    
    
    ## Return some data for analysis
    
    cat("Predicted values of new data", "\n","\n")
    cat(predict.new,"\n")
    
    return(result)
    
  }
  
}


(h <- my.kernel(dat1,1,3.764936e-01,dat.new))
attributes(h)

### If insert value of lambda is non-positive, then function 'my.kernel' is not working.
my.kernel(dat1,1,-1,dat.new)

