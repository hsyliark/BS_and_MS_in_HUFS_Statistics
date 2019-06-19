#### Making function of Kernel-trick regression and classification


### Simulation data

## Data for modeling
x1 <- runif(200,-3,3)
x2 <- runif(200,-3,3)
x3 <- runif(200,-3,3)
y <- 2*x1+1*x2^2+3*x3^3+rnorm(200,5,15)
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
    for (i in 1:n) {
      for (j in 1:n) {
        K[i,j] <- kernel(D[i,j])
      }
    } 
    
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
    
    V <- cbind(yhat,res,std.res)
    colnames(V) <- c("Predicted values of dependent variable","Residual",
                     "Standardized residual")
    V <- as.data.frame(V)
    
    
    ## Return some data for analysis
    
    cat("Predicted values of new data", "\n","\n")
    cat(predict.new,"\n")
    
    return(V)
    
  }
  
}


(g <- my.kernel(dat1,1,0.03,dat.new))
attributes(g)

### If insert value of lambda is non-positive, then function 'my.kernel' is not working.
my.kernel(dat1,1,-1,dat.new)


### Insert some values
my.kernel(dat1,1,2,dat.new)
my.kernel(dat1,1,1.5,dat.new)
my.kernel(dat1,1,1.3,dat.new)
my.kernel(dat1,1,1,dat.new)
my.kernel(dat1,1,0.7,dat.new)
my.kernel(dat1,1,0.5,dat.new)
my.kernel(dat1,1,0.2,dat.new)
my.kernel(dat1,1,0.1,dat.new)
my.kernel(dat1,1,0.08,dat.new)
my.kernel(dat1,1,0.05,dat.new)
my.kernel(dat1,1,0.02,dat.new)
my.kernel(dat1,1,0.01,dat.new)
my.kernel(dat1,1,0.007,dat.new)
my.kernel(dat1,1,0.004,dat.new)
my.kernel(dat1,1,0.003,dat.new)
my.kernel(dat1,1,0.001,dat.new)

  
  
  
