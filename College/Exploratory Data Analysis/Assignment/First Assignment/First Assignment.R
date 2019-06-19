MyReg <- function(data) {
  n <- nrow(data) ; p <- ncol(data)-1
  y <- as.matrix(data)[,1] ; X <- cbind(rep(1,n),as.matrix(data)[,-1])
  beta <- (solve(t(X)%*%X))%*%(t(X))%*%(y)
  pred <- X%*%beta
  residuals <- y-pred ; sig2 <- (t(residuals)%*%residuals)/(n-p-1)
  H <- (X)%*%(solve(t(X)%*%X))%*%(t(X))
  stdresid <- rep(0,n)
  for(i in 1:n) {
    stdresid[i] <- residuals[i]/sqrt(sig2*(1-H[i,i]))
  }
  SST <- sum((y-mean(y))^2) ; SSE <- sum((y-pred)^2) ; SSR <- SST - SSE
  MSR <- SSR/(p) ; MSE <- SSE/(n-p-1) ; F <- MSR/MSE
  P.value <- 1-pf(F,p,n-p-1) ; Rsq <- SSR/SST
  C <- solve(t(X)%*%X)
  std.error <- rep(0,p+1)
  for(i in 1:(p+1)) {
    std.error[i] <- sqrt(sig2*C[i,i])
  }
  t.value <- rep(0,p+1)
  for(i in 1:(p+1)) {
    t.value[i] <- beta[i]/std.error[i]
  }
  Pr.t <- rep(0,(p+1))
  for(i in 1:(p+1)) {
    Pr.t[i] <- 2*(1-pt(abs(t.value[i]), n-p-1))
  }
  args <- list(Data=data, Beta=beta, Sigma2=sig2, Predict=pred,
               Residuals=residuals, Standard.Residuals=stdresid,
               SSR=SSR, SSE=SSE, F.Statistic=F, P.value=P.value)
  
  aster <- function(p) {
    if (p >= 0.05 & p < 0.1) "."
    else if (p >= 0.01 & p < 0.05) "*"
    else if (p >= 0.001 & p < 0.1) "**"
    else if (p < 0.001) "***"
    else " "
  }
  
  cat("\n == ANALYSIS OF VARIANCE == \n\n", 
      encodeString(c("Source", "   df", "   SS", "   MS", "   F", "   P-value"), 
                   width=8, justify="right"), "\n", 
      "------------------------------------------------------------------", 
      "\n", encodeString(c("Regression", p, round(SSR,2), round(MSR,2), 
                           round(F,2), round(P.value,2)), width=8, justify="right"), 
      aster(P.value), "\n", encodeString(c("     Error", n-p-1, round(SSE,2), 
                                           round(MSE,2)), width=8, justify="right"), 
      "\n", encodeString(c("     Total", n-1, round(SST,2)), 
                         width=8, justify="right"), "\n", 
      "------------------------------------------------------------------", 
      "\n", "Estimated error variance :", round(sig2,4), "\n","R-squares :",
      round(Rsq,4), "\n\n")
  
  ind.name <- colnames(data)
  ind.name[1] <- "(Intercept)"
  test.mat <- cbind(ind.name, round(beta,4), round(std.error,4), round(t.value,4), 
                    round(Pr.t,4))
  cat("\n\n\n== PARAMETER ESTIMATES ==\n\n", 
      encodeString(c(" ", "Estimate", "Std.Error", "t value", "Pr(>|t|)"), 
                   width=11, justify="right"), "\n")
  for(i in 1:nrow(test.mat)) {
    cat(encodeString(test.mat[i,], width=11, justify="right"), 
        aster(Pr.t[i]),"\n")
  }
  
  cat("\n", "===Various Statistics in Multiple Regression===", "\n\n")
  print(args)
}

N <- 100
set.seed(1234)
tX1 <- rnorm(N,0,1)
tX2 <- rnorm(N,3,1)
tX3 <- rgamma(N,1,3)
tX4 <- sample(c(0,1),N,replace=T)
tX5 <- sample(1:3,N,replace=T,prob=c(1,2,3)/6)
tX6 <- rbinom(N,4,0.3)
tX <- cbind(rep(1,length(tX1)),tX1,tX2,tX3,tX4,tX5,tX6)
tbeta <- (0:6)/5
ty <- as.vector(tX%*%tbeta)+rnorm(N,0,2)
dat <- data.frame(y=ty, age=tX1, height=tX2, weight=tX3, smoking=tX4, 
                  therapy=tX5, surgery=tX6)
res1 <- MyReg(dat)
str(res1)
attributes(res1)

crime <- read.csv("D:/수업자료/대학/(4)Senior/4-2/탐색적자료분석및상담(1전공)/
                  Assignment/crime.csv",sep=",",header=T)
crime1 <- crime[,-1]
res2 <- MyReg(crime1)
str(res2)
attributes(res2)