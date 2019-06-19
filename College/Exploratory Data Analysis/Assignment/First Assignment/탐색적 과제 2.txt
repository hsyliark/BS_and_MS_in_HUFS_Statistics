## Case 1 ##

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
               Residuals=residuals, Standard.Residuals=stdresid, SSR=SSR,
               SSE=SSE, F.Statistic=F, P.value=P.value)
  
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
      "------------------------------------------------------------------", "\n",
      encodeString(c("Regression", p, round(SSR,2), round(MSR,2), round(F,2), 
                     round(P.value,2)), width=8, justify="right"),
      aster(P.value), "\n", encodeString(c("     Error", n-p-1, round(SSE,2), 
                                           round(MSE,2)), width=8, justify="right"),
      "\n", encodeString(c("     Total", n-1, round(SST,2)), width=8, 
                         justify="right"), 
      "\n",
      "------------------------------------------------------------------", "\n",
      "Estimated error variance :", round(sig2,4), "\n","R-squares :",round(Rsq,4), 
      "\n\n")
  
  ind.name <- colnames(data)
  ind.name[1] <- "(Intercept)"
  test.mat <- cbind(ind.name, round(beta,4), round(std.error,4), round(t.value,4), 
                    round(Pr.t,4))
  cat("\n\n\n== PARAMETER ESTIMATES ==\n\n",
      encodeString(c(" ", "Estimate", "Std.Error", "t value", "Pr(>|t|)"), width=11, 
                   justify="right"),
      "\n")
  for(i in 1:nrow(test.mat)) {
    cat(encodeString(test.mat[i,], width=11, justify="right"), aster(Pr.t[i]),"\n")
  }
  
  cat("\n", "===Various Statistics in Multiple Regression===", "\n\n")
  print(args) 
  
  outlier1 <- rep(0,n)
  outlier2 <- rep(0,n)
  std.sign <- rep(0,n)
  for(i in 1:n) {
    outlier1[i] <- ifelse(abs(stdresid[i]) > 2, "out", "in")
    outlier2[i] <- ifelse(abs(stdresid[i]) > 2, 2, 1)
    std.sign[i] <- ifelse(stdresid[i] > 0, 24, 20)
  }
  abs.stdresid <- abs(stdresid)
  ex1 <- as.data.frame(cbind(number=(1:n), stdresid=stdresid,
                             pred=pred, y=y, 
                             outlier1=outlier1, outlier2=outlier2,
                             std.sign=std.sign, abs.stdresid=abs.stdresid))
  ex1_1 <- as.matrix(ex1[,1:4,8])
  ex1_1 <- as.numeric(ex1_1)
  ex1_1 <- matrix(ex1_1,nrow=n,ncol=5)
  colnames(ex1_1) <- c("number","stdresid","pred","y","abs.stdresid")
  ex2 <- as.data.frame(ex1_1)
  ex2 <- ex2[abs.stdresid > 2,]
  
  pred1 <- as.vector(pred) ; abs.stdresid1 <- as.vector(abs.stdresid)
  ord <- order(pred1,abs.stdresid1)
  re1 <- as.data.frame(t(rbind(pred1,abs.stdresid1)[,ord]))
  
  KDE <- function(x) {
    w <- rep(0,n)
    for (i in 1:n) {
      w[i] <- exp(-(pred[i]-x)^2)/sqrt(2*pi)
    }
    ans <- sum(w*abs(stdresid))/sum(w)
    return(ans)
  }
  d1 <- seq(min(re1$pred1),max(re1$pred1),0.01)
  d2 <- rep(0,length(d1)) 
  for (j in 1:length(d1)) {
    d2[j] <- KDE(d1[j])
  }
  d3 <- re1$pred1
  d4 <- rep(0,length(d3))
  for (j in 1:length(d3)) {
    d4[j] <- mean(re1$abs.stdresid1[j:(j+9)],na.rm=TRUE)
  }
  
  pairs(data, main="Scatterplot matrix from given data", cex=outlier2,
        pch=21, bg=c("yellow","red")[unclass(ex1$outlier1)])
  dev.new()
  plot(y~pred, main="Dependent variable VS Fitted values", 
       xlab="Fitted response values",
       ylab="Response variables", cex=abs(stdresid)*7, pch=20, 
       col=factor(outlier2, labels=c("black", "red")))
  abline(0,1,col="blue",lwd=2)
  for (i in 1:nrow(ex2)) {
    text(ex2$pred[i], ex2$y[i], ex2$number[i], col="light blue")
  }
  dev.new()
  par(fig=c(0,0.7,0,1), new=FALSE)
  plot(stdresid~pred, main="Standard residuals VS predicted values",
       xlab="Fitted response values", ylab="Studentized residuals", 
       cex=abs(stdresid)*3, pch=20,
       col=factor(outlier2, labels=c("black", "orange"))) 
       abline(h=c(-2,0,2),col="blue",lty=c(2,1,2))
  for (i in 1:nrow(ex2)) {
    text(ex2$pred[i], ex2$stdresid[i], ex2$number[i], col="yellow")
  }
  par(fig=c(0.7,1,0,1),new=TRUE)
  boxplot(stdresid,col="green")
  for (i in 1:6) {
    text(0.7,summary(stdresid)[i],round(summary(stdresid)[i],3),col="purple")
  }
  dev.new()
  par(fig=c(0,1,0,1),new=FALSE)
  plot(abs.stdresid~pred,
       main="Absolute Standard residuals VS predicted values",
       xlab="Fitted response values",
       ylab="Absolute studentized residuals", cex=1.5, pch=std.sign)
  legend("topright",c("positive","negative"),pch=c(24,20))
  lines(d1,d2,lty=3,lwd=2,col="red")
  lines(d3,d4,lty=2,lwd=2,col="blue")
  legend("topleft",c("KDE","MA10"),lty=c(3,2),lwd=2,col=c("red","blue"))
  dev.new()
  qqnorm(stdresid) ; qqline(stdresid)
  test.res <- shapiro.test(stdresid)
  print(test.res)
  
  return(args)
}

res1 <- MyReg(dat)

str(res1) 

attributes(res1)

## Case 2 ##

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
               Residuals=residuals, Standard.Residuals=stdresid, SSR=SSR,
               SSE=SSE, F.Statistic=F, P.value=P.value)
  
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
      "------------------------------------------------------------------", "\n",
      encodeString(c("Regression", p, round(SSR,2), round(MSR,2), round(F,2), 
                     round(P.value,2)), width=8, justify="right"),
      aster(P.value), "\n", encodeString(c("     Error", n-p-1, round(SSE,2), 
                                           round(MSE,2)), width=8, justify="right"),
      "\n", encodeString(c("     Total", n-1, round(SST,2)), width=8, 
                         justify="right"), 
      "\n",
      "------------------------------------------------------------------", "\n",
      "Estimated error variance :", round(sig2,4), "\n","R-squares :",round(Rsq,4), 
      "\n\n")
  
  ind.name <- colnames(data)
  ind.name[1] <- "(Intercept)"
  test.mat <- cbind(ind.name, round(beta,4), round(std.error,4), round(t.value,4), 
                    round(Pr.t,4))
  cat("\n\n\n== PARAMETER ESTIMATES ==\n\n",
      encodeString(c(" ", "Estimate", "Std.Error", "t value", "Pr(>|t|)"), width=11, 
                   justify="right"),
      "\n")
  for(i in 1:nrow(test.mat)) {
    cat(encodeString(test.mat[i,], width=11, justify="right"), aster(Pr.t[i]),"\n")
  }
  
  cat("\n", "===Various Statistics in Multiple Regression===", "\n\n")
  print(args) 
  
  outlier1 <- rep(0,n)
  outlier2 <- rep(0,n)
  std.sign <- rep(0,n)
  for(i in 1:n) {
    outlier1[i] <- ifelse(abs(stdresid[i]) > 2, "out", "in")
    outlier2[i] <- ifelse(abs(stdresid[i]) > 2, 2, 1)
    std.sign[i] <- ifelse(stdresid[i] > 0, 24, 20)
  }
  abs.stdresid <- abs(stdresid)
  ex1 <- as.data.frame(cbind(number=(1:n), stdresid=stdresid,
                             pred=pred, y=y, 
                             outlier1=outlier1, outlier2=outlier2,
                             std.sign=std.sign, abs.stdresid=abs.stdresid))
  ex1_1 <- as.matrix(ex1[,1:4,8])
  ex1_1 <- as.numeric(ex1_1)
  ex1_1 <- matrix(ex1_1,nrow=n,ncol=5)
  colnames(ex1_1) <- c("number","stdresid","pred","y","abs.stdresid")
  ex2 <- as.data.frame(ex1_1)
  ex2 <- ex2[abs.stdresid > 2,]
  
  pred1 <- as.vector(pred) ; abs.stdresid1 <- as.vector(abs.stdresid)
  ord <- order(pred1,abs.stdresid1)
  re1 <- as.data.frame(t(rbind(pred1,abs.stdresid1)[,ord]))
  
  KDE <- function(x) {
    w <- rep(0,n)
    for (i in 1:n) {
      w[i] <- exp(-(pred[i]-x)^2)/sqrt(2*pi)
    }
    ans <- sum(w*abs(stdresid))/sum(w)
    return(ans)
  }
  d1 <- seq(min(re1$pred1),max(re1$pred1),0.01)
  d2 <- rep(0,length(d1)) 
  for (j in 1:length(d1)) {
    d2[j] <- KDE(d1[j])
  }
  d3 <- re1$pred1
  d4 <- rep(0,length(d3))
  for (j in 1:length(d3)) {
    d4[j] <- mean(re1$abs.stdresid1[j:(j+9)],na.rm=TRUE)
  }
  
  pairs(data, main="Scatterplot matrix from given data", cex=outlier2,
        pch=21, bg=c("yellow","red")[unclass(ex1$outlier1)])
  dev.new()
  plot(y~pred, main="Dependent variable VS Fitted values", 
       xlab="Fitted response values",
       ylab="Response variables", cex=abs(stdresid)*7, pch=20, 
       col=factor(outlier2, labels=c("black", "red")))
  abline(0,1,col="blue",lwd=2)
  for (i in 1:nrow(ex2)) {
    text(ex2$pred[i], ex2$y[i], ex2$number[i], col="light blue")
  }
  dev.new()
  par(fig=c(0,0.7,0,1), new=FALSE)
  plot(stdresid~pred, main="Standard residuals VS predicted values",
       xlab="Fitted response values", ylab="Studentized residuals", 
       pch=21, cex=1, col="black") 
  abline(h=c(-2,0,2),col="saddlebrown",lty=c(2,1,2))
  for (i in 1:nrow(ex2)) {
    text(ex2$pred[i], ex2$stdresid[i], ex2$number[i], col="darkgoldenrod", pos=2)
    points(ex2$pred[i], ex2$stdresid[i], pch=19, cex=2, col="violetred")
  }
  par(fig=c(0.7,1,0,1),new=TRUE)
  boxplot(stdresid,col="seagreen")
  for (i in 1:6) {
    text(0.7,summary(stdresid)[i],round(summary(stdresid)[i],3),col="darkmagenta")
  }
  dev.new()
  par(fig=c(0,1,0,1),new=FALSE)
  plot(abs.stdresid~pred,
       main="Absolute Standard residuals VS predicted values",
       xlab="Fitted response values",
       ylab="Absolute studentized residuals", cex=1.5, pch=std.sign)
  legend("topright",c("positive","negative"),pch=c(24,20))
  lines(d1,d2,lty=3,lwd=2,col="red")
  lines(d3,d4,lty=2,lwd=2,col="blue")
  legend("topleft",c("KDE","MA10"),lty=c(3,2),lwd=2,col=c("red","blue"))
  dev.new()
  qqnorm(stdresid) ; qqline(stdresid)
  test.res <- shapiro.test(stdresid)
  print(test.res)
  
  return(args)
}

res1 <- MyReg(dat)

str(res1) 

attributes(res1)