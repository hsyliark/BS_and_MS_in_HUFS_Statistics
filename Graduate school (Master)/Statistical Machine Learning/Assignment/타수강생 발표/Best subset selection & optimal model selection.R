library(dplyr)

data <- read.csv('Credit.csv')
str(data)
attach(data)
y <- Balance
data$Eth_AA <- (Ethnicity=="African American")*1
data$Eth_A <- (Ethnicity=="Asian")*1

x <- select(data, -X, -Balance, -Ethnicity)

combn.model <- list()
rss.model <- list()
rss <- list()
mse <- list()
foo <- list()


df <- c()
for(i in 1:ncol(x)){
  if(i==1){
    var.names <- paste("x[,combn.model[[j]][,i]",'[',i,']]',sep='')
    df[i] <- paste('y~', var.names, sep='')
  }
  
  if(i!=1){  
    df[i] <- paste(df[i-1] ,paste("x[,combn.model[[j]][,i]",'[',i,']]',sep=''), sep='+')
  }
}


for(j in 1:ncol(x)){
  combn.model[[j]] <- combn(names(x), j)  
  rss.model[[j]] <- matrix(NA, nrow = ncol(combn.model[[j]]))
  for(i in 1:ncol(combn.model[[j]])){
    rss.model[[j]][i,] <- sum(summary(lm(df[j]))$residuals^2)
    rss[[j]] <- min(rss.model[[j]])
    mse[[j]] <- rss[[j]]/nrow(x)
    foo[[j]] <- combn.model[[j]][ , which.min(rss.model[[j]])]    
  }
  print(j)
}


cp <- list()

for(i in 1:ncol(x)){
  cp[[i]] <- (rss[[i]]+2*nrow(combn.model[[i]])*mse[[i]])/nrow(x)
}

AIC <- list()

for(i in 1:ncol(x)){
  AIC[[i]] <- (rss[[i]]+2*nrow(combn.model[[i]])*mse[[i]])/(mse[[i]]*nrow(x))
}

BIC <- list()

for(i in 1:ncol(x)){
  BIC[[i]] <-(rss[[i]]+(log(nrow(x))*nrow(combn.model[[i]])*mse[[i]]))/(mse[[i]]*nrow(x))
}

df.2 <- c()
for(i in 1:ncol(x)){
  if(i==1){
    var.names <- paste("x[,foo[[j]]",'[',i,']]',sep='')
    df.2[i] <- paste('y~', var.names, sep='')
  }
  
  if(i!=1){
    
    df.2[i] <- paste(df.2[i-1], paste("x[,foo[[j]]",'[',i,']]',sep=''), sep='+')
  } 
} 

R.adj <- list()
for(j in 1:ncol(x)){
  R.adj[j] <- summary(lm(df.2[j]))[9]
}

k <- 10
data.fold <- sample(1:nrow(x), replace=F)
folds <- data.fold%%k
table(folds)
mse.k <- matrix(NA, k, ncol(x))

df.3 <- c()
df.3[1] <- paste('Balance ~ Rating')
df.3[2] <- paste('Balance ~ Income + Rating')
df.3[3] <- paste('Balance ~ Income + Rating + Student')
df.3[4] <- paste('Balance ~ Income + Limit + Cards + Student')
df.3[5] <- paste('Balance ~ Income + Limit + Rating + Cards + Student')
df.3[6] <- paste('Balance ~ Income + Limit + Rating + Cards + Age + Student')
df.3[7] <- paste('Balance ~ Income + Limit + Rating + Cards + Age + Gender + Student')
df.3[8] <- paste('Balance ~ Income + Limit + Rating + Cards + Age + Gender + Student + Eth_AA')
df.3[9] <- paste('Balance ~ Income + Limit + Rating + Cards + Age + Gender + Student + Married + Eth_AA')
df.3[10] <- paste('Balance ~ Income + Limit + Rating + Cards + Age + Education + Gender + Student + Married + Eth_AA')
df.3[11] <- paste('Balance ~ Income + Limit + Rating + Cards + Age + Education + Gender + Student + Married + Eth_AA + Eth_A')


for(j in 1:ncol(x)){
  for(i in 1:k){
    train.data <- data[folds != i-1, ]
    test.data <- data[folds == i-1, ]
    y.hat <- predict(lm(df.3[j], train.data), test.data[,c("Balance", foo[[j]])])
    mse.k[i,j] <- sum((test.data$Balance - y.hat)^2)/nrow(test.data)  
  }
  print(j)
}

mse.res <- colMeans(mse.k)


no.pre <- 1:ncol(x)

par(mfrow=c(1,3))

plot(no.pre, cp, type = "o" , lty = 1, pch = 21, col = "hotpink" ,
     ylim = c(10000, 30000), xlab = "Number of Predictors", ylab = "Cp", bg="dodgerblue3")
points(no.pre, cp, col="dodgerblue3")
points(which.min(cp), cp[[which.min(cp)]], col="dodgerblue3", cex=3, pch=4)
abline(0, 0, v = which.min(cp), lty = 2, col = "grey47")

plot(no.pre, R.adj, type = "o" , lty = 1, pch = 21, col = "hotpink",
     ylim = c(0.86, 0.96), xlab = "Number of Predictors", ylab = "Adjusted R^2", bg="dodgerblue3")
points(no.pre, R.adj, col="dodgerblue3")
points(which.max(R.adj), R.adj[[which.max(R.adj)]], col="dodgerblue3", cex=3, pch=4)
abline(0, 0, v = which.max(R.adj), lty = 2, col = "grey47")

plot(no.pre, mse.res, type = "o" , lty = 1, pch = 21, col = "hotpink",
     ylim = c(10000, 30000), xlab = "Number of Predictors", ylab = "10-fold CV", bg="dodgerblue3")
points(no.pre, mse.res, col="dodgerblue3")
points(which.min(mse.res), mse.res[[which.min(mse.res)]], col="dodgerblue3", cex=3, pch=4)
abline(0, 0, v = which.min(mse.res), lty = 2, col = "grey47")

BIC2 <- list()

for(i in 1:ncol(x)){
  #i=1
  BIC2[[i]] <-(rss[[i]]+(log(nrow(x))*nrow(combn.model[[i]])*mse[[i]]))/nrow(x)
}

plot(no.pre, BIC2, type = "o" , lty = 1, pch = 21, col = "hotpink",
     ylim = c(10000, 30000), xlab = "Number of Predictors", ylab = "BIC", bg="dodgerblue3")
points(no.pre, BIC2, col="dodgerblue3")
points(which.min(BIC2), BIC2[[which.min(BIC2)]], col="dodgerblue3", cex=3, pch=4)
abline(0, 0, v = which.min(BIC2), lty = 2, col = "grey47")



library(leaps)
regfit.full <- regsubsets(y~., x, nvmax = 11)
regfit.forward <- regsubsets(y~., x, nvmax = 11, method = 'forward')
reg.summary <- summary(regfit.full)
reg.summary.forward <- summary(regfit.forward)


reg.summary$rss
unlist(rss)

reg.summary$cp
unlist(cp)

reg.summary$bic
unlist(BIC2)

reg.summary$adjr2
unlist(R.adj)

coef(regfit.full, 4)





