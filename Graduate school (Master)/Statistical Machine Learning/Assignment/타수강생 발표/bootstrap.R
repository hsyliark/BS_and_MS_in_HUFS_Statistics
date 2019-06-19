##Data
library(ISLR)
data(Auto)
head(Auto)

##Bootstrap function

#boot.list <- list()
boot.result <- NULL
for(i in 1:1000){
  print(i)
  dat <- Auto 
  data <- data.frame(mpg=dat[,1],horsepower=dat[,4])
  boot.dat <- data[sample(length(data[,1]),length(data[,1]),replace=T),]
  boot.auto <- lm(mpg~horsepower,data=boot.dat)
  #boot.list[[i]] <-summary(boot.auto)$coefficients[,1]
  boot.result <- rbind(boot.result, summary(boot.auto)$coefficients[,1])
}


#Boot.intercept <- sd(unlist(lapply(1:1000, function(i) boot.list[[i]][1])))
#Boot.horsepower <- sd(unlist(lapply(1:1000, function(i) boot.list[[i]][2])))

##comparison Bootstrap with raw data
Boot.all <- apply(boot.result,2,sd)
auto <- lm(mpg ~ horsepower, data=Auto)
summary(auto)$coefficients[,2]
Boot.all
#Boot.intercept
#Boot.horsepower

##histogram (When you use list method)
par(mfrow=c(1,2))
hist(unlist(lapply(1:1000, function(i) boot.list[[i]][1])),col='blue',main="Boot.intercept")
abline(v=Boot.intercept,col='red',lwd=5)
hist(unlist(lapply(1:1000, function(i) boot.list[[i]][2])),col='orange',main="Boot.horsepower")
abline(v=Boot.horsepower,col='red',lwd=5)
