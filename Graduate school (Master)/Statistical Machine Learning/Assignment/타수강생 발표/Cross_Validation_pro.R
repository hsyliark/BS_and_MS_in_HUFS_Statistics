## data
library(ISLR)
data(Auto)
head(Auto)
auto <- Auto[,c(1,4)]

## LOOCV(1~10차)
n <- nrow(auto)
#cv.n <- rep(NA,10)
MSE <- matrix(NA,n,10)
for(j in 1:10) {
  
  for(i in 1:n){
    loocv.train <- auto[-i,];  loocv.valid <- auto[i,]
    yi.hat <- predict(lm(mpg ~ poly(horsepower,j), loocv.train), loocv.valid)
    MSE[i,j] <- (loocv.valid$mpg - yi.hat)^2
  }
#  cv.n[j] <- sum(MSE[,j])/n
}
cv.n<-colMeans(MSE)

#plot
plot(cv.n, ylim=range(16:28), xlab="Degree of Polynomial", ylab="Mean Squared Error",
     type="b", pch=19, lwd=1.5, main="LOOCV")


## k-fold cv
# data split
cv.k <- NULL
for(a in 1:10){
  cat('The a =' , a, 'th is running....\n')
k=10
auto.fold <- sample(1:n,replace=F)
auto$folds <- auto.fold%%k
table(auto$folds)

# 10-fold cv
#cv.k <- rep(NA,10)
MSE.k <- matrix(NA,k,10)
for(j in 1:10) {
  
  for(i in 1:k){
    train.fold <- auto[auto$folds!=i-1,]
    test.fold <- auto[auto$folds==i-1,]
    yk.hat <- predict(lm(mpg ~ poly(horsepower,j), train.fold), test.fold)
    MSE.k[i,j] <- sum((test.fold$mpg - yk.hat)^2)/(length(test.fold$mpg))  
  }
#  cv.k[j] <- sum(MSE.k[,j])/k
}
cv.k <- rbind(cv.k, colMeans(MSE.k))
}
#plot
matplot(t(cv.k), ylim=c(16,28), col=rainbow(10), xlab='Degree of Polynomial', ylab='Mean Squared Error', type='l', lwd=2, main='10-fold CV', lty=1)
#plot(cv.k, ylim=range(16:28), xlab="Degree of Polynomial", ylab="Mean Squared Error",
     type="l", lwd=1.5, col=1, main="10-fold CV")
#lines(cv.k, ylim=range(16:28), xlab="Degree of Polynomial", ylab="Mean Squared Error",
#      type="l", lwd=1.5, col=12, main="10-fold CV")
