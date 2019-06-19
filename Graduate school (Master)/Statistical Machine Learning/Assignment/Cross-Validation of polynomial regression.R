### Leave-One-Out Cross-Validation for Auto data

install.packages("ISLR")
library(ISLR)
data(Auto)
dim(Auto)
View(Auto)
par(mfrow=c(1,2))

## Polynomial regression

attach(Auto)
origin.lm <- lm(mpg~horsepower)
summary(origin.lm)
detach(Auto)

loocv.mse <- NULL
poly.degree <- NULL
cv.mse <- NULL
loocv.auto <- Auto[,c(1,4)]
n <- dim(Auto)[1]
k <- 10
for (j in 1:k) {
  for (i in 1:n) {
    test.auto <- loocv.auto[i,]
    train.auto <- loocv.auto[-i,]
    poly.lm <- lm(mpg~poly(horsepower,j),data=train.auto)
    poly.pred <- predict(poly.lm,test.auto)
    cv.mse[i] <- (test.auto[,1]-poly.pred)^2
  }
  poly.degree[j] <- j
  loocv.mse[j] <- sum(cv.mse)/n
}

## Drawing graph
poly.degree
loocv.mse
plot(loocv.mse~poly.degree,main="Leave-One-Out Cross-Validation",
     xlab="Degree of Polynomial",ylab="Mean Squared Error",type="l",
     pch=20,col="darkgreen",lwd=2,ylim=c(16,28))
for (i in 1:10) {
  points(poly.degree[i],loocv.mse[i],col="blue",pch=20,cex=2)
}

### 10-Fold Cross-Validation

k <- 10
n <- dim(Auto)[1]
num <- 1:dim(Auto)[1]
poly.degree <- NULL
cv.mse1 <- NULL ; cv.mse2 <- NULL ; cv.mse3 <- NULL
cv.mse4 <- NULL ; cv.mse5 <- NULL ; cv.mse6 <- NULL
cv.mse7 <- NULL ; cv.mse8 <- NULL ; cv.mse9 <- NULL
cv.mse <- data.frame(cv.mse1=cv.mse1,cv.mse2=cv.mse2,cv.mse3=cv.mse3,
                     cv.mse4=cv.mse4,cv.mse5=cv.mse5,cv.mse6=cv.mse6,
                     cv.mse7=cv.mse7,cv.mse8=cv.mse8,cv.mse9=cv.mse9)

for (i in 1:9) {
  for (j in 1:k) {
    ## Folding data
    select1 <- sample(num,39,replace=FALSE)
    select2 <- sample(num[-c(select1)],39,replace=FALSE)
    select3 <- sample(num[-c(select1,select2)],39,replace=FALSE)
    select4 <- sample(num[-c(select1,select2,select3)],39,replace=FALSE)
    select5 <- sample(num[-c(select1,select2,select3,select4)],39,replace=FALSE)
    select6 <- sample(num[-c(select1,select2,select3,select4,select5)],39,
                      replace=FALSE)
    select7 <- sample(num[-c(select1,select2,select3,select4,select5,select6)],39,
                      replace=FALSE)
    select8 <- sample(num[-c(select1,select2,select3,select4,select5,select6,
                             select7)],39,replace=FALSE)
    select9 <- sample(num[-c(select1,select2,select3,select4,select5,select6,
                             select7,select8)],39,replace=FALSE)
    select10 <- sample(num[-c(select1,select2,select3,select4,select5,select6,
                              select7,select8,select9)],41,replace=FALSE)
    fold.auto <- Auto[,c(1,4)]
    
    ## Training data and Test data
    mse <- NULL
    # MSE1
    test.fold.auto <- fold.auto[select1,]
    train.fold.auto <- fold.auto[-select1,]
    poly.lm <- lm(mpg~poly(horsepower,j),data=train.fold.auto)
    poly.pred <- predict(poly.lm,test.fold.auto)
    mse1 <- sum((test.fold.auto[,1]-poly.pred)^2)/length(select1)
    mse[1] <- mse1
    # MSE2
    test.fold.auto <- fold.auto[select2,]
    train.fold.auto <- fold.auto[-select2,]
    poly.lm <- lm(mpg~poly(horsepower,j),data=train.fold.auto)
    poly.pred <- predict(poly.lm,test.fold.auto)
    mse2 <- sum((test.fold.auto[,1]-poly.pred)^2)/length(select2)
    mse[2] <- mse2
    # MSE3
    test.fold.auto <- fold.auto[select3,]
    train.fold.auto <- fold.auto[-select3,]
    poly.lm <- lm(mpg~poly(horsepower,j),data=train.fold.auto)
    poly.pred <- predict(poly.lm,test.fold.auto)
    mse3 <- sum((test.fold.auto[,1]-poly.pred)^2)/length(select3)
    mse[3] <- mse3
    # MSE4
    test.fold.auto <- fold.auto[select4,]
    train.fold.auto <- fold.auto[-select4,]
    poly.lm <- lm(mpg~poly(horsepower,j),data=train.fold.auto)
    poly.pred <- predict(poly.lm,test.fold.auto)
    mse4 <- sum((test.fold.auto[,1]-poly.pred)^2)/length(select4)
    mse[4] <- mse4
    # MSE5
    test.fold.auto <- fold.auto[select5,]
    train.fold.auto <- fold.auto[-select5,]
    poly.lm <- lm(mpg~poly(horsepower,j),data=train.fold.auto)
    poly.pred <- predict(poly.lm,test.fold.auto)
    mse5 <- sum((test.fold.auto[,1]-poly.pred)^2)/length(select5)
    mse[5] <- mse5
    # MSE6
    test.fold.auto <- fold.auto[select6,]
    train.fold.auto <- fold.auto[-select6,]
    poly.lm <- lm(mpg~poly(horsepower,j),data=train.fold.auto)
    poly.pred <- predict(poly.lm,test.fold.auto)
    mse6 <- sum((test.fold.auto[,1]-poly.pred)^2)/length(select6)
    mse[6] <- mse6
    # MSE7
    test.fold.auto <- fold.auto[select7,]
    train.fold.auto <- fold.auto[-select7,]
    poly.lm <- lm(mpg~poly(horsepower,j),data=train.fold.auto)
    poly.pred <- predict(poly.lm,test.fold.auto)
    mse7 <- sum((test.fold.auto[,1]-poly.pred)^2)/length(select7)
    mse[7] <- mse7
    # MSE8
    test.fold.auto <- fold.auto[select8,]
    train.fold.auto <- fold.auto[-select8,]
    poly.lm <- lm(mpg~poly(horsepower,j),data=train.fold.auto)
    poly.pred <- predict(poly.lm,test.fold.auto)
    mse8 <- sum((test.fold.auto[,1]-poly.pred)^2)/length(select8)
    mse[8] <- mse8
    # MSE9
    test.fold.auto <- fold.auto[select9,]
    train.fold.auto <- fold.auto[-select9,]
    poly.lm <- lm(mpg~poly(horsepower,j),data=train.fold.auto)
    poly.pred <- predict(poly.lm,test.fold.auto)
    mse9 <- sum((test.fold.auto[,1]-poly.pred)^2)/length(select9)
    mse[9] <- mse9
    # MSE10
    test.fold.auto <- fold.auto[select10,]
    train.fold.auto <- fold.auto[-select10,]
    poly.lm <- lm(mpg~poly(horsepower,j),data=train.fold.auto)
    poly.pred <- predict(poly.lm,test.fold.auto)
    mse10 <- sum((test.fold.auto[,1]-poly.pred)^2)/length(select10)
    mse[10] <- mse10
    
    poly.degree[j] <- j
    cv.mse[i,j] <- sum(mse)/k # Mean Squared Error
  }
}

cv.mse <- t(cv.mse)
cv.mse <- as.matrix(cv.mse)
colnames(cv.mse) <- c("cv.mse1","cv.mse2","cv.mse3","cv.mse4","cv.mse5",
                      "cv.mse6","cv.mse7","cv.mse8","cv.mse9")
cv.mse <- as.data.frame(cv.mse)
View(cv.mse)

## Drawing graph
plot(poly.degree,cv.mse[,1],main="10-Fold Cross-Validation",xlab="Degree of Polynomial",
     ylab="Mean Squared Error",ylim=c(16,28),type="n")
for (i in 1:9) {
  lines(poly.degree,cv.mse[,i],col=i,lwd=1)
}
