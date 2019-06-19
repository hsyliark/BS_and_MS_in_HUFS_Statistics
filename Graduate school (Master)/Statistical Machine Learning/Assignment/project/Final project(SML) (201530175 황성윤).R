##### Statistical Machine Learning
##### Carseats {ISLR} : 'Sales' prediction

#### Opening Carseats data
install.packages("ISLR")
library(ISLR)
data(Carseats) ; dim(Carseats)
attach(Carseats)

### Give an explanation of Carseats data

## 서로 다른 400가지의 상점들이 제시하는 어린이용 차량시트의 가격과 관련됨.

# Sales : 판매가격, CompPrice : 경쟁사에 의해 부가된 가격,
# Income : 수입, Advertising : 광고비용,
# Population : 인구, Price : 자회사에서 부가한 가격, 
# ShelveLoc : 차량시트의 기울기에 대한 평가(Good,Medium,Bad), Age : 거주자들의 평균나이,
# Education : 교육수준, Urban : 도시지역은 Yes이고 교외지역은 No,
# US : 미국 내에 자리한 회사인지에 대한 여부

## Introduction of project

## Sales를 예측하기 위한 8가지 통계적인 방법론들 중 최적의 방법을 찾는 것이 목적.

# 1. 전체의 Carseats data를 사용해서 각각의 방법론들에 의해 제시되는
#    유의한 변수들이나 best lambda 값, 또는 component의 개수를 찾는다.
# 2. 임의로 난수를 뽑아 전체 data의 1/10 크기의 test data를 설정하고
#    나머지를 train data로 설정한다.
# 3. train data를 이용해 1번에서 설정한 결과에 따른 model을 만들고
#    test MSE 값을 계산한다.
# 4. 2번과 3번의 과정을 1000번 반복하여 1000개의 test MSE 값을 산출하고
#    평균을 계산하여 가장 작은 값이 나온 방법론을 최적으로 판단한다.

### Multiple Linear Regression

## Choosing best model
car.lm <- lm(Sales~.,data=Carseats)
summary(car.lm)
car.lm <- lm(Sales~.-Population-Education-Urban-US,data=Carseats)
summary(car.lm) # Select this~~!

## Simulation
n <- dim(Carseats)[1]
test.size <- round(n/10)
test.mse.lm <- NULL
for (i in 1:1000) {
  test <- sample(1:n, size=test.size, replace=F)
  carseats.train <- Carseats[-test,]
  car.test <- Carseats[test,-1]; actual <- Carseats[test,1]
  car.train <- Carseats[-test,-1]
  car.lm <- lm(Sales~.-Population-Education-Urban-US,data=carseats.train)
  pred.lm <- predict(car.lm,car.test)
  test.mse.lm <- c(test.mse.lm, mean((pred.lm-actual)^2))
}

### Ridge Regression

## Choosing best model
x <- model.matrix(Sales~.,data=Carseats)[,-1]
y <- Carseats$Sales
install.packages("glmnet")
library(glmnet)
grid <- 10^seq(10,-2,length=100)
par(mfrow=c(1,2))
car.ridge <- glmnet(x,y,alpha=0,lambda=grid)
plot(car.ridge)
cv.car.ridge <- cv.glmnet(x,y,alpha=0)
plot(cv.car.ridge)
bestlam.ridge <- cv.car.ridge$lambda.min  
bestlam.ridge # Value of lambda that gives minimum mean cross-validated error
out.ridge <- glmnet(x,y,alpha=0)
predict(out.ridge,type="coefficients",s=bestlam.ridge) # Select this~~!

## Simulation
n <- dim(Carseats)[1]
test.size <- round(n/10)
test.mse.ridge <- NULL
for (i in 1:1000) {
  test <- sample(1:n, size=test.size, replace=F)
  car.test <- x[test,]; actual <- y[test]
  car.train <- x[-test,]; car.train.actual <- y[-test]
  car.ridge <- glmnet(car.train,car.train.actual,alpha=0,lambda=grid)
  ridge.pred <- predict(car.ridge,s=bestlam.ridge,newx=car.test)
  test.mse.ridge <- c(test.mse.ridge, mean((ridge.pred-actual)^2))
}

### Lasso Regression

## Choosing best model
x <- model.matrix(Sales~.,data=Carseats)[,-1]
y <- Carseats$Sales
install.packages("glmnet")
library(glmnet)
grid <- 10^seq(10,-2,length=100)
par(mfrow=c(1,2))
car.lasso <- glmnet(x,y,alpha=1,lambda=grid)
plot(car.lasso)
cv.car.lasso <- cv.glmnet(x,y,alpha=1)
plot(cv.car.lasso)
bestlam.lasso <- cv.car.lasso$lambda.min 
bestlam.lasso # Value of lambda that gives minimum mean cross-validated error
out.lasso <- glmnet(x,y,alpha=1)
predict(out.lasso,type="coefficients",s=bestlam.lasso) # Select this~~!

## Simulation
n <- dim(Carseats)[1]
test.size <- round(n/10)
test.mse.lasso <- NULL
for (i in 1:1000) {
  test <- sample(1:n, size=test.size, replace=F)
  car.test <- x[test,]; actual <- y[test]
  car.train <- x[-test,]; car.train.actual <- y[-test]
  car.lasso <- glmnet(car.train,car.train.actual,alpha=1,lambda=grid)
  lasso.pred <- predict(car.lasso,s=bestlam.lasso,newx=car.test)
  test.mse.lasso <- c(test.mse.lasso, mean((lasso.pred-actual)^2))
}

### PCR Regression

## Choosing best model
x <- model.matrix(Sales~.,data=Carseats)[,-1]
y <- Carseats$Sales
install.packages("pls")
library(pls)
par(mfrow=c(1,1))
pcr.fit <- pcr(Sales~.,data=Carseats,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP") # Choosing ncomp=10 ~~!

## Simulation
n <- dim(Carseats)[1]
test.size <- round(n/10)
test.mse.pcr <- NULL
for (i in 1:1000) {
  test <- sample(1:n, size=test.size, replace=F)
  carseats.train <- Carseats[-test,]
  car.test <- x[test,]; actual <- y[test]
  car.train <- x[-test,]
  pcr.fit <- pcr(Sales~.,data=carseats.train,scale=TRUE,validation="CV")
  pcr.pred <- predict(pcr.fit,car.test,ncomp=10)
  test.mse.pcr <- c(test.mse.pcr, mean((pcr.pred-actual)^2))
}

### PLS Regression

## Choosing best model
x <- model.matrix(Sales~.,data=Carseats)[,-1]
y <- Carseats$Sales
install.packages("pls")
library(pls)
par(mfrow=c(1,1))
pls.fit <- plsr(Sales~.,data=Carseats,scale=TRUE,validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP") # Choosing ncomp=2 ~~!

## Simulation
n <- dim(Carseats)[1]
test.size <- round(n/10)
test.mse.pls <- NULL
for (i in 1:1000) {
  test <- sample(1:n, size=test.size, replace=F)
  carseats.train <- Carseats[-test,]
  car.test <- x[test,]; actual <- y[test]
  car.train <- x[-test,]
  pls.fit <- plsr(Sales~.,data=carseats.train,scale=TRUE,validation="CV")
  pls.pred <- predict(pls.fit,car.test,ncomp=2)
  test.mse.pls <- c(test.mse.pls, mean((pls.pred-actual)^2))
}

### Cubic Splines

## Choosing best model
install.packages("splines")
library(splines)
car.splines <- lm(Sales~bs(CompPrice)+bs(Income)+bs(Advertising)+bs(Population)
                  +bs(Price)+ShelveLoc+bs(Age)+bs(Education)+Urban+US,data=Carseats)
summary(car.splines)
car.splines <- lm(Sales~bs(CompPrice)+bs(Advertising)
                  +bs(Price)+ShelveLoc+bs(Age),data=Carseats)
summary(car.splines) # Select this ~~!

## Simulation
n <- dim(Carseats)[1]
test.size <- round(n/10)
test.mse.splines <- NULL
for (i in 1:1000) {
  test <- sample(1:n, size=test.size, replace=F)
  carseats.train <- Carseats[-test,]
  car.test <- Carseats[test,-1]; actual <- Carseats[test,1]
  car.train <- Carseats[-test,-1]
  car.splines <- lm(Sales~bs(CompPrice)+bs(Advertising)
                    +bs(Price)+ShelveLoc+bs(Age),data=carseats.train)
  pred.splines <- predict(car.splines,car.test)
  test.mse.splines <- c(test.mse.splines, mean((pred.splines-actual)^2))
}

### Generalized Additive Models

## Choosing best model
install.packages("gam")
library(gam)
car.gam <- gam(Sales~s(CompPrice)+s(Income)+s(Advertising)+s(Population)
               +s(Price)+ShelveLoc+s(Age)+s(Education)+Urban+US,data=Carseats)
summary(car.gam)
car.gam <- gam(Sales~s(CompPrice)+s(Income)+s(Advertising)
               +s(Price)+ShelveLoc+s(Age),data=Carseats)
summary(car.gam) # Select this ~~!
par(mfrow=c(2,3))
plot(car.gam,se=TRUE,col="green")

## Simulation
n <- dim(Carseats)[1]
test.size <- round(n/10)
test.mse.gam <- NULL
for (i in 1:1000) {
  test <- sample(1:n, size=test.size, replace=F)
  carseats.train <- Carseats[-test,]
  car.test <- Carseats[test,-1]; actual <- Carseats[test,1]
  car.train <- Carseats[-test,-1]
  car.gam <- gam(Sales~s(CompPrice)+s(Income)+s(Advertising)
                 +s(Price)+ShelveLoc+s(Age),data=carseats.train)
  pred.gam <- predict(car.gam,car.test)
  test.mse.gam <- c(test.mse.gam, mean((pred.gam-actual)^2))
}

### Random Forests

## Choosing best model
install.packages("randomForest")
library(randomForest)
par(mfrow=c(1,1))
car.rf <- randomForest(Sales~.,data=Carseats,importance=TRUE,ntree=500)
summary(car.rf) 
# The number of variables randomly sampled as candidates at each split is p/3 
# where p is number of predictors. 
# The number of tree is 500.
importance(car.rf)
varImpPlot(car.rf)

## Simulation
n <- dim(Carseats)[1]
test.size <- round(n/10)
test.mse.rf <- NULL
for (i in 1:1000) {
  test <- sample(1:n, size=test.size, replace=F)
  carseats.train <- Carseats[-test,]
  carseats.test <- Carseats[test,]
  car.test <- Carseats[test,-1]; actual <- Carseats[test,1]
  car.train <- Carseats[-test,-1]
  car.rf <- randomForest(Sales~.,data=carseats.train,importance=TRUE,ntree=500)
  pred.rf <- predict(car.rf,newdata=carseats.test)
  test.mse.rf <- c(test.mse.rf, mean((pred.rf-actual)^2))
}

### Final result

## Test Mean Squared Error
test.mse <- cbind(test.mse.lm,test.mse.ridge,test.mse.lasso,test.mse.pcr,
                  test.mse.pls,test.mse.splines,test.mse.gam,test.mse.rf)
head(test.mse)
apply(test.mse,2,mean) # Calculate mean

## Drawing boxplot
par(mfrow=c(2,2))
boxplot(test.mse.lm,main="Multiple Linear Regression",ylab="Test Mean Squared Error")
boxplot(test.mse.ridge,main="Ridge Regression",ylab="Test Mean Squared Error")
boxplot(test.mse.lasso,main="Lasso Regression",ylab="Test Mean Squared Error")
boxplot(test.mse.pcr,main="PCR Regression",ylab="Test Mean Squared Error")
boxplot(test.mse.pls,main="PLS Regression",ylab="Test Mean Squared Error")
boxplot(test.mse.splines,main="Cubic Splines",ylab="Test Mean Squared Error")
boxplot(test.mse.gam,main="Generalized Additive Models",ylab="Test Mean Squared Error")
boxplot(test.mse.rf,main="Random Forests",ylab="Test Mean Squared Error")

