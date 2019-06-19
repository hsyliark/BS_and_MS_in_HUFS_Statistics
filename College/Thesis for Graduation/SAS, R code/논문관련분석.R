german <- read.csv("D:/수업자료/대학/(4)Senior/졸업논문/SAS, R code/germancredit1.csv",sep=",",header=T)
germanknn <- read.csv("D:/수업자료/대학/(4)Senior/졸업논문/SAS, R code/germanknn1.csv",sep=",",header=T)
germansvm <- read.csv("D:/수업자료/대학/(4)Senior/졸업논문/SAS, R code/germansvm1.csv",sep=",",header=T)
head(german)

#### Optimal method
--------------------
  
  ### 1. Tuning (KNN, SVM, Decision Tree 를 사용할 때 가장 최적의 방법을 찾는다.)
  
  install.packages("MASS") # LDA에 필요
library(MASS)
install.packages("class") # KNN에 필요
library(class)
install.packages("e1071") # SVM과 Tuning에 필요
library(e1071)
install.packages("rpart") # Decision Tree에 필요
library(rpart)

# 데이터의 절반 크기인 train data 생성
# (이산형인 설명변수들은 모두 dummy variable로 변환시킴.)
# sampling 방법은 'boot'을 적용함.

## 1. K-nearest Neighbor에서 적절한 K값은?

n <- dim(germanknn)[1]
train.size <- round(n/2)
train <- sample(1:n, size=train.size, replace=F)
german.train <- germanknn[train,-1]
german.test <- germanknn[train,1]

obj1 <- tune.knn(german.train,german.test,k=1:120,tunecontrol=tune.control(sampling ="boot")) 
summary(obj1) # k=73
attributes(obj1)
plot(obj1)

## 2. Support vector machine에서 적절한 gamma와 cost의 값은?

n <- dim(germansvm)[1]
test.size <- round(n/2)
test <- sample(1:n, size=test.size, replace=F)
german.test <- germansvm[test,-1]
german.train <- germansvm[-test,]

obj2 <- tune(svm,Default~.,data=german.train,ranges=list(gamma=seq(0.01,0.1,by=0.001),cost=seq(0.25,2,by=0.25)),tunecontrol=tune.control(sampling ="boot"))
summary(obj2) # gamma=0.026, cost=0.75
attributes(obj2)
plot(obj2)

## 3. Decision Tree를 적용할 시 적절한 해결책은(Pruning)?

n <- dim(german)[1]
test.size <- round(n/2)
test <- sample(1:n, size=test.size, replace=F)
german.test <- german[test,-1]
german.train <- german[-test,]

obj3 <- rpart(Default~.,data=german.train)
install.packages("partykit")
library(partykit)
plot(as.party(obj3))
printcp(obj3)
plotcp(obj3) # Size=3, cp=0.046

### 2. 최적의 Logistic Regression Model은?

glm.german <- glm(Default~.,data=german,family="binomial")
summary(glm.german)
# Unavailable explanatory valiables :
# purpose, savings, employ, status, residence, property,
# age, housing, cards, liable, tele

glm.german <- glm(Default~checkingstatus1+checkingstatus2+checkingstatus3+duration+history1+history2+history3+history4+amount+installment1+installment2+installment3+others1+others2+otherplans1+otherplans2+foreign,data=german,family="binomial")
summary(glm.german)
# Unavailable explanatory valiables : amount

glm.german <- glm(Default~checkingstatus1+checkingstatus2+checkingstatus3+duration+history1+history2+history3+history4+installment1+installment2+installment3+others1+others2+otherplans1+otherplans2+foreign,data=german,family="binomial")
summary(glm.german)
# Unavailable explanatory valiables : installment

glm.german <- glm(Default~checkingstatus1+checkingstatus2+checkingstatus3+duration+history1+history2+history3+history4+others1+others2+otherplans1+otherplans2+foreign,data=german,family="binomial")
summary(glm.german) # Best model~~!!
plot(glm.german)

anova(glm.german,test="Cp")
anova(glm.german,test="LRT")
anova(glm.german,test="Rao")

#### Calculate Misclassification Rate
--------------------------------------
  
  ## 위의 분석을 통해서 얻은 최적의 방법을 적용하여 
  ## 각 방법론에 따른 오분류율을 계산한다.
  ## 데이터의 크기는 Train : Test = 3:2 로 할 것이며,
  ## 각 방법론마다 Train data와 Test data로 나누는 작업을
  ## 5000번 반복하여 시행에 따른 오분류율을 계산하고
  ## 최종적으로 평균을 내서 어느 방법론이
  ## 최소의 오분류율을 산출해내는지 알아본다.
  ## (KNN의 경우는 train data와 test data의 size가
  ## 반드시 같아야 함수가 작동하므로 예외로 한다.)
  ## 단, LDA(선형판별분석)의 경우에는
## 따로 최적화된 모형을 선정하는 기준이 없으므로
## 모든 설명변수들을 적용한다.
## Decision Tree와 Logistic Regression Model의 경우는
## Pr(Default=1) >= 0.5 인 상황을
## 신용도가 좋지 않은 사람이라고 간주하고 실시함.

### 1. K-nearest Neighbor

## KNN의 경우는 train data와 test data의 size가
## 반드시 같아야 함수가 작동하므로 유의해야 한다.

par(mfrow=c(1,1))
n <- dim(germanknn)[1]
test.size <- round(n/2)
error.rate.knn <- NULL
for ( i in 1:5000) {
  test <- sample(1:n, size=test.size, replace=F)
  german.test <- germanknn[test,-1]; actual <- germanknn[test,1]
  german.train <- germanknn[-test,-1]
  pred.knn <- knn(german.train,german.test,actual,k=73)
  error.rate.knn <- c(error.rate.knn, mean(actual != pred.knn))
}
boxplot(error.rate.knn,main="Misclassification rate (K-nearest Neighbor)")
mean(error.rate.knn) # 0.2999408

### 2. Support Vector Machine

par(mfrow=c(1,1))
n <- dim(germansvm)[1]
test.size <- round(2*n/5)
error.rate.svm <- NULL
for ( i in 1:5000) {
  test <- sample(1:n, size=test.size, replace=F)
  german.test <- germansvm[test,-1]; actual <- germansvm[test,1]
  german.train <- germansvm[-test,]
  svm.german <- svm(Default~.,data=german.train,gamma=0.026,cost=0.75)
  pred.svm <- predict(svm.german,german.test)
  error.rate.svm <- c(error.rate.svm, mean(actual != pred.svm))
}
boxplot(error.rate.svm,main="Misclassification rate (Support Vector Machine)")
mean(error.rate.svm) # 0.259802

### 3. Decision Tree

par(mfrow=c(1,1))
n <- dim(german)[1]
test.size <- round(2*n/5)
error.rate.tr <- NULL
for ( i in 1:5000) {
  test <- sample(1:n, size=test.size, replace=F)
  german.test <- german[test,-1]; actual <- german[test,1]
  german.train <- german[-test,]
  tr.german1 <- rpart(Default~.,data=german.train)
  tr.german2 <- prune(tr.german1,cp=0.046)
  pred.tr <- as.numeric(predict(tr.german2,german.test) >= 0.5)
  error.rate.tr <- c(error.rate.tr, mean(actual != pred.tr))
}
boxplot(error.rate.tr,main="Misclassification rate (Decision Tree)")
mean(error.rate.tr) # 0.310823

### 4. Logistic Regression Model

par(mfrow=c(1,1))
n <- dim(german)[1]
test.size <- round(2*n/5)
error.rate.glm <- NULL
for ( k in 1:5000) {
  test <- sample(1:n, size=test.size, replace=F)
  german.test <- german[test,-1]; actual <- german[test,1]
  german.train <- german[-test,]
  glm.german <- glm(Default~checkingstatus1+checkingstatus2+checkingstatus3+duration+history1+history2+history3+history4+others1+others2+otherplans1+otherplans2+foreign,data=german,family="binomial")
  pred.glm <- as.numeric(predict(glm.german, german.test, type="response") >= 0.5)
  error.rate.glm <- c(error.rate.glm, mean(actual != pred.glm))
}
boxplot(error.rate.glm,main="Misclassification rate (Logistic Regression Model)")
mean(error.rate.glm) # 0.245382

### 5. Linear Discriminant Analysis

par(mfrow=c(1,1))
n <- dim(german)[1]
test.size <- round(2*n/5)
error.rate.lda <- NULL
for ( k in 1:5000) {
  test <- sample(1:n, size=test.size, replace=F)
  german.test <- german[test,-1]; actual <- german[test,1]
  german.train <- german[-test,]
  lda.german <- lda(Default~.,data=german.train)
  pred.lda <- predict(lda.german, german.test)$class
  error.rate.lda <- c(error.rate.lda, mean(actual != pred.lda))
}
boxplot(error.rate.lda,main="Misclassification rate (Linear Discriminant Analysis)")
mean(error.rate.lda) # 0.2508126

#### Final Result
------------------
  
  # 5가지의 방법론들을 통해서 얻은
  # 오분류율의 평균값들을 정리해보면 다음과 같다.
  # K-nearest Neighbor : 0.2999408
  # Support Vector Machine : 0.259802
  # Decision Tree : 0.310823
  # Logistic Regression Model : 0.245382
  # Linear Discriminant Analysis : 0.2508126
  # 결과적으로 Logistic Regression Model을 적용했을 경우
  # 오분류율이 가장 작게 나왔으며,
  # 반면에 Decision Tree의 경우는
# 오분류율이 가장 크게 나왔다.
# 이 결과만을 이용한다면 Logistic Regression Model이
# 가장 바람직한 방법이라고 할 수 있을 것이다.
# 하지만 위의 5가지의 방법론들이
# 데이터가 속해있는 집단을 판별하는 데 사용할 수 있는
# 방법론의 전부는 아니다.
# 그 외에 Quadratic Discriminant Analysis(이차형판별분석),
# Semiparametric Logistic Regression Model(준모수적 로지스틱 회귀모형)
# Bagging, Boosting 등등
# 다양한 방법론들이 존재한다.
# 이러한 방법론들을 사용하여 오분류율을 계산할 경우
# 더 좋은 결과를 얻을 수도 있다.
# 이는 향후 연구에 남기도록 하겠다.