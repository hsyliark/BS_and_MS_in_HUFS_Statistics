### 6.5 Lab 1 : Subset Selection Methods

## 6.5.1 Best Subset Selection

library(ISLR)              # 패키지 불러오기
fix(Hitters)               # 데이터 편집
names(Hitters)             # 변수확인
dim(Hitters)               # 데이터의 크기 확인
sum(is.na(Hitters$Salary)) # 결측치이면 TRUE 아니면 FALSE, 59라는 결과는 Salary 변수에 결측치가 59개 있다

Hitters <- na.omit(Hitters) # 결측치가 포함된 player들의 데이터를 지워 버림
dim(Hitters)                # 결측치 제외 후 데이터의 크기 확인
sum(is.na(Hitters))         # 결측치 없음을 확인

library(leaps)             # Best Subset Selection을 하기위한 패키지 불러오기
regfit.full <- regsubsets(Salary ~ ., Hitters)  # RSS를 사용하여 최적의 모델 선택
summary(regfit.full)       

regfit.full <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
reg.summary <- summary(regfit.full)
names(reg.summary) # reg.summary에 있는 값들 확인
reg.summary$rsq    # 32% ~ 54.6% 까지 R2가 증가하는 것을 확인

par(mfrow = c(2, 2)) # 그림을 2행 2열로 그림
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l") # reg.summary에서 rss를 사용하여 그래프를 그림
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l") # reg.summary에서 adjr2를 사용하여 그래프를 그림
which.max(reg.summary$adjr2) # adjr2의 값 중 가장 큰 값이 몇번째에 있는지 
points(11, reg.summary$adjr2[11], col = 'red', cex = 2, pch = 20) # 그 점을 그래프에 빨간색점으로 표시

plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l") # reg.summary에서 cp를 사용하여 그래프를 그림 
which.min(reg.summary$cp) # 가장 작은 cp 값이 몇번째에 있는지
points(10, reg.summary$cp[10], col = 'red', cex = 2, pch = 20) # 그 점을 그래프에 표시
which.min(reg.summary$bic) # 가장 작은 bic 값이 몇번째에 있는지
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l") # reg.summary에서 bic를 사용하여 그래프를 그림 
points(6, reg.summary$bic[6], col = 'red', cex = 2, pch = 20) # 가장 작은 bic를 그래프에 표시

plot(regfit.full, scale = "r2")    
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")   # 가장 위에 검정색 사각형, 6개의 변수(AtBat, Hits, Walks, CRBI, DivisionW, PutOuts)를 선택  

coef(regfit.full, 6)   # bic로 선택된 변수들로 만든 최적의 모형 회귀계수 확인

## 6.5.2 Forward and Backward Stepwise Selection

regfit.fwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "forward") # forward stepwise selection
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "backward") # backward stepwise selection
summary(regfit.bwd)

coef(regfit.full, 7) 
coef(regfit.fwd, 7) 
coef(regfit.bwd, 7) # 선택되는 변수들과 회귀계수가 다르다.


## 6.5.3 Choosing Among Models Using the Validation Set Approach and Cross-Validation

set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters), rep = TRUE) # Hitters의 행의 개수 만큼 복원추출
test <- (!train) # train이 아닌 나머지를 test

regfit.best <- regsubsets(Salary ~ ., data = Hitters[train,], nvmax = 19) # Hitters에서 TRUE에 해당하는 데이터로 regsubsets 
test.mat <- model.matrix(Salary ~ ., data = Hitters[test,]) # X matrix

val.errors <- rep(NA, 19)    # test MSE를 계산하기 위한 빈자리 생성
for(i in 1:19) {
  coefi <- coef(regfit.best, id = i)  # i = 1, 2, ..., 19 넣어서 회귀계수 구함
  pred <- test.mat[,names(coefi)]%*%coefi
  val.errors[i] <- mean((Hitters$Salary[test]-pred)^2) # test MSE 구함
}
val.errors # 결과
which.min(val.errors) # 가장 작은 test MSE 값이 몇번째인지
coef(regfit.best, 10) # 최적의 모델의 회귀계수

predict.regsubsets <- function(object, newdata, id,...){  # regsubsets을 위한 predict함수가 없기 때문에 
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19) # full data set
coef(regfit.best, 10) # nvmax = 19 일 때 최적의 모델의 회귀계수 


k <- 10
set.seed(1)
folds <- sample(1:k, nrow(Hitters), replace = TRUE)  # 1~10까지 Hitters의 행의 개수만큼 복원추출
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19))) # cv.error 값을 구한 후 채워넣기 위한 matrix 생성

for(j in 1:k) {
  best.fit <- regsubsets(Salary ~ ., data = Hitters[folds!=j,], nvmax = 19) # folds = j이면 test set 나머지 training set
  for(i in 1:19) {
    pred <- predict(best.fit, Hitters[folds==j,], id = i) # 앞에서 만든 predict.regsubsets 이용하여 best.fit에 대한 predict 구함
    cv.errors[j,i] <- mean((Hitters$Salary[folds==j]-pred)^2) # test error 계산 후 앞에 생성한 matrix에 채움
  }
}


mean.cv.errors <- apply(cv.errors, 2, mean) # matrix 열에 대하여 평균
mean.cv.errors

par(mfrow = c(1,1))
plot(mean.cv.errors, type = 'b')   # ㅡmean.cv.errors 그래프, 11개의 변수 선택

reg.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19) # full data set
coef(reg.best, 11) # 그래프에서 변수 11개일 때 가장 낮은 mean.cv.errors값을 가지므로 11개에 대한 회귀계수 추정


### 6.6 Lab2 : Ridge Regression and the Lasso

x <- model.matrix(Salary ~ ., Hitters)[,-1] # Salary변수를 뺀 나머지 변수들로 X matrix 생성, 질적자료를 dummy 변수로 변경
y <- Hitters$Salary                         # y vector


## 6.6.1 Ridge Regression

library(glmnet)         # Ridge Regression과 Lasso를 실행하기 위한 패키지 불러오기
grid <- 10^seq(10, -2, length = 100)  # grid 생성
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid) # alpha = 0 ridge regression

dim(coef(ridge.mod))  # 각 lambda에 대한 회귀계수 matrix 크기

ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))  # l2 norm

ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))  # l2 norm

predict(ridge.mod, s = 50, type = 'coefficients')[1:20,] # lambda = 50일 때, ridge regression의 회귀계수

set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2) # Ridge, lasso의 test error를 추정하기 위해 
test <- (-train)
y.test <- y[test]

ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12) # x = matrix, y = response variable
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test,]) # newx 새로운 데이터
mean((ridge.pred - y.test)^2)  # test MSE

mean((mean(y[train]) - y.test)^2) # test MSE

ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test,]) # lambda를 가장 큰 값으로 했을 때 
mean((ridge.pred - y.test)^2) # test MSE

ridge.pred <- predict(ridge.mod, s = 0, newx = x[test,], exact = T) # lambda = 0이면 lm과 같다.
mean((ridge.pred - y.test)^2)
lm(y ~ x, subset = train)
predict(ridge.mod, s = 0, exact = T, type = "coefficients")[1:20,]

set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0) # default 10-folds cross-validation
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam  # 가장 작은 cross-validation error의 값을 주는 lambda

ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,]) # best lambda를 사용했을 때
mean((ridge.pred - y.test)^2) # test MSE

out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20,] # best lambda로 회귀계수 추정


## 6.6.2 The Lasso

lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred - y.test)^2)

out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:20,]
lasso.coef # Ridge와 달리 7개의 변수를 선택


### Lab3 : PCR and PLS Regression

## 6.7.1 Principal Components Regression

library(pls) # 패키지 불러오기
set.seed(2)
pcr.fit <- pcr(Salary ~ ., data = Hitters, scale = TRUE, validation = "CV") # scale = T 표준화, 10-fold cross validation error 계산
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP") # val.type = "MSEP" cross-validation MSE 값으로 plot그림

set.seed(1)
pcr.fit <- pcr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = "CV") # training data 사용하여 PCR
validationplot(pcr.fit, val.type = "MSEP")

pcr.pred <- predict(pcr.fit, x[test,], ncomp = 7) # training data에서 CV 7일 때 가장 작으므로
mean((pcr.pred - y.test)^2) # test MSE

pcr.fit <- pcr(y ~ x, scale = TRUE, ncomp = 7) # full data set
summary(pcr.fit)


## 6.7.2 Partial Least Squares

set.seed(1)
pls.fit <- plsr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")

pls.pred <- predict(pls.fit, x[test,], ncomp = 2)
mean((pls.pred - y.test)^2)

pls.fit <- plsr(Salary ~ ., data = Hitters, scale = TRUE, ncomp = 2)
summary(pls.fit)

