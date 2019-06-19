#### Input data

german <- read.csv("D:/수업자료 (대학, 대학원)/대학원/석사졸업논문/논문주제선정/Kernel logistic/R simulation/4번째/germancredit.csv",sep=",",header=T)

attributes(german)





### Logistic regression using glm() function

german.glm <- glm(Default~.,data=german,family="binomial")
summary(german.glm)
attributes(german.glm)

german.coef <- german.glm$coefficients
german.pred <- as.numeric(predict(german.glm,german,type="response") >= 0.5)



### Calculate coefficients using theory


## Maximum Likelihood Estimation + Newton-Raphson algorithm
## Iteratively Reweighted Least Squares (IRLS)

beta0 <- lm(Default~.,data=german)$coefficients # initial value

y <- as.matrix(german[,1])
X <- cbind(matrix(rep(1,length(y)),length(y),1),as.matrix(german[,-1]))


my.logistic <- function(y,X,k,s) {
  
  y <- as.matrix(y) ; X <-as.matrix(X)
  
  B <- as.matrix(s)
  
  for (j in 1:k) {
    p <- matrix(NA,nrow(X),1)
    p1 <- matrix(NA,nrow(X),1)
    
    C <- (X)%*%B[,j]
    
    my.prob1 <- function(m) exp(m)/(1+exp(m))
    my.prob2 <- function(m) (exp(m)/(1+exp(m)))*(1-exp(m)/(1+exp(m)))
    
    p <- my.prob1(C)
    p1 <- my.prob2(C)
    
    W <- diag(as.vector(p1))
    
    new.b <- B[,j]+solve(t(X)%*%(W)%*%(X))%*%(t(X))%*%(y-p)
    B <- cbind(B,new.b)
    cat(j,",")
  }
  
  cat("\n","Algorithm converged...")
  
  beta.hat <- B[,k]
  
  logit.hat <- X%*%beta.hat
  pi.hat <- exp(logit.hat)/(1+exp(logit.hat))
  y.pred <- rep(1,length(logit.hat))
  y.pred[pi.hat < 0.5] <- 0
  
  res <- list(beta.hat=beta.hat,y.pred=y.pred)
}


g <- my.logistic(y,X,100,beta0)


## Checking result
summary1 <- cbind(g$beta.hat,german.coef)
summary2 <- cbind(y,g$y.pred,german.pred)

sum(g$y.pred != german.pred)

mean(y != g$y.pred) # misclassification rate

summary1







