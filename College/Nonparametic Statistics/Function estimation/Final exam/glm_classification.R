dat <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
dim(dat)
head(dat)
tail(dat)

library(SemiPar)
attach(dat)
spm.o <- spm(admit~f(gre)+f(gpa), family="binomial")
par(mfrow=c(1,2))
plot(spm.o)

glm.o <- glm(admit~gre+gpa, family="binomial", data=dat)
summary(glm.o)

############
# Evaluation of the prediction model

n <- dim(dat)[1]
test.size <- round(n/4)
error.rate.glm <- NULL
for ( k in 1:1000) {
  test <- sample(1:n, size=test.size, replace=F)
  dat.test <- dat[test,-1]; actual <- dat[test,1]
  dat.train <- dat[-test,]
  glm.o <- glm(admit~gre+gpa, family="binomial", data=dat.train)
  pred.glm <- (predict(glm.o, dat.test, type="response") >= 0.5)
  error.rate.glm <- c(error.rate.glm, mean(actual != pred.glm))
}
boxplot(error.rate.glm)
mean(error.rate.glm)


