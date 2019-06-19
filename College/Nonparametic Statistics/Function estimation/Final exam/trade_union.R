library(SemiPar)

data(trade.union)
head(trade.union)
tail(trade.union)

dat <- trade.union[-171,]
attach(dat)

white <- (race == 3)

fit.glm <- glm(union.member~wage,family="binomial")
summary(fit.glm)

new.data <- data.frame(wage=5)
predict(fit.glm,new.data)
predict(fit.glm,new.data,type="response")

fit.spm <- spm(union.member~f(wage),family="binomial")
summary(fit.spm)
plot(fit.spm)

predict(fit.spm,new.data)
lgp <- predict(fit.spm,new.data)
exp(lgp)/(1+exp(lgp))

fit <- spm(union.member~f(wage)+years.educ+age+female+white+south,family="binomial")
par(mfrow=c(2,3))
plot(fit)
summary(fit)

new.data <- data.frame(wage=5,years.educ=15,age=24,female=1,white=0,south=0)
lgp <- predict(fit,new.data)
exp(lgp)/(1+exp(lgp))

fit <- spm(union.member~f(wage)+female+white,family="binomial")
par(mfrow=c(1,3))
plot(fit)
summary(fit)

new.data <- data.frame(wage=5,female=1,white=0)
lgp <- predict(fit,new.data)
exp(lgp)/(1+exp(lgp))
