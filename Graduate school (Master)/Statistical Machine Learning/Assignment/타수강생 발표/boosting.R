
library(data.table)
library(psych)
library(ggplot2)
library(reshape2)

## data.handle
auto <- fread("Auto.csv")
str(auto)
which(auto$horsepower=='?')
auto[which(auto$horsepower=='?')]

auto <- auto[!which(auto$horsepower=='?')]
str(auto)

auto$horsepower <- as.numeric(auto$horsepower)
str(auto)

auto$mpg <- scale(auto$mpg)
auto$cylinders <- scale(auto$cylinders)
auto$displacement <- scale(auto$displacement)
auto$horsepower <- scale(auto$horsepower)
auto$weight <- scale(auto$weight)
auto$acceleration <- scale(auto$acceleration)
auto$year <- scale(auto$year)
auto$origin <- scale(auto$origin)


## end

lamda=0.0001
## start boosting


y.hat <- numeric(nrow(auto))
resi <-auto$mpg - y.hat
xnam <-names(auto)[2:8]
beta <- auto[1,2:8,with=F]
beta[,1:7] <- 0

#identical(resi,auto$mpg)
# 27 - 217
# 28 - 11211
# 29 - 11237
while(sum(resi)^2 > 1e-34){
corr <- abs(cor(auto[,2:8,with=F],resi))
fmla <- as.formula(paste("resi ~ ", paste(xnam[which.max(corr)]),"-1"))


reg <- lm(fmla,data=auto)
y.hat.b <- reg[[1]]*auto[[which.max(corr)+1]]

resi <- resi-(y.hat.b*lamda)
y.hat <- y.hat + (y.hat.b*lamda)

beta.b <- auto[1,2:8,with=F]
beta.b[,1:7] <- 0
beta.b[,which.max(corr)] <-reg[[1]] 
beta <- rbind(beta,beta.b)
if(nrow(beta)==50000){
  break
}
}

beta <- cumsum(beta)

g <- ggplot(data=beta,aes(c(1:nrow(beta))))
g+geom_line(aes(y=weight,colour="1")) + geom_line(aes(y=year,colour="2")) +
geom_line(aes(y=cylinders,colour="3"))+geom_line(aes(y=displacement,colour="4"))+
geom_line(aes(y=horsepower,colour="5"))+geom_line(aes(y=acceleration,colour="7"))+geom_line(aes(y=origin,colour="6"))


  
  
library(glmnet)
x=model.matrix(mpg~.,auto[,1:8,with=F])[,-1]
y=auto$mpg

lasso.mod<-glmnet(x,y,alpha=1)
plot(lasso.mod)


