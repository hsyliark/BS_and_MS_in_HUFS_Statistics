### Find value M and target(Y ~ N(0,1)) and candidate(V ~ f(v))
### U ~ Uniform(0,1)
### V ~ Cauchy(0,1) , V ~ Double Exponential(0,1)



## (a) Cauchy distribution

#1 Find M

y1 <- seq(-1000,1000,by=0.001)
grid.m1 <- dnorm(y1,0,1)/dcauchy(y1,0,1)
(M1 <- grid.m1[which.max(grid.m1)])

#2 Accept/Reject Algorithm

u1 <- runif(length(y1),0,1) ; v1 <- rcauchy(length(y1),0,1)
condition1 <- dnorm(v1,0,1)/(M1*dcauchy(v1,0,1))
select.y1 <- ifelse(u1 < condition1,TRUE,FALSE)

 
## (b) Double Exponential distribution

install.packages("smoothmest")
library(smoothmest)

#1 Find M

y2 <- seq(-1000,1000,by=0.001)
grid.m2 <- dnorm(y2,0,1)/ddoublex(y2,0,1)
(M2 <- grid.m2[which.max(grid.m2)])

#2 Accept/Reject Algorithm

u2 <- runif(length(y2),0,1) ; v2 <- rdoublex(length(y2),0,1)
condition2 <- dnorm(v2,0,1)/(M2*ddoublex(v2,0,1))
select.y2 <- ifelse(u2 < condition2,TRUE,FALSE)


## (c) Compare two results

par(mfrow=c(1,2))

random.y1 <- v1[select.y1] 
hist(random.y1,prob=TRUE,nclass=20,
     main="Accept/Reject Algorithm Y~N(0,1),V~Cauchy(0,1)",col="green")
lines(y1,dnorm(y1,0,1),col="red",lwd=2)

random.y2 <- v2[select.y2] 
hist(random.y2,prob=TRUE,nclass=20,
     main="Accept/Reject Algorithm Y~N(0,1),V~Double Exponential(0,1)",col="yellow")
lines(y2,dnorm(y2,0,1),col="blue",lwd=2)

print(list(mean(random.y1),abs(mean(random.y1)-0),sd(random.y1)))
print(list(mean(random.y2),abs(mean(random.y2)-0),sd(random.y2)))




