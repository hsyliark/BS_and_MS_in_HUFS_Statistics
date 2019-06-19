# Ex 3.14

height <- c(135,146,153,154,139,131,149,137,
            143,146,141,136,154,151,155,133,149,141,
            164,146,149,147,152,140,143,148,149,141,
            137,135)
weight <- c(26,33,54,32,25,44,44,31,36,35,
            28,28,36,48,36,31,34,32,47,37,46,36,47,
            33,42,32,32,29,34,30)

hist(height,freq=FALSE,main="키에 대한 히스토그램",
     ylab="상대도수",xlab="키(cm)",col="gray")
lines(density(height))

hist(weight,probability=TRUE,main="몸무게에 대한 히스토그램",
     ylab="상대도수",xlab="몸무게(kg)",col="gray")
lines(density(weight))



# Ex 3.15

x <- seq(from=0,to=2,by=0.1)*pi
y <- cos(x)

par(mfrow=c(2,2))
plot(x,y,type="b",pch="A",main="cosine graph")
plot(x,y,type="o",main="cosine graph")
plot(x,y,type="h",main="cosine graph")
plot(x,y,type="s",main="cosine graph")
