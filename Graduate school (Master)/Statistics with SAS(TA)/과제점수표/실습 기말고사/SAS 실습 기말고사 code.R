### SAS Statistics Lab Final examination



## Q1

Kim <- list(Korean.name="±èÇöÁø", English.name="Hyeon-jin", age=35,
            married=T, no.child=3, child.ages=c(9, 12, 15)) 
#1
Kim
str(Kim)
#2
Kim$English.name
Kim$no.child 
#3
Kim$child.ages[3] 
#4
is.list(Kim)



## Q2

A <- matrix(1:18, 3, 6, byrow=T)
#1
rownames(A) <- c("First", "Second", "Third")
colnames(A) <- c("Korea", "Japan", "USA", "Germany", "England", "Russia")
A
#2
apply(A, 2, mean)
#3
apply(A, 1, var)
#4
t(A)
A%*%t(A)



## Q3

library(MASS) 
data(Cars93)
View(Cars93)
#1
subset(Cars93, select=Make, subset=(AirBags == "Driver only" & Fuel.tank.capacity > 20))
#2
with(Cars93, summary(MPG.highway[Max.Price > 25]))
#3
cor(subset(Cars93, select=c(Min.Price, Price,	Max.Price, MPG.city, MPG.highway),
           subset=(Origin == "USA" | DriveTrain == "Front")))
#4
with(Cars93, tapply(Horsepower, DriveTrain, quantile, probs=c(0.35, 0.85)))



## Q4

n <- 0
sum.so.far <- 0
prod.so.far <- 1 
while (1) {
  if(sum.so.far > 50000 | prod.so.far > 100000) break 
  n <- n+1 
  sum.so.far <- sum.so.far+n
  prod.so.far <- prod.so.far*n 
} 
print(c(n, sum.so.far, prod.so.far))



## Q5

#1
s <- as.Date("2016-06-17")
t <- seq(from=s, by="5 months", length.out=10)
t
typeof(t)
#2
y <- -5:5
y
z <- ifelse(abs(y) > 2, 1, -1)
z
#3
x <- z[5]
if ( x == 1 ) {
  print("x == 1") 
  w <- "TRUE" 
} else {
  print("x != 1") 
  w <- "FALSE" 
}
w



## Q6

my.stdnorm <- function(x) {
  m <- mean(x); v <- var(x); s <- sd(x) 
  res <- list(m=m, v=v, s=s) 
  par(mfrow=c(1, 2)) 
  boxplot(x, main="Boxplot", horizontal=T) 
  hist(x, prob=T, col="skyblue", border="white", main="Histrogram", xlab="data")
  z <- seq(min(x), max(x), by=0.01)
  lines(z, dnorm(z), col="darkgreen", lwd=2)
  return(res) 
}
dat <- rnorm(1000, mean=0, sd=1) 
my.stdnorm(x=dat)