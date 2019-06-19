n <- 1000
sig <- 2
x <- rnorm(n,0,2)

B <- 10000
boot<-NULL
for(b in 1:B){
  boot.x <- x[sample(1:length(x),length(x),replace=T)]
  boot <- c(boot,mean(boot.x))
}


sd(boot)
sig/sqrt(n)