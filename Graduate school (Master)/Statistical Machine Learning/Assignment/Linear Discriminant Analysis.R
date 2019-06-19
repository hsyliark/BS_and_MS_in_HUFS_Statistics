### Linear Discriminant Analysis for p=1

G1.test <- rnorm(1000,2,1) ; G2.test <- rnorm(1000,-2,1)

my.LDA <- function(n1,n2,test1,test2) {
   ## Make training data 
   n1 <- n1 ; n2 <- n2  
   n <- n1+n2 
   k <- 2
   prior1 <- n1/n ; prior2 <- n2/n
   G1.train <- rnorm(n1,2,1) ; G2.train <- rnorm(n2,-2,1)
   mu1 <- mean(G1.train) ; mu2 <- mean(G2.train)
   sigma2 <- sum((G1.train-mu1)^2+(G2.train-mu2)^2)/(n-k)

   ## Make test data
   test <- c(test1,test2)
   true.group <- c(rep("Group1",1000),rep("Group2",1000))

   ## Decision boundary of LDA when k=2
   delta1 <- function(x) x*(mu1/sigma2)-(((mu1^2)/sigma2)/2)+log(prior1)
   delta2 <- function(x) x*(mu2/sigma2)-(((mu2^2)/sigma2)/2)+log(prior2)
   d1 <- delta1(test) ; d2 <- delta2(test)
   boundary <- d1-d2
   predict.group <- rep("Group1",2000)
   predict.group[boundary < 0] <- "Group2"

   ## Drawing some graph
   par(mfrow=c(1,2))
   # Picture 1
   x <- seq(-5,5,0.001)
   y1 <- dnorm(x,2,1) ; y2 <- dnorm(x,-2,1)
   plot(x,y1,main="Normal distributions for two groups",type="n",xlab="x",ylab="y")
   lines(x,y1,col="hotpink",lwd=2,lty=1)
   lines(x,y2,col="darkgreen",lwd=2,lty=1)
   abline(v=0,lty=2,lwd=2)
   # Picture 2
   hist(G2.train,col="darkgreen",breaks=12,xlim=c(-5,5),prob=TRUE,main="",xlab="",ylab="")
   par(new=TRUE)
   hist(G1.train,col="hotpink",density=25,breaks=12,angle=45,xlim=c(-5,5),
        prob=TRUE,main="Result of Linear Discriminant Analysis",
        xlab="train",ylab="density",axes=FALSE)
   abline(v=0,lty=2,lwd=2)
   abline(v=((mu1+mu2)/2)+(sigma2/(mu1-mu2))*log(prior2/prior1),lty=1,lwd=2)

   ## Classification result
   res <- table(predict.group,true.group)
   miss <- mean(predict.group!=true.group) # Misclassificaion rate
   cat("\n","**Confusion matrix**")
   cat("\n","( When n1 =",n1,"and n2 =",n2,")","\n")
   print(res) 
   cat("\n","Misclassification rate is",miss,".")
   cat("\n","And LDA decision boundary estimated from the training data is",
       ((mu1+mu2)/2)+(sigma2/(mu1-mu2))*log(prior2/prior1),".")
}

## Checking result
my.LDA(20,20,G1.test,G2.test)
my.LDA(50,50,G1.test,G2.test)
my.LDA(100,100,G1.test,G2.test)
my.LDA(500,500,G1.test,G2.test)
my.LDA(1000,1000,G1.test,G2.test)
my.LDA(1500,1500,G1.test,G2.test)
my.LDA(2000,2000,G1.test,G2.test)