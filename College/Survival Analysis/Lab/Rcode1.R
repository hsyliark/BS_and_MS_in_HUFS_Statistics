install.packages('survival') # install survival package
library(survival) # loading survival package
# survival time
t<-c(6,6,6,7,10,13,16,22,23,6,9,10,11,17,19,20,25,32,32,34,35,1,1,2,2,3,4,4,5,5,8,8,8,8,11,11,12,12,15,17,22,23)
# censorship index
delta<-c(1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
# independent variable: group information
x1<-rep(c(0,1),c(21,21))
# covariate: log(WBC)
x2<-c(2.31,4.06,3.28,4.43,2.96,2.88,3.60,2.32,2.57,3.20,2.80,2.70,2.60,2.16,2.06,2.01,1.78,2.20,2.53,1.47,1.45,2.80,5.00,4.91,4.48,4.01,4.36,2.42,3.49,3.97,3.52,3.05,2.32,3.26,3.49,2.12,1.50,3.06,2.30,2.95,2.73,1.97)

# create 'survival' object using the function Surv()
remission<-Surv(t,delta)
remission
remission[x1==0] # for treatment group
remission[x1==1] # for placebo group