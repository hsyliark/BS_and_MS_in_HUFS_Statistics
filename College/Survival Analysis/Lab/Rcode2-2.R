### Rcode2-2.r

library(survival) # loading survival package

vet.dat<-read.table('vet.dat',sep='\t',header=TRUE)
#vet.dat<-read.table(file.choose(),sep='\t',header=TRUE)
head(vet.dat)

vet<-Surv(vet.dat[,6],vet.dat[,11])

grp1<-which(vet.dat[,7]<60)
grp2<-which(vet.dat[,7]>59 & vet.dat[7]<75)
grp3<-which(vet.dat[,7]>74)
length(grp1)
length(grp2)
length(grp3)

grp<-rep(1,137)
grp[grp2]<-2
grp[grp3]<-3
res<-survfit(vet~grp)
plot(res,main='KM curves for performance status groups', lty=1:3, col=1:3, lwd=2, cex=2)
legend(750,1,c('Group 1', 'Group 2', 'Group 3'), lty=1:3, col=1:3, lwd=2)
res2<-survdiff(vet~grp)
res2

