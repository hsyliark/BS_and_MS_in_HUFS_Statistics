
library(mvtnorm)
mean=c(0,0)
sigma1=matrix(c(1,0,0,1),2,2)
sigma2=matrix(c(1,0.7,0.7,1),2,2)

g1=g2=seq(-3,3,0.05)
g=c(outer(g1,g2))
d1=d2=matrix(0,length(g1),length(g2))

for (i in 1:length(g1))
{
for (j in 1:length(g2))
{
d1[i,j]=dmvnorm(c(g1[i],g2[j]), mean, sigma1)
}
}

for (i in 1:length(g1))
{
for (j in 1:length(g2))
{
d2[i,j]=dmvnorm(c(g1[i],g2[j]), mean, sigma2)
}
}



par(mfrow=c(1,2))

library(plot3D)

persp3D(g1,g2,d1,theta=30,phi=30,colkey = TRUE,expand=0.5)
persp3D(g1,g2,d2,theta=30,phi=30,colkey = TRUE,expand=0.5)

persp3D(g1,g2,d1,theta=0,phi=90,colkey = TRUE,expand=0.5)
persp3D(g1,g2,d2,theta=0,phi=90,colkey = TRUE,expand=0.5)
	
persp3D(g1,g2,d1,theta=0,phi=0,colkey = TRUE,expand=0.5)
persp3D(g1,g2,d2,theta=0,phi=0,colkey = TRUE,expand=0.5)

persp3D(g1,g2,d1,theta=90,phi=0,colkey = TRUE,expand=0.5)
persp3D(g1,g2,d2,theta=90,phi=0,colkey = TRUE,expand=0.5)

image(d1)
image(d2)
