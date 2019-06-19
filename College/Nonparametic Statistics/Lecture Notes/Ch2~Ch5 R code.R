################## Ch 2 : one sample location ######################

#### mileage data 

mileage = c(14,15,13,17,16,16,15,25,14,15)

# t-test under normality assumption 
t.test(mileage,alternative = "less",mu=17)
t.test(mileage-17,alternative = "two.sided")

# sign test
x = mileage[-(which(mileage==17))] # delete ties
sn = sum(x>17)				# compute test statistic
pbinom(sn,length(x),0.5)		# compute p-value

Lsn = (sn - length(x)/2)/sqrt(length(x)/4)	# large sample approximation
pnorm(Lsn)
	
binom.test(sn,n=length(x),p=0.5,alternative="less")	# R-function

# Wilcoxon signed rank test
r = rank(abs(x-17))				# rank of | x_i - m_0 |
wn = sum(r*(x>17))				# compute test statistic

Lwn = (wn - length(x)*(length(x)+1)/4)/sqrt(length(x)*(length(x)+1)*(2*length(x)+1)/24)
pnorm(Lwn)							# large sample approximation

wilcox.test(x,mu=17,alternative="less")				# R-function

################## Ch 3 : two sample location ######################

##### weight data
x = c(5.7,7.3,7.6,6.0,6.5,5.9)
y = c(4.9,7.4,5.3,4.6)

# two sample t-test with equal variances
t.test(x,y,alternative = "two.sided",var.equal=TRUE)

# Wilcoxon rank sum test
z = c(x,y) ; I = c(x*0,y/y)		# pooled sample & index variable
Wn = sum(rank(z)*I)			# compute test statistic

m = length(x) ; n = length(y)		# lengths of variables

LWn = (Wn - n*(m+n+1)/2) / sqrt(m*n*(m+n+1)/12) # large sample approximation
2*pnorm(LWn)

wilcox.test(x,y,exact = TRUE)		# R-function

# Note that this function returns "m*n - Mann-Whitney statistic"


# Mann-Whitney test
M = outer(x,y,"-") 			# compute differences of alll combinations
Umn = sum(M<0)				# test statistic

LUmn = (Umn - m*n/2) / sqrt(m*n*(m+n+1)/12)	# large sample approximation
2*pnorm(LUWn)


################# Ch 4 : One-way ANOVA ######################

# wire data
A = c(56,60,57,64)
B = c(48,61,49,53)
C = c(52,50,44,46)

# Parametric ANOVA
str = c(A,B,C)
machine = c(rep("A",length(A)),rep("B",length(B)),rep("C",length(C)))
dataex = data.frame(str,machine)
summary(aov(str ~ machine,data=dataex))

# Kruskal-Wallis test
PS = c(A,B,C) ; IA = c(A/A,B*0,C*0) ; IB = c(A*0,B/B,C*0) ; IC = c(A*0,B*0,C/C)	# pooled sample & index variable

rPS = rank(PS)
rA = rPS[IA==1]				# rank of A in the pooled sample
rB = rPS[IB==1]				# rank of B in the pooled sample
rC = rPS[IC==1]				# rank of C in the pooled sample

rstr = c(rA,rB,rC)
rdataex = data.frame(rstr,machine)
summary(aov(rstr ~ machine,data=rdataex))
KW = (length(PS)-1)*78.5/(78.5+64.5)		# test statistic
KW = 12/(length(PS)*(length(PS)+1)) * (sum(rA)^2/length(A)+sum(rB)^2/length(B)+sum(rC)^2/length(C)) - 3*(length(PS)+1) 
1-pchisq(KW,3-1)						# p-value

kruskal.test(str ~ machine,data=dataex)		# R-function


################# Ch 5 : correlation analysis ######################

# score data
eng = c(56,75,45,71,61,64,58,80,76,62)
math = c(66,70,40,60,65,56,59,77,67,63)
n = length(eng)

plot(eng,math,main="Scores",pch=16)			# plotting

# Pearson
cor.test(eng,math)

#Spearman
reng = rank(eng)
rmath = rank(math)	
sp = cor(reng,rmath)					# Spearman by method 1
sp = 1 - 6/(n*(n-1)*(n+1))*sum((reng-rmath)^2)	# Spearman by method 2

Lsp = sp/sqrt(1/(n-1))					# large sample approximation

cor.test(eng,math,method="spearman")		# R-function	

#Kendall
Ieng = order(eng)					# extract index by "eng"
neweng = eng[Ieng]
newmath = math[Ieng]
CD = outer(newmath,newmath,"-")
newCD = CD*upper.tri(CD)
C = sum(newCD<0)					# concordant pair
D = sum(newCD>0)					# discordant pair
tau = (C-D)/(C+D)					# Kendall

Ltau = tau/sqrt((4*n+10)/(9*n*(n-1)))					# large sample approximation

cor.test(eng,math,method="kendall")		# R-function



