#* 교재 637p 이후에 패키지나 코드 설명O *#

# * 데이터 설명 * #
 #- 약물 중독 치료소에서 나올 때까지의 시간
 #- clinic : 치료소 종류
 #- time : 일 기준
 #- prison : 감옥에 갔는지 안 갔는지
 #- dose : 처방받은 약


addicts <- read.table(file.choose(), head=T)
library(survival)

attach(addicts)
Y = Surv(time, censor)   # time에 censoring 여부도 같이 붙여줌


#### KM curve
(KM = survfit(Y~1))   # 공변량이 없을 때의 카플란-마이어 (1: 공변량 없음)
ls(KM)
summary(KM)
plot(KM)   # 신뢰구간도 구해줌
plot(KM$time, KM$surv)   # "plot(KM)"과 같음 (과연?)


### comparing survival curves for clinic
KM1 = survfit(Y ~ clinic)
ls(KM1)
summary(KM1)
plot(KM1, lty=c(1, 2), col=c(1, 2))
 #- 검정선에 속한 그룹(Clinic 1)은 치료소에서 빨리 나간다
survdiff(Y~clinic)   # logrank test for clinic
 #- H0 : 두 그룹의 커브는 차이가 없다   vs   H1 : 두 그룹의 커브는 차이가 있다
 #- 결과에서 p < 0.05이므로 귀무가설을 기각한다
 #- 두 그룹의 커브는 서로 다르다고 할 근거가 있다   (대립가설 위주로 결과 해석)


### ph model for 3 variables
ph = coxph(Y ~ clinic + prison + dose)
summary(ph)
 #- prison 변수는 p > 0.05라서 유의하지 않음
 #- hazard가 낮다는 것은 빨리 나간다는 뜻?   (hazard 식, 코드, 결과 복습)


### LRT test   → 개념, 식 복습하기
ph1 = coxph(Y ~ clinic + prison + dose + clinic*dose + clinic*prison)
anova(ph, ph1)
 #- H0 :    vs   H1 : 
 #- ~이어서 귀무가설을 기각한다/하지 않는다
 #- 공변량이 없는 모델1을 선택


### PH assumption for dose   → PH 가정 왜 확인하는지 복습
new = data.frame(clinic0 = 1.5, prison0 = 0.5)   # 각각 평균 넣음
I1 = (dose<50); I2 = (dose>=50 & dose<80); I3 = (dose>=80)
clinic0 = clinic[I1]; prison0 = prison[I1]   # dose<50인 데이터에서 clinic, prison 값 추출
ph2_1 = coxph(Y[I1] ~ clinic0 + prison0)   # dose<50인 그룹의 coxph
plot(survfit(ph2_1, newdata = new), conf.int = F, fun="cloglog", ylim=c(-6, 1), xlim=c(2, 1200))
#plot(survfit(ph2_1,newdata=new),conf.int=F,xlim=c(0,1200))
#par(new=T)
clinic0 = clinic[I2]; prison0 = prison[I2]
ph2_2 = coxph(Y[I2] ~ clinic0 + prison0)
lines(survfit(ph2_2, newdata = new), conf.int=F, fun = "cloglog")
#plot(survfit(ph2_2,newdata=new),conf.int=F,xlim=c(0,1200))
#par(new=T)
clinic0 = clinic[I3]; prison0 = prison[I3]
ph2_3 = coxph(Y[I3] ~ clinic0 + prison0)
lines(survfit(ph2_3, newdata = new), conf.int = F, fun = "cloglog")
#plot(survfit(ph2_3,newdata=new),conf.int=F,xlim=c(0,1200))


### PH assumption for clinic
new=data.frame(dose0=mean(dose),prison0=0.5)
I1 = (clinic==1) ;I2 = (clinic==2)
dose0=dose[I1];prison0=prison[I1]
ph3_1 = coxph(Y[I1]~dose0+prison0)
plot(survfit(ph3_1,newdata=new),conf.int=F,fun="cloglog")
dose0=dose[I2];prison0=prison[I2]
ph3_2 = coxph(Y[I2]~dose0+prison0)
lines(survfit(ph3_2,newdata=new),conf.int=F,fun="cloglog")


### PH assumption for prison
new=data.frame(dose0=mean(dose),clinic0=0.5)
I1 = (prison==0) ;I2 = (prison==1)
clinic0=clinic[I1];dose0=dose[I1]
ph4_1 = coxph(Y[I1]~clinic0+dose0)
plot(survfit(ph4_1,newdata=new),conf.int=F,fun="cloglog",xlim=c(5,1000),ylim=c(-5,2))
clinic0=clinic[I2];dose0=dose[I2]
ph4_2 = coxph(Y[I2]~clinic0+dose0)
lines(survfit(ph4_2,newdata=new),conf.int=F,fun="cloglog")


### PH assumption test
cox.zph(ph, transform=rank)   # ph = coxph(Y ~ clinic + prison + dose)
 #- H0 : PH가정을 만족한다   vs   H1 : PH가정을 만족하지 않는다
 #- p-value만 보면 됨   → 그래서 결과가 무엇인지 복습하기
 #- cox.zph(ph) vs +rank option 차이 확인


# ovarian data (난소암 환자들의 생존시간)
 #- 1. KM plot + C.I.
 #- 2. rx에 따라 차이?
      #- i) KM curves
      #- ii) log-rank test
 #- 3. Cox model with 3 covariates 해석
 #- 4. PH Assumption
      #- Graph
      #- test
