# https://www.umass.edu/statdata/statdata/data/pharynx.txt 설명 참고


#### 데이터 속성, 특성 확인 ####
pharynx <- read.csv('pharynx.csv', head=T)   # 추후 필요시 몇몇 변수 Factor 처리
head(pharynx); tail(pharynx)
str(pharynx)   # 데이터 195개, 13개 변수


#### 이상치, 결측치 제거 ####
which(pharynx$GRADE == 9)   # missing value
which(pharynx$COND == 9)   # missing value
which(pharynx$COND == 0)   # outlier

pharynx <- read.csv('pharynx_handling.csv', head=T)   # 추후 필요시 몇몇 변수 Factor 처리


#### 최소, 최대값 확인 ####
summary(pharynx[, c(6, 13)])
boxplot(pharynx$AGE)   # 20대 환자 1명, 50~70대 환자가 제1~3분위수
boxplot(pharynx$TIME)   # 생존시간이 237~783일인 환자가 제1~3분위수 (정말 오래 사는 경우는 1500일 넘음)


#### 연령대 변수 생성 ####
pharynx$AGE_GRP[pharynx$AGE %in% 20:29] <- '20-29'
pharynx$AGE_GRP[pharynx$AGE %in% 30:39] <- '30-39'
pharynx$AGE_GRP[pharynx$AGE %in% 40:49] <- '40-49'
pharynx$AGE_GRP[pharynx$AGE %in% 50:59] <- '50-59'
pharynx$AGE_GRP[pharynx$AGE %in% 60:69] <- '60-69'
pharynx$AGE_GRP[pharynx$AGE %in% 70:79] <- '70-79'
pharynx$AGE_GRP[pharynx$AGE %in% 80:89] <- '80-89'
pharynx$AGE_GRP[pharynx$AGE %in% 90:99] <- '90-99'


#### 빈도 확인 ####
table(pharynx$INST)
table(pharynx$SEX)   # 남자 147명, 여자 46명
table(pharynx$TX)   # placebo group 98명, 방사선 치료 그룹 95명
table(pharynx$GRADE)
table(pharynx$AGE_GRP)   # 20대 1명, 30대 3명, 40대 31명, 50대 57명, 60대 64명, 70대 30명, 80대 5명, 90대 1명
table(pharynx$COND)   # 3(requires assistance with self care), 4(bed confined)의 개수가 부족함
table(pharynx$SITE)
table(pharynx$T_STAGE)   # 1(primary tumor measuring 2 cm or less in largest diameter)의 개수가 부족함
table(pharynx$N_STAGE)


#### time, censoring 붙여주기####
library(survival)
attach(pharynx)
Y = Surv(TIME, STATUS)


#### KM curve ####
(KM = survfit(Y~1))   # 공변량이 없을 때의 카플란-마이어
summary(KM)
plot(KM)   # 색깔 입히기, 50% 위치 선 긋기? (아래 내용 확인 후 필요할 시)


#### PH assumption test ####
  # 전연령대(AGE)
ph_2090age = coxph(Y ~ INST + TX + GRADE + AGE + COND + T_STAGE + N_STAGE)
summary(ph_2090age)   # COND, T_STAGE 유의
cox.zph(ph_2090age, transform=rank)   # 유의한 것 없음

  # 40~70대(AGE)
ph_4070age = coxph(Y ~ INST + TX + GRADE + AGE + COND + T_STAGE + N_STAGE, subset=(AGE %in% 40:79))
summary(ph_4070age)   # COND, T_STAGE 유의
cox.zph(ph_4070age, transform=rank)   # COND p-value > 0.05 (cond=3인 데이터의 갯수가 충분치 않아 그런 것으로 추정)


#### survival curves 비교 ####
  # INST (그룹이 6개라 시각적 판단 어려움)
KM_INST = survfit(Y ~ INST)
plot(KM_INST, lty=1:6, col=1:6)
survdiff(Y ~ INST)   # logrank test

  # TX
    #- 전구간
KM_TX = survfit(Y ~ TX)
plot(KM_TX, lty=1:2, col=1:2)
survdiff(Y ~ TX)   # logrank test 결과 p-value = 0.308 > 0.05
    #- 1091일 이전
KM_TX_1091 = survfit(Y ~ TX, subset=(TIME < 1092))
plot(KM_TX_1091, lty=1:2, col=1:2)
survdiff(Y ~ TX, subset=(TIME < 1092))   # logrank test 결과 p-value = 0.0981 > 0.05

  # GRADE
KM_GRADE = survfit(Y ~ GRADE)
plot(KM_GRADE, lty=1:3, col=1:3)
survdiff(Y ~ GRADE)   # logrank test 결과 p-value = 0.0702 > 0.05

  # AGE_GRP
    #- 전연령대
KM_AGE_GRP = survfit(Y ~ AGE_GRP)
plot(KM_AGE_GRP, lty=1:8, col=1:8)
survdiff(Y ~ AGE_GRP)   # logrank test 결과 p-value = 0.02 < 0.05
    #- 자료가 부족한 20-30, 80-90대 제외?
KM_AGE_GRP_some = survfit(Y ~ AGE_GRP, subset=(AGE %in% 40:79))
plot(KM_AGE_GRP_some, lty=1:4, col=1:4)   # 1~4가 각각 어느 그룹인지 확인
survdiff(Y ~ AGE_GRP, subset=(AGE %in% 40:79))   # logrank test 결과 p-value = 0.247 > 0.05

  # COND
KM_COND = survfit(Y ~ COND)
plot(KM_COND, lty=1:4, col=1:4)
survdiff(Y ~ COND)   # logrank test 결과 p-value = 1.24e-10 < 0.05

  # T_STAGE
KM_T_STAGE = survfit(Y ~ T_STAGE)
plot(KM_T_STAGE, lty=1:4, col=1:4)
survdiff(Y ~ T_STAGE)   # logrank test 결과 p-value = 0.0143 < 0.05

  # N_STAGE
KM_N_STAGE = survfit(Y ~ N_STAGE)
plot(KM_N_STAGE, lty=1:4, col=1:4)
survdiff(Y ~ N_STAGE)   # logrank test 결과 p-value = 0.0158 < 0.05

