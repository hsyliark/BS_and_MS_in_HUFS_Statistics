#-----------------------------------------------------------5월 13일

## 현재 작업 디렉토리 확인
getwd()

## 특정 디렉토리 지정
setwd("C:/Users/user2/Desktop/pdf 변환")

## 함수 persp에 대한 검색
? persp
help(persp)

## log라는 명칭이 들어간 함수 검색
?? log
help.search("log")


## 모든 이름의 시작은 알파벳 또는 마침표(.)로
abc <- 3 # OK
.jeong <- abc # OK
2.res <- 3 # 에러...

## 여러 명령어를 한 줄에 입력할 때는 세미콜론(;)으로 구분 
beta.0 <- 3; beta.1 <- 2

## 주석문(comment)은 #을 이용 
rnorm(10) # to generate 10 random numbers 

## R은 대소문자를 구별함 
a <- 1 
A <- 2 
a == A 

## 산술연산 
x <- 11; y <- 3 
x+y 
x-y
x*y 
x/y 
x^y 
x%/%y # integer quotient (정수부분)
x%%y # modulo (x를 y로 나눴을 때 나머지) 

## 지수 표현 Numbers with exponents 
1.2e3 # 1.2 * 1,000 
1.2e-3 # 1.2 * 1/1,000 

## 복소수 Complex numbers 
z1 <- 1.2+3.4i; z2 <- 4i 
z1 + z2 

## 수학 함수 
x <- 10; y <- 3.21; z <- 1; n <- 2 
log(x)
log10(x) 
log(n, x)
exp(x) 
sin(x) 
cos(x)
tan(x)
asin(z)
acos(z)
atan(z) 
abs(x) 
sqrt(x) 
floor(y) #소수점 이하 버림 
ceiling(y) #소수점 이하 올림
round(y, digits=0) #소수점 이하 반올림 
gamma(x) 
lgamma(x) 
factorial(x) # x! 
choose(x, n) # xCn


## 벡터표현 : 실수(double), 정수(integer), 문자열(string), 논리값(logical)
x <- c(1, 2.5, 3.2) # double 
y <- c(1L, 2L, 3L) # integer 
z <- c("KTX", "Saemaul", "Mugunghwa") # string 
v <- c(TRUE, FALSE, FALSE, TRUE) # logical
x[3] # x의 세 번째 성분
x[c(1,3)] # x의 첫 번째, 세 번째 성분을 추출한 부분 벡터

## 이름 지정하기
fruit <- c(5, 3, 2) 
names(fruit) <- c("apple", "orange", "peach") 
fruit
fruit[c("apple", "peach")]
fruit <- setNames(c(5, 3, 2), c("apple", "orange", "peach"))
fruit


## 벡터 관련 주요 함수
typeof(x) # double 
is.double(x) # TRUE 
as.double(y) # 벡터 y를 실수화
as.integer(x) # 벡터 x를 정수화
is.numeric(z) # FALSE 
as.numeric(v) # 1 0 0 1 
length(x) # length of x 

## 새로운 벡터 생성
a <- c(1, 2, 3); b <- c(5, 6) 
x <- c(a, 4, b) 
a[7] <- 2 # assign to the 7th element 
a # R extends the vector automatically
append(x, 99, after=3) # x의 세 번째 성분 다음에 99 삽입
append(x, -99, after=0) # x의 맨 앞에 -99 삽입
x <- seq(from=0, to=1, by=0.1) # 0부터 1까지 0.1씩 증가하는 등차수열 
y <- seq(from=0, to=1, length=11) # 0부터 1까지 길이가 11인 등차수열 
x==y
z <- 1:10 # 1, 2, 3, ... , 9, 10 
z
5:-5 # 5, 4, 3, ... , -4, -5 
rep(1, 10) # repeat 1 ten times

## 벡터 간 산술연산
x <- 1:3; y <- c(2,2,2) 
x+y 
x-y 
x*y 
x/y 
x^y

## 길이가 서로 다른 벡터 : 재사용규칙(recycling rule)
z <- rep(2, 5) 
x+z
y-3

## 논리값 벡터(logical vector)
x <- 1:10; y <- rep(5, 10) 
z <- x<5 # less than 
z
sum(z)
x<=5 # less than or equal to
x==5 # equal
x!=5 # not equal
(x>5)&(y<2) # and
(x<5)|(y<2) # or


## 다양한 함수
x <- rnorm(10) 
x
y <- 1:10
y
z <- -5:4 
z
max(x) # 최대값
min(x) # 최소값 
sum(x) # 모든 성분의 합 
prod(x) # 모든 성분의 곱 
mean(x) # 평균 
median(x) # 중앙값 
range(x) # 최대값과 최소값의 차이 
quantile(x, probs=c(0.2, 0.7)) # 20%, 70% 분위수 
var(x) # 분산 
sd(x) # 표준편차
cov(x, y) # 공분산
cor(x, y) # 상관계수 
x 
sort(x) # 오름차순 정렬
rank(x) # 가장 작은 값을 1등으로 한 순위
order(x) # x를 정렬하기 위한 번호
x[order(x)] # sort(x)와 같은 효과 
cumsum(x) # 누적합  
cumprod(x) # 누적곱
cummax(x) # 누적최대값
cummin(x) # 누적최소값
x; y; z
pmax(x, y, z) # 성분별 최대값 
pmin(x, y, z) # 성분별 최소값 
x <- c(1, 2, 3, NA, 5) # NA : 결측치
is.na(x) 


## 인덱스 벡터
x <- -10:10 
x[3]
x[1:3]
x[c(1,3,5)]
x[-1]
x[-c(1,3,5)]
y <- x[x<0] # x에서 음수인 성분을 추출해 y에 할당 
y
x[x<0] <- -x[x<0] # 절대값 벡터 
x
x <- c(1, 2, 3, NA, 5) 
x[!is.na(x)]

#----------------------------------------------------------------------- 5월 27일

## 배열(arrays)과 행렬(matrices)
z <- array(1:20, dim=c(4,5)) # 벡터 1:20를 4*5 배열로 재배치 
z
z <- matrix(1:20, 4, 5) 
z
z <- matrix(2, 4, 5) 
z
z <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3, byrow=T) # Readability enhanced 
z
z[2, 3] # indexed by the position
x <- 1:4; y <- 5:8 
cbind(x, y)
rbind(x, y)
B <- matrix(0, 4, 5) 
cbind(B, 1:4)
A <- matrix(1:20, 4, 5); B <- matrix(1:20, 4, 5); C <- cbind(A, B) 
C
A+B
A-B
A*B
A/B
# 두 행렬의 차원이 같지 않은 경우 벡터와 달리 재사용규칙이 적용되지 않고 에러메시지 발생
matrix(1:20, 4, 5) + matrix(1:10, 2, 5) # Not run...
A <- matrix(runif(20), 5, 4) 
A
t(A) # matrix transposition
B <- t(A)%*%A # %*%: Matrix multiplication 
B
solve(B) # Inverse matrix
diag(5) # 5-by-5 diagonal identity matrix
diag(B) # diagonal matrix with the diagonal elements of B
C <- outer(1:3, 4:6, FUN="*") # outer product
C
# 행, 열에 이름 부여
z <- matrix(1:20, 4, 5) 
z
nrow(z)
colnames(z) <- c("alpha", "beta", "gamma", "delta", "eps") 
rownames(z) <- c("a", "b", "c", "d") 
z


## 리스트(list) : 성분의 타입이 동일하지 않음
Hong <- list(kor.name="홍길동", eng.name="Gil-dong", age=43, married=T, no.child=2, child.ages=c(13, 10)) 
Hong
Hong$age 
Hong["age"] 
Hong$child.age[2] 
str(Hong) # 자료의 구조 살피기
Hong[[2]] # Hong의 두번째 성분
Hong[c(1,2)]
is.list(Hong)
x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE)) 
x
lapply(x, mean) # x의 각 성분의 평균값을 계산
lapply(x, quantile, probs = (1:3)/4) # x의 각 성분의 사분위수를 계산


## 데이터프레임(data frame)
x <- c(100, 75, 80) 
y <- c("A302043", "A302044", "A302045") 
z <- data.frame(score=x, ID=y)
z
dat.1 <- data.frame(x=1:3, y=c('a', 'b', 'c')) 
dat.1
str(dat.1)
# 문자열 벡터 성분의 성격을 factor로 지정하지 않음
dat.2 <- data.frame(x=1:3, y=c('a', 'b', 'c'), stringsAsFactors=F) 
dat.2
str(dat.2)
typeof(dat.2) # list
class(dat.2) # data frame
is.data.frame(dat.2) # TRUE
# initial, dosage, blood, response라는 변수를 갖고 
# 1,000,000건의 데이터를 저장할 수 있는 데이터프레임을 
# 미리 할당해 초기화
N <- 1000000 
dat <- data.frame(initial=character(N), dosage=numeric(N),
                  blood=factor(N, levels=c("O", "A", "B", "AB")), response=numeric(N))
# faithful : 내장데이터
data(faithful)
View(faithful)
str(faithful)
faithful[[1]]
faithful$waiting
# Cars93 : MASS 라이브러리 내장데이터
library(MASS) # for dataset Cars93 
data(Cars93)
View(Cars93)
subset(Cars93, subset=(MPG.city > 32))
subset(Cars93, select=Model, subset=(MPG.city > 32))
x <- data.frame(a=1:5, b=c(1,2,NA,4,5)) 
x
cumsum(x) # fails
cumsum(na.omit(x)) # 결측치가 포함된 행을 제거 후 실시
# EuStockMarkets : 내장데이터
data(EuStockMarkets)
View(EuStockMarkets)
str(EuStockMarkets)
cor(EuStockMarkets) # 상관계수행렬 (correlation matrix)
cor(subset(EuStockMarkets, select=-SMI)) # 변수 SMI 제외
cor(subset(EuStockMarkets, select=-c(SMI, CAC))) # 변수 SMI, CAC 제외


## 데이터 이어붙이기
a <- data.frame(x=c(5, 10, 15), y=c("a", "b", "c")) 
b <- data.frame(z=c(10, 20, 30)) 
a
b
cbind(a, b) # 열방향 붙이기
a1 <- data.frame(x=c(20, 25, 30), y=c("d", "e", "f")) 
a1
rbind(a, a1) # 행방향 붙이기
a <- data.frame(Name=c("Mary", "Jane", "Alice", "Bianca"), score=c(90, 95, 100, 100)) 
a
b <- data.frame(Name=c("Jane", "Alice", "Ana"), weight=c(70, 55, 60)) 
b
merge(a, b, by="Name") # 지정변수를 기준으로 merging
merge(a, b, by="Name", all=T)
# with 함수 : 간결한 코딩
View(Cars93)
z <- (Cars93$Price-mean(Cars93$Price))/sd(Cars93$Price) # Cars93$... 지저분... 
head(z)
z <- with(Cars93, (Price-mean(Price))/sd(Price)) # 깔끔... 
head(z)


## 문자형 변수(Factor)
blood.type <- factor(c("A", "A", "AB", "O", "O"), levels=c("A", "B", "AB", "O")) 
table(blood.type)
blood.type <- c("A", "B", "AB", "O", "O") 
table(blood.type)
# 데이터 타입 변환
x <- 1:3
typeof(x) # integer (정수)
y <- as.character(x) 
y 
typeof(y) # character (문자)
z <- as.numeric(y)
z
typeof(z) # double (실수)


## 데이터 변형
library(MASS) # for the dataset Cars93 
View(Cars93)
summary(Cars93)
tmp <- split(Cars93$MPG.city, Cars93$Origin) # grouping by 'Origin' 
tmp
str(tmp) # List of 2
Jeong <- list(first.name="Samuel", age=44, gender="M", n.child=2, child.gender=c("M", "F")) 
Jeong
lapply(Jeong, length) # returns a list
is.list(lapply(Jeong, length))
sapply(Jeong, length) # returns a vector
is.vector(sapply(Jeong, length))
a <- matrix(1:20, 4, 5) 
a
apply(a, 1, mean) # to every row
apply(a, 2, mean) # to every column
# Cars93 데이터의 Price변수의 평균을 Origin별로 계산 (tapply function)
View(Cars93)
with(Cars93, tapply(Price, Origin, mean)) # tapply(Cars93$Price, Cars93$Origin, mean) 과 같은 결과
# Cars93의 Origin별로 summary() 함수를 적용
by(Cars93, Cars93$Origin, summary)

#----------------------------------------------------------------------- 6월 3일

## 문자열, 날짜변수
nchar("ABC") # 문자의 개수
length("ABC") # 벡터의 길이
nchar(c("A", "B", "C"))
length(c("A", "B", "C"))
# 이어붙이기
paste("A", "B", "C") # default separator: " "
paste("A", "B", "C", sep="")
paste("A", "B", "C", sep="/")
# 문자열의 일부추출
substr("12345678", start=5, stop=7)
cities <- c("New York, NY", "Los Angeles, CA", "Peoria, IL") 
nchar(cities)-1
nchar(cities)
substr(cities, start=nchar(cities)-1, stop=nchar(cities)) # extract last two characters
# 지정한 문자열로 쪼개기
path <- "/home/data/test.csv" 
strsplit(path, "/")
path <- c("/home/data/test1.csv", "/home/data/test2.csv", "/home/data/test2.csv") 
strsplit(path, "/")
Sys.Date() # 현재 날짜
class(Sys.Date()) # The class of the result from Sys.Date() is...
# as.Date()는 기본적으로 문자열이 yyyy-mm-dd 형태임을 가정
as.Date("2015-10-09") # standard format
as.Date("10/09/2015") # error (not in a standard format)
# format 값에서 %Y는 네 자리, %y는 두 자리로 연도를 표시한다는 뜻임
as.Date("10/09/2015", format="%m/%d/%Y") # American date format
as.Date("10/09/15", format="%m/%d/%y") # 2-digit year
format(Sys.Date()) # 날짜형 변수 -> 문자 변수
as.character(Sys.Date()) # format(Sys.Date()) 와 같은 효과
format(Sys.Date(), format="%m/%d/%Y")
as.character(Sys.Date(), format="%m/%d/%y")
format(Sys.Date(), format="%b/%d/%y") # %b: Abbreviated month name
format(Sys.Date(), format="%B %d, %Y") # %B: Full month name
# 연, 월, 일 정보를 하나로 합쳐 하나의 날짜형 데이터 만들기
ISOdate(2015, 10, 9) # year, month, day
as.Date(ISOdate(2015, 10, 9))
ISOdate(2015, 2, 29) # an invalid date
years <- 2011:2015 
months <- rep(1, 5) 
days <- c(4, 9, 14, 19, 24, 29) 
as.Date(ISOdate(years, months, days))
as.Date(ISOdate(years, 1, days)) # 재사용 규칙
# 날짜-시간 데이터에서 연, 월, 일, 요일, 시간 등의 세부 정보 추출
d <- as.Date("2015-10-09"); p <- as.POSIXlt(d) 
d
p
attributes(p)
p$year # Years since 1900
p$mon # Month (0 = January)
p$mday # Day of the month
s <- as.Date("2015-10-01") 
e <- as.Date("2015-10-10") 
seq(from=s, to=e, by=1)
seq(from=s, by=1, length.out=7)
seq(from=s, by="month", length.out=12)
seq(from=s, by="3 months", length.out=4)
seq(from=s, by="year", length.out=5)


## 데이터 입출력
x <- scan() # e.g) 1부터 3까지 입력
x
# 데이터 읽어들이기
x <- scan(file="C:/Users/user2/Desktop/R lab/data_x.txt")
x
y <- matrix(scan("C:/Users/user2/Desktop/R lab/data_y.txt"), ncol=3, byrow=T)
y
x <- read.table(file="C:/Users/user2/Desktop/R lab/credit.txt", header=T, sep=" ")
x
x <- read.csv(file="C:/Users/user2/Desktop/R lab/stress.csv", header=T, sep=",")
x
library(foreign)
read.spss("data.sav", to.data.frame=T) # SPSS 제작데이터 읽어들이기
library(MASS) 
data(geyser) # MASS 라이브러리 내장데이터
View(geyser)
data() # 현재 열린 패키지 안의 모든 내장데이터 확인
# 데이터 내보내기
x <- 1:3 
print(x)
x <- seq(from=0, to=1, by=0.1) 
write(x, file="C:/Users/user2/Desktop/R lab/output1.txt", ncolumns=11)
x <- matrix(1:20, 4, 5) 
write.table(x, file="C:/Users/user2/Desktop/R lab/output2.txt")
data(faithful)
View(faithful)
write.table(faithful, file="C:/Users/user2/Desktop/R lab/faithful.csv", sep=",", row.names=F)

#----------------------------------------------------------------------- 6월 10일

## 간단한 도표를 통한 자료 요약

# Beer Preference example 
beer <- c(3, 4, 1, 1, 3, 4, 3, 3, 1, 3, 2, 1, 2, 1, 2, 3, 2, 3, 1, 1, 1, 1, 4, 3, 1) 
# (1) Domestic can (2) Domestic bottle, 
# (3) Microbrew (4) Import 
table(beer)
barplot(table(beer))
barplot(table(beer)/length(beer), col=c("lightblue", "mistyrose", "lightcyan","cornsilk"),
        names.arg=c("Domestic\n can", "Domestic\n bottle", "Microbrew\n", "Import\n"),
        ylab="Relative frequency", main="Beer Preference Survey") # "\n" : 줄바꿈
beer.counts <- table(beer) # store the table result 
pie(beer.counts) # first pie -- kind of dull
names(beer.counts) <- c("Domestic\n can","Domestic\n bottle", "Microbrew","Import") # give names 
pie(beer.counts) # prints out names

# 양적자료(Quantitative data)의 요약
scores <- c(2, 3, 16, 23, 14, 12, 4, 13, 2, 0, 0, 0, 6, 28, 31, 14, 4, 8, 2, 5) 
stem(scores) # 줄기-잎 그림 (Stem-and-leaf plot)
x <- rnorm(1000) # To generate 1,000 random numbers from N(0,1) 
hist(x, xlab="data")
hist(x, probability=T, xlab="data", col="gray", border="white") 
z <- seq(from=-3, to=3, by=0.01) # N(0,1)의 밀도함수 곡선을 위한 grid 잡기 
lines(z, dnorm(z), col=2) # 빨간 색으로 N(0,1)의 밀도함수 곡선 덧그리기
# boxplot
growth <- c(75,72,73,61,67,64,62,63) # the size of flies 
sugar <- c("C","C","C","F","F","F","S","S") # diet 
fly <- data.frame(growth=growth, sugar=sugar) 
boxplot(fly$growth)
# Scatterplot
# cars: built-in dataset with the speed of cars and the distances taken to stop 
data(cars)
View(cars)
plot(cars$speed, cars$dist, col="blue", pch="+", ylab="Distance taken to stop", 
     xlab="Speed", ylim=c(-20, 140))
with(cars, plot(speed, dist, col="blue", pch=20, ylab="Distance taken to stop",
                xlab="Speed", ylim=c(-20, 140))) # 같은 효과
# R에 내장되어 있는 657가지의 색
colors()


## 프로그래밍
# if-else
x <- 10 
if (x < 3) print("x < 3") else print("x >= 3")
y <- -3:3
y
z <- ifelse(y < 0, -1, 1)
z
cbind(y, z)
x <- 4 
if ( x < 3 ) {
  print("x<3") 
  z <- "M" 
} else {
  print("x>3") 
  z <- "F" 
}
z

## 반복문(loop)
n <- 10; x <- 1:n; sum.so.far <- 0 
for ( i in 1:n ) { 
  sum.so.far <- sum.so.far + x[i] 
  print(sum.so.far) 
}
sum.so.far
# while : loop를 지속할 수 있는 조건지정
n <- 0; sum.so.far <- 0 
while ( sum.so.far <= 1000 ) { 
  n <- n+1 
  sum.so.far <- sum.so.far + n
}
print(c(n, sum.so.far))
sum(1:44)
sum(1:45)
# break : 조건문 강제로 빠져나오기
# 조건문 자리에 1(TRUE와 동일한 효과)이 있으므로 무한루프
n <- 0; sum.so.far <- 0 
while (1) {
    if(sum.so.far > 1000) break 
    n <- n+1 
    sum.so.far <- sum.so.far + n 
    } 
print(c(n, sum.so.far))
# 소요시간 계산
n <- 10000000; x <- 1:n 
system.time(y <- x) # 효율적인 방법
system.time(for (i in 1:n) y[i] <- x[i]) # loop 구문에 의해 시간이 많이 소요됨


## 원하는 함수 제작
my.stat <- function(x) {
  m <- mean(x); s <- sd(x) 
  res <- list(m=m, s=s) 
  par(mfrow=c(1, 2)) 
  boxplot(x, main="Boxplot", horizontal=T) 
  hist(x, prob=T, col="skyblue", border="white", main="Histrogram", xlab="data") 
  return(res) 
}
dat <- rnorm(1000, mean=3, sd=1) 
my.stat(x=dat)

## Q : 함수 my.ftn(x)를 제작 시 다음 조건을 만족하도록 하여라.
# 1. 평균, 분산, 그리고 표준편차가 출력되게 한다.
# 2. 상자그림과 히스토그램을 그리도록 지정한다.
# 3. lines 함수를 통해 밀도함수 곡선을 덧그리도록 지정한다.
# 4. 그림의 색깔은 본인이 원하는대로 설정할 것.