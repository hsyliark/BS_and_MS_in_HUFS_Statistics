/*** (2.25) Gas Mileage (highway) for 147 cars  : 
          - 자동차 모델가운데 Best 5 models ? /  Worst 5 models ?
          -(histogram, univariate, t-test in sas 사용해서 자료 설명하기)  
+ C.I. 손으로 계산하여 sas 와 비교하기 ***/

/** p31 ex 2-25 **/

proc import out=cars2
datafile='C:\Users\user\Desktop\CARS2.csv' 
dbms=csv replace ;
run ;

/* Best 5 model, Worst 5 model */

proc sort data=cars2 ;
by HWYMPG ;
run ;

proc print data=cars2 ;
run ;

/* Explanation data */

proc univariate data=cars2 ;
var HWYMPG ;
histogram / outhistogram=outhisto cfill=green 
vaxis=0 to 50 by 5
midpoints=10 to 70 by 5 ;
run ;
quit ;

proc ttest data=cars2 ;
var HWYMPG ;
run ;


/*** (2.30) CAFE (corporate average fuel economy ) standard 가 왜 필요한지 논하라 
          - CAFE 기준이 27.5 라면 표본으로 선택된 147개 자동차 가운데 이에 미치지 못하는 자동차는 
전체 표본의 몇%인지 답하라.
          - 2003년 생산된 모든 자동차들의 평균 gas mileage 가 CAFE 기준인  27.5 를 통과하는지 여부를 
유의수준 0.05로 가설검정하여라 ***/

/** p39 ex 2-30 **/

data cars2_1 ;
set cars2 ;
if HWYMPG < 27.5 then delete ;
run ;

proc print data=cars2_1 ;
run ;

/* Hypothesis testing */

proc ttest data=cars2 H0=27.5 ;
var HWYMPG ;
run ;

/*** (2.32)  GMAT 평균비교  손으로 계산하고 sas 로 확인하기 (F-test 포함) ***/

/** p43 ex 2-32 **/

/* F-test for testing difference of variation */

data gmat1 ;
xbar1=545 ; xbar2=510 ; n1=50 ; n2=50 ;
s1=104 ; s2=95 ; f=s1**2/s2**2 ;
f1=finv(0.95,49,49) ;
s_prob=1-probf(f,49,49) ;
run ;

proc print data=gmat1 ;
run ;

/* Confidence interval for difference between mean GMAT */

data gmat2 ;
xbar1=545 ; xbar2=510 ; n1=50 ; n2=50 ;
s1=104 ; s2=95 ;
df=n1+n2-2 ;
t=tinv(0.975,df) ;
sp2=((n1-1)*s1**2+(n2-1)*s2**2)/(n1+n2-2) ;
ci_lower=(xbar1-xbar2)-t*sqrt(sp2*(1/n1+1/n2)) ; 
ci_high=(xbar1-xbar2)+t*sqrt(sp2*(1/n1+1/n2)) ;
run ;

proc print data=gmat2 ;
run ; 






 







