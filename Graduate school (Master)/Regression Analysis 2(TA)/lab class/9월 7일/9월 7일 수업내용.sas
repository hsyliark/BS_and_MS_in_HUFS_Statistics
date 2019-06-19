/*** p39 ex 2-30 ***/

/** CAFE 기준이 27.5 라면 표본으로 선택된 147개 자동차 가운데 이에 미치지 못하는 자동차는 전체 표본의 몇%인지 답하라. **/

/* data 불러오기 */
proc import out=gas
datafile='C:\Users\user\Desktop\CARS2.csv'
dbms=csv replace ;
run;

/* data 정렬하기 */
proc sort data=gas;
by HWYMPG;
run;

/* data 출력하기*/
proc  print data=gas;
run;

/** 2003년 생산된 모든 자동차들의 평균 gas mileage 가 CAFE 기준인?27.5를 통과하는지 여부를 유의수준 0.05로 가설검정 하여라. **/

/* T-test */
proc ttest data = gas h0=27.5 sides=U ;
var HWYMPG ;
run ;




/***(2.32) GMAT 평균비교 손으로 계산하고 sas 로 확인하기 (F-test 포함)***/

/** p43 ex 2-32 **/

/* F-test for testing difference of variation */

data gmat1 ;
xbar1=545 ; xbar2=510 ; n1=50 ; n2=50 ;
s1=104 ; s2=95 ; f=s1**2/s2**2 ;
f1=finv(0.975,49,49) ;
f2=finv(0.025,49,49) ;
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
ci_upper=(xbar1-xbar2)+t*sqrt(sp2*(1/n1+1/n2)) ;
run ;

proc print data=gmat2 ;
run ; 
