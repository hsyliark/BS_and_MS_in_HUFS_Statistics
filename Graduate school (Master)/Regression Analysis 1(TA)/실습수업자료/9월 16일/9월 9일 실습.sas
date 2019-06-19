*----------------------------------------------* ;
*-----          Using INFILE Statement         ----------* ;
*----------------------------------------------* ;

data class ;
infile 'C:\Users\ADMIN\Desktop\조교\회귀분석\2015 2학기 회귀분석\Sample_data.csv' delimiter=',' firstobs=2 ;
informat Name $8.   Age best32.   Height best32.   Weight best32.    ;
input Name $ Sex $ Age Height Weight ;
run ;

/*data 내보내기*/
proc export data=sashelp.class
   outfile='C:\Users\Statistic\Desktop\abc\abc.xlsx'
   dbms=xlsx replace;
 run;


/* (2.25) Gas Mileage C.I 손으로 계산하기 */

proc import
out=gas
datafile='C:\Users\0\Desktop\9월 9일 실습\CARS2.csv'
dbms=csv replace ;
run;

proc sort data=gas;
by HWYMPG;
run;

proc  print data=gas;
run;

proc univariate data=gas ;
histogram HWYMPG;
run;

proc ttest data = gas;
var HWYMPG ;
run ;

/* (2.30)CAFE standard 에 대한 Hypothesis Testing 손으로 하기 */     

proc ttest data = gas h0=27.5 sides=U ;
var HWYMPG ;
run ;

/*** (2.32)  GMAT 평균비교  손으로 계산하고 sas 로 확인하기 (F-test 포함) ***/

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



