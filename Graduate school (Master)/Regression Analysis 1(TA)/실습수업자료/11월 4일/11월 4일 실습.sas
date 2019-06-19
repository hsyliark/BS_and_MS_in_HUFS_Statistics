/* Regression analysis Lab (Nov.4th) */



/* Meddicorp data */

proc import out=meddicorp
datafile="D:\수업자료 (대학, 대학원)\대학원\대학원 석사 2학기\회귀분석(TA)\실습수업자료\11월 4일\MEDDICORP4.csv"
dbms=csv replace ;
run ;

proc print data=meddicorp ;
run ;

* 1.1 F-test using Type 1 SS ;

proc reg data=meddicorp ;
model SALES = ADV BONUS MKTSHR COMPET / SS1 ;
run ; quit ;

proc reg data=meddicorp ;
model SALES = ADV BONUS / SS1 ;
run ; quit ;

* 1.2 Checking multicollinearity problem using VIF ;

proc reg data=meddicorp ;
model SALES = ADV BONUS MKTSHR COMPET / VIF ;
run ; quit ;

* 1.3 Regression line using Lagged Independent variable ;

data meddicorp1 ;
set  meddicorp  ;
Lag1_adv = Lag1(ADV) ;
run ;

proc print data=meddicorp1 ;
run ;

proc reg data=meddicorp1 ;
model SALES = ADV Lag1_adv BONUS / VIF ;
run ; quit ;


/* Unemployment data */

proc import out=unemp
datafile="D:\수업자료 (대학, 대학원)\대학원\대학원 석사 2학기\회귀분석(TA)\실습수업자료\11월 4일\UNEMP4.csv"
dbms=csv replace ;
run ;

proc print data=unemp ;
run ;

** Regression line using Lagged Dependent variable ;

* 2.1 Trend model ;

data unemp1 ;
set unemp ;
TIME+1 ;
Lag_unemp = Lag1(UNEMP) ;
run ;

proc print data=unemp1 ;
run ;

proc sgplot data=unemp1 ;
series x=TIME y=UNEMP ;
run ; quit ;

proc reg data=unemp1 ;
model UNEMP=TIME ;
run ; quit ;

* 2.2 Regression with first lag ;

proc reg data=unemp1 ;
model UNEMP = Lag_unemp ;
output out=unemp_out p=pred ;
run ; quit ;

proc print data=unemp_out ; 
run ;

proc sgplot data=unemp_out ;
series x=TIME y=UNEMP ;
series x=TIME y=pred ;
run ; quit ;
