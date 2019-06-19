/* Regression Analysis Lab (Dec. 2nd) */

* Import data ;
proc import out=fuel 
datafile='C:\Users\user2\Desktop\2015-2 TA\회귀분석(TA)\실습수업자료\12월 2일\FUELCON6.csv'
dbms=csv replace ;
run ;

proc print data=fuel ;
run ;

* Plotting ;
proc gplot data=fuel ;
plot fuelcon*pop ;
plot fuelcon*area ;
plot fuelcon*drivers ;
plot fuelcon*hwymiles ;
plot fuelcon*gastax ;
plot fuelcon*income ;
run ; quit ;

* Variable selection (criterion : R-square, Adjusted R-square, Cp statistic, AIC) ;
proc reg data=fuel ;
id state ;
model fuelcon = pop area drivers hwymiles gastax income / selection = rsquare adjrsq cp aic ; 
run ; quit ;

* Variable selection (criterion : R-square, AIC) with two variables include ;
proc reg data=fuel ;
id state ;
model fuelcon = pop gastax area drivers hwymiles income / include=2 selection = rsquare aic ;
run ; quit ;

* Forward selection ;
proc reg data=fuel ;
id state ;
model fuelcon = pop area drivers hwymiles gastax income / selection=forward sle=0.15 ; 
run ; quit ;

* Backward elimination ;
proc reg data=fuel;
id state ;
model fuelcon = pop area drivers hwymiles gastax income / selection=backward sls=0.20 ; 
run ; quit ;

* Stepwise regression ;
proc reg data=fuel ;
id state ;
model fuelcon = pop area drivers hwymiles gastax income / selection=stepwise sle=0.15  sls=0.20 ; 
run ; quit ;

* Graph of Cp statistic ;
ods graphics on ;
proc reg data=fuel plots=cp(label) ;
id state ;
model fuelcon = pop area drivers hwymiles gastax income / selection = cp ; 
run ; quit ;
ods graphics off ;

* Graph of AIC ;
ods graphics on ;
proc reg data=fuel plots=aic(label) ;
id state ;
model fuelcon = pop area drivers hwymiles gastax income / selection = rsquare aic ; 
run ; quit ;
ods graphics off ;







