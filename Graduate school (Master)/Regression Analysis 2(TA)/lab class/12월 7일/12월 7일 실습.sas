/* Regression Analysis Lab (Dec. 7th) */


** Indicator variables

* Salary vs. Education data ;
proc import out=harris 
datafile="C:\Users\user\Desktop\HARRIS7.csv"
dbms=csv replace ;
run;

proc print data=harris ;
run ;

* 1) 성별에 따른 산점도 (y=salary, x=education) 그리기 ;
proc sgplot data=harris ;
scatter y=salary x=educat / group=males ;
run ; quit ;

* 2) 남녀별 교육수준에 따른 급여를 나타내는 회귀선을 지시변수를 이용하여 구하라. ;
proc reg data=harris ;
model salary=educat males ;
output out=out_harris p=pred ;
run ; quit ;

proc print data=out_harris ;
run ;

proc sgplot data=out_harris ;
scatter y=salary x=educat / group=males ;
series y=pred x=educat / group=males ;
run ;

* 3) 교육수준에 따른 급여변화가 남녀에 따라서 다른지 조사하라. (Interaction variable) ;
data harris2 ;
set harris ;
edumales=educat*males ;
run ;

proc print data=harris2 ;
run ;

proc reg data=harris2 ;
model salary=educat males edumales ;
run ; quit ; 



** Variable selection

* Import data ;
proc import out=fuel 
datafile='C:\Users\user\Desktop\FUELCON6.csv'
dbms=csv replace ;
run ;

proc print data=fuel ;
run ;

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







