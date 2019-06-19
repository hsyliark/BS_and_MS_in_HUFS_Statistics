/* Regression Analysis Lab (Nov. 25th) */

** Salary vs. Education data ;
proc import out=harris 
datafile="D:\수업자료 (대학, 대학원)\대학원\대학원 석사 2학기\회귀분석(TA)\실습수업자료\11월 25일\HARRIS7.csv"
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


* 3) 남자자료만 이용해서 교육수준에 따른 급여를 나타내는 회귀선을 구하라. ;
data harris_male ;
set harris ;
if males=0 then delete ;
run ;

proc print data=harris_male ;
run ;

proc reg data=harris_male ;
model salary=educat ;
run ; quit ;


* 4) 여자자료만 이용해서 교육수준에 따른 급여를 나타내는 회귀선을 구하라. ;
data harris_female ;
set harris ;
if males=1 then delete ;
run ;

proc print data=harris_female ;
run ;

proc reg data=harris_female ;
model salary=educat ;
run ; quit ; 


* 5) 교육수준에 따른 급여변화가 남녀에 따라서 다른지 조사하라. (Interaction variable) ;
data harris2 ;
set harris ;
edumales=educat*males ;
run ;

proc print data=harris2 ;
run ;

proc reg data=harris2 ;
model salary=educat males edumales ;
run ; quit ; 


* 6) 만일 급여증가율이 남녀별 다르다고 했을 경우, 남녀 각각 교육수준에 따른 급여증가율을 회귀선을 통해 구하라. ;
proc reg data=harris2 ;
model salary=educat males edumales;
output out=out_harris2 p=pred ;
run ; quit ;

proc print data=out_harris2 ;
run ;

proc sgplot data=out_harris2 ;
scatter y=salary x=educat / group=males ;
series y=pred x=educat / group=males ;
run ;
