/** 9월 23일 회귀분석 실습 **/

/* p74. exercise 3 */


* 1. p.74 (3)번 문제 3개의 자료를 각각 Scatter Plot 하시오. ;

proc import out=REALEST3
datafile="C:\Users\0\Desktop\회귀 9월 23일\REALEST3.csv"
dbms=csv replace ;
run ;

proc import out=COMNODE3
datafile="C:\Users\0\Desktop\회귀 9월 23일\COMNODE3.csv"
dbms=csv replace ;
run ;

proc import out=HSTARTS3
datafile="C:\Users\0\Desktop\회귀 9월 23일\HSTARTS3.csv"
dbms=csv replace ;
run ;

proc print data=REALEST3 ;
run ;

proc print data=COMNODE3 ;
run ;

proc print data=HSTARTS3 ;
run ;

proc sgplot data=REALEST3 ;
scatter x=SIZE y=VALUE ;
title "REALEST3" ;
run ; quit ;

proc sgplot data=COMNODE3 ;
scatter x=NUMPORTS y=COST ;
title "COMNODE3" ;
run ; quit ;

proc sgplot data=HSTARTS3 ;
scatter x=RATES y=STARTS ;
title "HSTARTS3" ;
run ; quit ;



symbol1  V=circle C=blue  I= r ;

proc gplot data=REALEST3 ;
plot VALUE*SIZE ;
run ; quit ;

proc gplot data=COMNODE3 ;
plot COST*NUMPORTS ;
run ; quit ;

proc gplot data=HSTARTS3 ;
plot STARTS*RATES ;
run ; quit ;



* 2. Regression line 을 각각 구하라. ;

* 3. 기울기에 대한 95% 신뢰구간을 sas output 을 보고 구하라. ;


proc reg data=REALEST3 ;
model VALUE=SIZE / clb alpha=0.05 ;
plot VALUE*SIZE / conf pred ;
run ; quit ;

proc reg data=COMNODE3 ;
model COST=NUMPORTS / clb alpha=0.05 ;
plot COST*NUMPORTS / conf pred ;
run ; quit ;

proc reg data=HSTARTS3 ;
model STARTS=RATES / clb alpha=0.05 ;
plot STARTS*RATES / conf pred ;
run ; quit ;





/* HW : p.91 (6)번 문제를 한국말로 해석하고, 한국말로 답하라. */


