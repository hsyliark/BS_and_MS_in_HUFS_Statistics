/* HW : p.91 (6)번 문제를 한국말로 해석하고, 한국말로 답하라. */

proc import out=DIV3
datafile="C:\Users\user2\Desktop\2015-2 TA\회귀분석(TA)\실습수업자료\9월 23일\DIV3.csv"
dbms=csv replace ;
run ;

proc print data=DIV3 ;
run ;

proc reg data=DIV3 ;
model DIVYIELD=EPS / clb alpha=0.05 ;
plot DIVYIELD*EPS / conf pred ;
run ; quit ;

