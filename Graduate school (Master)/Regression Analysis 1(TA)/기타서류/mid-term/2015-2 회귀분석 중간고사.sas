/* 이론시험 */

proc import out=theory
datafile="C:\Users\user2\Desktop\2015-2 TA\회귀분석(TA)\기타서류\2015-2 Regression Analysis mid-term score.csv"
dbms=csv replace ;
run ;

proc print data=theory ;
run ;

proc univariate data=theory normal plot ;
var SCORE ;
histogram SCORE / normal
cfill = green
midpoints = 15 30 45 60 75 90 105 ;
run ; quit ;

/* 연습시험 */

proc import out=lab
datafile="C:\Users\user2\Desktop\2015-2 TA\회귀분석(TA)\기타서류\2015-2 Regression Analysis Lab mid-term score.csv"
dbms=csv replace ;
run ;

proc print data=lab ;
run ;

proc univariate data=lab normal plot ;
var SCORE ;
histogram SCORE / normal
cfill = yellow
midpoints = 10 20 30 40 50 60 70 80 90 ;
run ; quit ;



