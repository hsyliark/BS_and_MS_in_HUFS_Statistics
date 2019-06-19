/*148쪽 연습문제1번+158쪽연습문제3번*/
proc import
out=cost
datafile='H:\회귀분석\cost4.csv'
dbms=csv replace ;
run;
/*full model*/
proc reg data=cost;
model cost = paper machine overhead labor ;
run;quit;
/*reduced model*/
proc reg data=cost;
model cost = paper machine ;
run;quit;


/*150쪽 연습문제 2번+158쪽연습문제4번*/
proc import
out=aaa
datafile='H:\회귀분석\harris4.csv'
dbms=csv replace ;
run;

proc reg data=aaa;
model salary = educ exper time;
run;quit;

proc reg data=aaa;
model salary = educ ;
run;quit;

