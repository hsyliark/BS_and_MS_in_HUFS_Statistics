proc import 
 datafile='I:\회귀분석실습\실습 자료\harris7.csv'
 out=salary
 dbms=csv replace;
run;


proc sgplot data=salary;
 scatter y=salary x=educat/ group=males;
run;

proc gplot data= salary ;
plot salary * educat ;
by males;
run;

*남녀별 교육수준에 따른 급여를 나타내는 회귀선을 구하고 남여별 회귀선;
proc reg data=salary;
 model salary=educat males;
 output out=out  p=predic;
run;quit;

proc sgplot data=out;
 scatter y=salary x=educat / group=males;
 series y=predic x=educat / group=males;
run;

*남녀별 교육수준에 따른 급여증가비율이 같은지 다른지 조사 교호작용 변수 생성;
data salary2;
 set salary;
 edumales=educat*males;
run;

*급여증가비율이 다르다고 했을 경우, 남녀 각각 교육수준에 따른 급여증가를 나타내는 회귀선;
proc reg data=salary2;
 model salary=educat males edumales;
 output out=out2 p=predic;
run;quit;

proc sgplot data=out2;
 scatter y=salary x=educat / group=males;
 series y=predic x=educat / group=males;
run;
