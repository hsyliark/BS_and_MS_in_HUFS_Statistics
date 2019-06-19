/* Exercise 4.1 */
proc import out=cost
datafile="D:\수업자료 (대학, 대학원)\대학원\대학원 석사 2학기\회귀분석(TA)\실습수업자료\10월 28일\COST4.csv"
dbms=csv replace ;
run ;

proc print data=cost ;
run ;

proc reg data=cost ;
model COST=PAPER MACHINE OVERHEAD LABOR / clb alpha=0.05 ;
run ; quit ;

/* Exercise 4.3 */

proc reg data=cost ;
model COST=PAPER MACHINE OVERHEAD LABOR ;
run ; quit ;

proc reg data=cost ;
model COST=PAPER MACHINE ;
run ; quit ;

/* Exercise 4.2 */

proc import out=harris
datafile="D:\수업자료 (대학, 대학원)\대학원\대학원 석사 2학기\회귀분석(TA)\실습수업자료\10월 28일\HARRIS4.csv"
dbms=csv replace ;
run ;

proc print data=harris ;
run ;

proc reg data=harris ;
model SALARY=EDUC EXPER TIME / clb alpha=0.05 ;
run ; quit ;

/* Exercise 4.4 */

proc reg data=harris ;
model SALARY=EDUC EXPER TIME ;
run ; quit ;

proc reg data=harris ;
model SALARY=EDUC ;
run ; quit ;


