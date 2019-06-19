/* Exercise 4.1 */
proc import out=cost
datafile="C:\Users\user2\Desktop\È¸±ÍºÐ¼®(TA)\COST4.csv"
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
datafile="C:\Users\user2\Desktop\È¸±ÍºÐ¼®(TA)\HARRIS4.csv"
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


