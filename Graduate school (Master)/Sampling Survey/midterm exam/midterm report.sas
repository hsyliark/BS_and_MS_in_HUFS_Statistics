/* Midterm report */

proc import out=exam
datafile="D:\수업자료\College, Graduate school\Graduate school (Master)\4th semester\표본조사론\midterm exam\exam.csv"
dbms=csv replace ;
run ;

proc sort data=exam ;
by SIZES CITY ;
run ;

proc summary data=exam ;
var jong ;
by SIZES CITY ;
output out=res1 n=number ;
run ;

proc print data=res1 ;
by SIZES ; sum number ;
run ;

proc sort data=res1 out=res1_1 ;
by CITY ;
run ;

proc print data=res1_1 ;
by CITY ; sum number ;
run ;

proc import out=exam_1
datafile="D:\수업자료\College, Graduate school\Graduate school (Master)\4th semester\표본조사론\midterm exam\exam_1.csv"
dbms=csv replace ;
run ;

proc sort data=exam_1 ;
by SIZES CITY ;
run ;

proc summary data=exam_1 ;
var jong ;
by SIZES CITY ;
output out=res2 n=number ;
run ;

proc print data=res2 ;
by SIZES ; sum number ;
run ;

proc sort data=res2 out=res2_1 ;
by CITY ;
run ;

proc print data=res2_1 ;
by CITY ; sum number ;
run ;

proc summary data=exam_1 ;
var totwage ;
by SIZES CITY ;
output out=res3 std=sd ;
run ;

proc summary data=exam_1 ;
var totwage ;
by SIZES ;
output out=res3_1 std=sd ;
run ;

proc sort data=exam_1 out=exam_1_1 ;
by CITY ;
run ;

proc summary data=exam_1_1 ;
var totwage ;
by CITY ;
output out=res3_2 std=sd ;
run ;

proc univariate data=exam_1 ;
var totwage ;
run ;

data res2 ; set res2 ;
sqrtnumber=sqrt(number) ;
run ;

proc print data=res2 ;
sum sqrtnumber ;
run ;

data res2 ; set res2 ;
allocate=1000*(sqrtnumber/409.242) ;
run ;

proc print data=res2 ;
sum allocate ;
run ;

proc sort data=exam_1 ;
by SIZES CITY ;
run ;

proc import out=sqrtalloc
datafile="D:\수업자료\College, Graduate school\Graduate school (Master)\4th semester\표본조사론\midterm exam\sqrtalloc.csv"
dbms=csv replace ;
run ;

proc surveyselect data=exam_1 sampsize=sqrtalloc out=res4 seed=12345 ;
strata SIZES CITY ;
run ;

proc import out=examtotal
datafile="D:\수업자료\College, Graduate school\Graduate school (Master)\4th semester\표본조사론\midterm exam\examtotal.csv"
dbms=csv replace ;
run ;

proc print data=res4 ;
run ;

proc sort data=res4 ;
by SIZES CITY ;
run ;

proc sort data=examtotal ;
by SIZES CITY ;
run ;

proc surveymeans data=res4 total=examtotal ;
strata SIZES CITY / list ;
var totwage ;
weight weight ;
run ; quit ;

proc surveymeans data=res4 total=examtotal ;
strata SIZES CITY / list ;
var totwage ;
weight weight ;
by SIZES CITY ;
ods output statistics=res4_1 ;
run ; quit ;

proc sort data=res4 ;
by SIZES ;
run ;

proc sort data=examtotal ;
by SIZES ;
run ;

proc surveymeans data=res4 total=examtotal ;
strata SIZES / list ;
var totwage ;
weight weight ;
by SIZES ;
ods output statistics=res4_2 ;
run ; quit ;

proc sort data=res4 ;
by CITY ;
run ;

proc sort data=examtotal ;
by CITY ;
run ;

proc surveymeans data=res4 total=examtotal ;
strata CITY / list ;
var totwage ;
weight weight ;
by CITY ;
ods output statistics=res4_3 ;
run ; quit ;



























