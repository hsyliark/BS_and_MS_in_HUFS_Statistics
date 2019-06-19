** SAS Statistics Homework 1 answer ;



* Question 1 ;

data work.health ;
set sasuser.diabetes ;
run ;

proc print data=work.health ;
run ;


* Question 2 ;

proc means data=work.health mean std range skew kurt maxdec=3 ;
var height weight pulse ;
class sex ;
run ;


* Question 3 ;

proc sort data=work.health out=work.healthsort ;
by sex ;
run ;

proc means data=work.healthsort mean std range skew kurt maxdec=3 ;
var height weight pulse ;
by sex ;
run ;

 
* Question 4 ;

proc print data=work.health ;
var Age FastGluc PostGluc ;
where Sex='M' ;
sum Age FastGluc PostGluc ;
run ;


* Question 5 ;

data healthwoman ;
set health ;
drop ID ;
if Sex='F' ;
SumPG+PostGluc ;
HeightKor=Height*2.5445 ;
TestDate=20566 ;
length AgeRange $ 13 ;
if Age<30 then AgeRange='young person' ;
else if 30<=Age<50 then AgeRange='middle person' ;
else if Age>=50 then AgeRange='old person' ;
run ;

proc print data=healthwoman ;
format TestDate date9. ;
run ;


* Question 6 ;

data earn (drop=counter) ;
Value=100000 ;
do counter=1 to 15 ;
Value=Value + Value*0.0149 ;
Year+1 ;
SumValue+Value ;
output ;
end ;
run ;

 




