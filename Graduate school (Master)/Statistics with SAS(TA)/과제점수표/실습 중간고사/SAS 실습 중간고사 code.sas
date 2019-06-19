** SAS Statistics Lab Mid-term ;

* Q1 ;
 
data work.stress1 ;
set sasuser.stress ;
keep RestHR MaxHR RecHR ;
if Tolerance="D" ;
run ;

proc print data=work.stress1 ;
sum RestHR MaxHR RecHR ;
run ;


* Q2 ;

proc sort data=sasuser.admit out=work.admit1 ;
by Sex ;
run ;

proc means data=work.admit1 mean var cv q1 q3 kurt maxdec=3 ;
by Sex ;
run ;


* Q3 ;

data earn ;
Value=4000 ;
do Year=1 to 30 ;
Interest=Value*0.05 ;
Value+Interest ;
Year+1 ;
output ;
end ;
run ;

proc print data=earn ;
run ;


* Q4 ;

data work.stress2 ;
set sasuser.stress ;
if Tolerance="I" ;
keep RestHR MaxHR RecHR Tolerance ;
Totaltime=(TimeMin*60)+TimeSec ;
retain SumSec 10000 ;
SumSec+Totaltime ;
length RestLength $ 5 ;
if RestHR>75 then RestLength="long" ;
else if RestHR<=75 then RestLength="short" ;
run ;

proc print data=work.stress2 ;
run ;


* Q5 ;

data invest (drop = i) ;
do i=1 to 10 while (Capital < 50000) ;
year+1;
capital+4000 ;
capital+capital*.10 ;
output;
end;
run;

proc print data=invest ;
run;


