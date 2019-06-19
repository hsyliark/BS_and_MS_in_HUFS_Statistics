* Lab class March.11th ;

libname clinic 'D:\practice' ;

data sasuser.admit2;
set sasuser.admit;
where age>39;
run;

proc print data=sasuser.admit2;
run;

proc print data=sasuser.insure;
run; 

proc contents data=sasuser._all_ nods;
run; 

proc datasets;
contents data=sasuser._all_ nods;
quit;

proc contents data=sasuser.insure;
run; 

proc datasets;
contents data=sasuser.admit varnum;
quit;

proc contents data=sasuser.admit varnum;
run;


