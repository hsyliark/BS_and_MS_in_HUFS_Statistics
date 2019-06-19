** Lab class March.18th ;

* Chapter 1 ;

options nonumber nodate;
proc print data=sasuser.admit;
run;

options date;
proc print data=sasuser.admit;
run;

options nodate number pageno=3;
proc print data=sasuser.admit;
run;

options number pageno=1 pagesize=30;
proc print data=sasuser.admit;
run;

options number pageno=1 linesize=64;
proc print data=sasuser.y2000;
run; 

options number pageno=1 linesize=200;
proc print data=sasuser.y2000;
run; 

options firstobs=10;
proc print data=sasuser.heart;
run;

options firstobs=2 obs=10;
proc print data=sasuser.heart;
run;


