
* Trend Model  &   Lagged  Dependent Variable Model ;

* trend model  for  Unemployment Data  ;

libname library 'c:\sastemp' ;
libname tranfile xport '\\psf\Home\Documents\매직 폴더\학교수업(2011 이후)\2학기\회귀분석\Dielman Data\Data sets\Chapter 4 Data\SAS\unemp4.xpt';
* To restore all data files in the library: ;
proc copy in = tranfile out = library memtype = data ; run ; 




data work.dat_unemp; set library.unemp4; 
time = _n_;
run;
proc print data=dat_unemp; run;

proc sgplot data=dat_unemp;
series  x=time  y=unemp;
run;



* Trend Model   ;

proc reg data=dat_unemp;
model unemp = time ; 
run;






* Lagged Dependent Variable ;

data dat2_unemp; set dat_unemp;
lag_unemp = lag1(unemp);


proc reg data=dat2_unemp;
model unemp = lag_unemp;
output out=unemp_out p=pred ;
run;

proc print data=unemp_out; run;


proc sgplot data=unemp_out;
series x=time y=unemp;
series x=time  y=pred;
run;

