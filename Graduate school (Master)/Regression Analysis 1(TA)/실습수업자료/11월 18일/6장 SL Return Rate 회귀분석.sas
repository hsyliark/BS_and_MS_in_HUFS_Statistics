
* Outlier  &  Influential Point ;;

*  S&L  Return Rate Data  ;

libname library 'c:\sastemp' ;
libname tranfile xport '\\psf\Home\Documents\매직 폴더\학교수업(2011 이후)\2학기\회귀분석\Dielman Data\Data sets\Chapter 6 Data\SAS\sl6.xpt';
* To restore all data files in the library: ;
proc copy in = tranfile out = library memtype = data ; run ; 




* (1) Plot  Return Rate vs. Beta,  Sigma ;



data work.dat_sl6; set library.sl6; 
time = _n_;
run;
proc print data=dat_sl6; run;


proc sgplot data=dat_sl6;
scatter  x=beta  y=return;
run;


proc sgplot data=dat_sl6;
scatter  x=sigma  y=return;
run;



* (2) Calculate  R^2  and  Do the significance test for parameters ;
* (3)  Detect the outliers by using  studentized residual ... What is the studentized residual of outlier ? ;
* (4)  Detect the leverage point by using "Hat Diag H value ",  h_ii , > 0.171 = 2(2+1)/35 ;
* (5)  Find out the points such that  DFIT > 2 sqrt((1+2)/35) =0.585 ,  |Cook's D|  > 4/35 = 0.11  ;

proc reg data= dat_sl6;
model return = beta sigma /influence;
run;


*(6)  Plot the residual  vs.  time   ;
*(7)  DW test for residual  ;

proc reg data= dat_sl6;
model return = beta sigma / dwprob ;
output out=res_out r=res ;
run;

proc print data=res_out; run;

proc sgplot data=res_out;
series x=time y=res;
refline 0.0;
run;
