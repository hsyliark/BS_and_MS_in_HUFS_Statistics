*  Type I SS. ;
*  Sales vs.  ADV BONUS  MKTSHR  COMPET;


libname library 'c:\sastemp' ;
libname tranfile xport '\\psf\Home\Documents\매직 폴더\학교수업(2011 이후)\2학기\회귀분석\Dielman Data\Data sets\Chapter 4 Data\SAS\meddicorp4.xpt';
* To restore all data files in the library: ;
proc copy in = tranfile out = library memtype = data ; run ; 


data work.meddicor; set library.meddicor; run;
proc print data=meddicor; run;


proc reg data=meddicor; 
model sales = adv bonus mktshr compet ; 
run;


* Sequential SS ;
proc reg data=meddicor; 
model sales = adv bonus mktshr compet / ss1 ; 
run;


proc reg data=meddicor; 
model sales = adv bonus / ss1 ; 
run;


*  multi-collinearity   ;

proc reg data=meddicor; 
model sales = adv bonus mktshr compet / VIF ; 
run;


*  lagged independent variable ;

data col_med ; set meddicor;
lag1_adv = lag1(adv);

proc reg data=col_med; 
model sales = adv  lag1_adv   bonus  / VIF ; 
run;

