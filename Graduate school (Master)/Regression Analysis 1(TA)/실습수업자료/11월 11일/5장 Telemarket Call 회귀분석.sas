
* Linear or Quadratic Model ;;

*  Number of Calls per day  vs.  Work Period t Data  ;

libname library 'c:\sastemp' ;
libname tranfile xport '\\psf\Home\Documents\매직 폴더\학교수업(2011 이후)\2학기\회귀분석\Dielman Data\Data sets\Chapter 5 Data\SAS\telemark5.xpt';
* To restore all data files in the library: ;
proc copy in = tranfile out = library memtype = data ; run ; 




data work.dat_telemark; set library.telemark; 
time = _n_;
run;
proc print data=dat_telemark; run;

proc sgplot data=dat_telemark;
scatter  x=months  y=calls;
run;

proc reg data=dat_telemark;
model calls = months / lackfit; 
run;



*  Quadratic Term included ;

data b; set dat_telemark;
monsq = months**2;
moncub = months**3;
run;

proc reg data=b;
model calls = months monsq /lackfit ;
run;



proc reg data=b;
model calls = months monsq  moncub/ ss1 ;
run;



