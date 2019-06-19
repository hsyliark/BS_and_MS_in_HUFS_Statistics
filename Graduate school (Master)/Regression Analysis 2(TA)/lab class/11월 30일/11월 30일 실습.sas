/* Regression Analysis Lab (Nov.24th) */

** Telemarketing data ;

proc import out=telemark
datafile="C:\Users\user\Desktop\TELEMARK5.csv"
dbms=csv replace ;
run ;

proc print data=telemark ;
run ;

* Lack of fit test ;
proc reg data=telemark;
model calls = months / lackfit ; 
run ; quit ;

data telemark ; 
set telemark ;
monsq = months**2 ; 
run ;

proc reg data=telemark ;
model calls = months monsq / lackfit ss1 ;
run ; quit ;

* Studentized Residual Plot ;
proc reg data=telemark ;
model calls = months ;
plot rstudent.*months ;
run ; quit ;






**  S&L  Return Rate Data  ;
* Outlier  &  Influential Point ;

proc import out=sl6
datafile="C:\Users\user\Desktop\SL6.csv"
dbms=csv replace ;
run ;

proc print data=sl6 ;
run ;

* (1) Scatter plot with ID label ;
data sl6 ; 
set sl6 ;
ID=_n_ ;
run ;

proc print data=sl6 ;
run ;

proc reg data=sl6 ; 
model  return = beta sigma ; 
plot return*beta=id  predicted.*beta='P' / overlay ;
run ; quit ;

* (2) Outlier detection with studentized residual ; 
proc reg data=sl6 ; 
model  return = beta sigma / influence ;
plot rstudent.*beta ;
run ; quit ;

* (3) Leverage detection with ...
        Influential detection with DFIT, Cook¡¯s D ;
proc reg data=sl6 ;  
model return = beta sigma / influence r ;
plot DFFITS.*beta COOKD.*beta ;
run ; quit ; 
 
* (4) Durbin-Watson test ;
proc reg data=sl6 ;  
model return = beta sigma / dwprob ;
output out=res_out r=res ;
run ; quit ;

proc print data=res_out ;
run ;

proc univariate data=res_out normal plot ;
var res ;
run ;

