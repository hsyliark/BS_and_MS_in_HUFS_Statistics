/* telemarketing data */
proc import out=telemark
datafile="C:\Users\user2\Desktop\2015-2 TA\회귀분석(TA)\실습수업자료\11월 11일\TELEMARK5.csv"
dbms=csv replace ;
run ;

proc print data=telemark ;
run ;

proc sgplot data=telemark ;
scatter  x=months  y=calls ;
run ;

* Quadratic term ;
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

* Centering ;
proc univariate data=telemark ;
var months ;
run ;

data telemark ;
set telemark ;
months_c = months-20.55 ;
months_c_sq = months_c**2 ;
run ;

proc reg data=telemark ;
model calls = months monsq / vif ;
run ; quit ;

proc reg data=telemark ;
model calls = months_c months_c_sq / vif ;
run ; quit ;



/* gas mileage data */
proc import out=mpgph
datafile="C:\Users\user2\Desktop\2015-2 TA\회귀분석(TA)\실습수업자료\11월 11일\MPGHP5.csv"
dbms=csv replace ;
run ;

proc print data=mpgph ;
run ;

proc sgplot data=mpgph ;
scatter  x=hp  y=hwympg ;
run ;

* Reciprocal term ;
data mpgph ;
set mpgph ;
hpinv=1/hp ;
run ;

proc reg data=mpgph ;
model hwympg = hp ;
run ; quit ;

proc reg data=mpgph ;
model hwympg = hpinv ;
run ; quit ;



/* 'imports vs. GDP' data */
proc import out=impgdp
datafile="C:\Users\user2\Desktop\2015-2 TA\회귀분석(TA)\실습수업자료\11월 11일\IMPGDP5.csv"
dbms=csv replace ;
run ;

proc print data=impgdp ;
run ;

* Log transformation ;
data impgdp ;
set impgdp ;
logimp=log(imports) ;
loggdp=log(gdp) ;
run ;

proc sgplot data=impgdp ;
scatter x=gdp y=imports ;
run ; quit ;

proc sgplot data=impgdp ;
scatter x=loggdp y=logimp ;
run ; quit ;

* Regression ;
proc reg data=impgdp ;
model imports = gdp ;
run ; quit ;

proc reg data=impgdp ;
model logimp = loggdp ;
output out=res_gdp p=logimp_hat ;
run ; quit ;

proc print data=res_gdp ;
run ;

data res_gdp ;
set res_gdp ;
imports_hat=exp(logimp_hat) ;
residual=imports-imports_hat ;
residual2=residual**2 ;
run ;

proc print data=res_gdp ;
run ;

proc univariate data=res_gdp ;
var residual2 ;
run ;

data rsq ;
R_sq=1-(203139.478/1357403) ;
run ;

proc print data=rsq ;
run ;



























