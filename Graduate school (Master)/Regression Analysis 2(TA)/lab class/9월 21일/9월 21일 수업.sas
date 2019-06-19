/*** Regression analysis Lab (9/21) ***/

/** p69. exercise 3-1 Production Units vs. Overhead **/

* Input data ;
data budget ;
input production overhead @@ ;
cards ;
5 12 6 11.5 7 14 8 15 9 15.4 10 15.3 11 17.5
;
run ;
proc print data=budget ;
run ;

* Simple linear regression (proc reg) ;
proc reg data=budget ;
model overhead=production ;
plot overhead*production / conf pred ;
run ; quit ;

