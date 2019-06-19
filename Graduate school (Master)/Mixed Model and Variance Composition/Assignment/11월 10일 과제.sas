/* Input data */

data cars ;
input block treat tire @@ ;
cards ;
1 1 10.4 1 2 12.4 1 3 13.1 1 4 11.8 2 1 10.9 2 2 12.4 2 3 13.4 2 4 11.8
3 1 10.5 3 2 12.3 3 3 12.9 3 4 11.4 4 1 10.7 4 2 12.0 4 3 13.3 4 4 11.4
;
run ;

proc print data=cars ;
run ;

* Q1 ;

proc boxplot data=cars ;
plot tire*block ;
run ; quit ;

proc sort data=cars out=cars1 ;
by treat ;
run ;

proc boxplot data=cars1 ;
plot tire*treat ;
run ; quit ;

* Q2-1 ;

data cars_a ;
input block company $ tire @@ ;
cards ;
1 a1 11.4 1 a2 12.45 2 a1 11.65 2 a2 12.6 
3 a1 11.4 3 a2 12.15 4 a1 11.35 4 a2 12.35
;
run ;

proc print data=cars_a ;
run ;

proc glm data=cars_a ;
class block company ;
model tire=block company ;
random block / test ;
run ; quit ;

* Q2-2 ;

data cars_b ;
input block kind $ tire @@ ;
cards ;
1 b1 11.75 1 b2 12.1 2 b1 12.15 2 b2 12.1
3 b1 11.7 3 b2 11.85 4 b1 12 4 b2 11.7
;
run ;

proc print data=cars_b ;
run ;

proc glm data=cars_b ;
class block kind ;
model tire=block kind ;
random block / test ;
run ; quit ;

* Q3-1 ;

proc mixed data=cars_a ;
class company block ;
model tire=company ;
random block ;
run ; quit ;

* Q3-2 ;

proc mixed data=cars_b ;
class kind block ;
model tire=kind ;
random block ;
run ; quit ;

* Q3-3 ;

proc mixed data=cars ;
class treat block ;
model tire=treat ;
random block ;
run ; quit ;

* Q3-4 ;

proc mixed data=cars ;
class treat block ;
model tire=treat ;
random block ;
estimate 'block 3 & treat 3' intercept 1 treat 0 0 1 0 |
                                         block 0 0 1 0 ;   
run ; quit ;

* Q3-5 ;

proc mixed data=cars_b ;
class kind block ;
model tire=kind ;
random block ;
estimate 'block 1 & kind b1-b2' | block 1 0 0 0 / subject 1 -1 ;
run ; quit ;










 
