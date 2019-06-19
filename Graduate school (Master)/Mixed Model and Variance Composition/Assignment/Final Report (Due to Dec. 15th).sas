/* Final Report (Due to Dec. 15th) */

* Input data ;
data experiment ;
input id time score drug @@ ;
cards ;
1 1 20 1000 1 2 18 1100 1 3 15 1200 1 4 20 1300
2 1 22 1000 2 2 24 1000 2 3 18 1005 2 4 22 950
3 1 14 1000 3 2 10 1999 3 3 24 800 3 4 10 1700
4 1 38 1000 4 2 34 1100 4 3 32 1150 4 4 34 1100
5 1 25 1000 5 2 29 1000 5 3 25 1050 5 4 29 1010
6 1 30 1000 6 2 28 1100 6 3 26 1109 6 4 14 1500
;
run ;

proc print data=experiment ;
run ;

* Q1 ;
* Line graph (score vs time) ;
proc sgplot data=experiment ;
scatter x=time y=score / group=id ;
series x=time y=score / group=id ;
run ; quit ;

* Line graph (drug vs time) ;
proc sgplot data=experiment ;
scatter x=time y=drug / group=id ;
series x=time y=drug / group=id ;
run ; quit ;
 
* Q2 ;
proc reg data=experiment ;
model score=drug ;
output out=out_reg p=pred ;
run ; quit ;

proc print data=out_reg ;
run ;

proc sgplot data=out_reg ;
scatter x=drug y=score / group=id ;
series x=drug y=pred ;
run ; quit ;

* Q3, Q4 ;
* Making data by patients ;
data p1 ;
input id time score drug @@ ;
cards ;
1 1 20 1000 1 2 18 1100 1 3 15 1200 1 4 20 1300
;
run ;
data p2 ;
input id time score drug @@ ;
cards ;
2 1 22 1000 2 2 24 1000 2 3 18 1005 2 4 22 950
;
run ;
data p3 ;
input id time score drug @@ ;
cards ;
3 1 14 1000 3 2 10 1999 3 3 24 800 3 4 10 1700
;
run ;
data p4 ;
input id time score drug @@ ;
cards ;
4 1 38 1000 4 2 34 1100 4 3 32 1150 4 4 34 1100
;
run ;
data p5 ;
input id time score drug @@ ;
cards ;
5 1 25 1000 5 2 29 1000 5 3 25 1050 5 4 29 1010
;
run ;
data p6 ;
input id time score drug @@ ;
cards ;
6 1 30 1000 6 2 28 1100 6 3 26 1109 6 4 14 1500
;
run ;

* score vs time ;
proc reg data=p1 ;
model score=time ;
output out=res1_p1 p=pred ;
run ;
proc reg data=p2 ;
model score=time ;
output out=res1_p2 p=pred ;
run ;
proc reg data=p3 ;
model score=time ;
output out=res1_p3 p=pred ;
run ;
proc reg data=p4 ;
model score=time ;
output out=res1_p4 p=pred ;
run ;
proc reg data=p5 ;
model score=time ;
output out=res1_p5 p=pred ;
run ;
proc reg data=p6 ;
model score=time ;
output out=res1_p6 p=pred ;
run ;

data res1 ;
set res1_p1 res1_p2 res1_p3 res1_p4 res1_p5 res1_p6 ;
run ;
 
proc sgplot data=res1 ;
scatter x=time y=score / group=id ;
series x=time y=pred / group=id ;
run ; quit ;

* score vs drug ;
proc reg data=p1 ;
model score=drug ;
output out=res2_p1 p=pred ;
run ;
proc reg data=p2 ;
model score=drug ;
output out=res2_p2 p=pred ;
run ;
proc reg data=p3 ;
model score=drug ;
output out=res2_p3 p=pred ;
run ;
proc reg data=p4 ;
model score=drug ;
output out=res2_p4 p=pred ;
run ;
proc reg data=p5 ;
model score=drug ;
output out=res2_p5 p=pred ;
run ;
proc reg data=p6 ;
model score=drug ;
output out=res2_p6 p=pred ;
run ;

data res2 ;
set res2_p1 res2_p2 res2_p3 res2_p4 res2_p5 res2_p6 ;
run ;
 
proc sgplot data=res2 ;
scatter x=drug y=score / group=id ;
series x=drug y=pred / group=id ;
run ; quit ;

* Q5 ;
proc mixed data=experiment ;
class id time ;
model score=drug time drug*time ;
random id ;
repeated time / subject=id type=TOEP(1) ;
run ; quit ; 

* Q6 ;
proc genmod data=experiment ;
class id time ;
model score=drug time drug*time / link=identity ;
repeated subject=id / type=TOEP(1) ;
run ; quit ;
