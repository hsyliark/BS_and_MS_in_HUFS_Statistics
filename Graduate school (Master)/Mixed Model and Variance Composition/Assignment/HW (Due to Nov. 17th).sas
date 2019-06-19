/* HW (Due to Nov. 17th) */

data splitplot ;
input day worker RFID y @@ ;
cards ;
1 1 1 56 1 1 2 41 1 2 1 50 1 2 2 36 1 3 1 39 1 3 2 35
2 1 1 30 2 1 2 25 2 2 1 36 2 2 2 28 2 3 1 33 2 3 2 30
3 1 1 32 3 1 2 24 3 2 1 31 3 2 2 27 3 3 1 15 3 3 2 19
;
run ;

proc print data=splitplot ;
run ;

* Test using proc glm ;
proc glm data=splitplot ;
class day worker RFID ;
model y = worker day worker*day RFID worker*RFID ;
random worker day worker*day worker*RFID / test ;
run ; quit ;

* Test using proc mixed ;
proc mixed data=splitplot ;
class day worker RFID ;
model y = RFID ;
random worker day worker*day worker*RFID ;
run ; quit ;


 


