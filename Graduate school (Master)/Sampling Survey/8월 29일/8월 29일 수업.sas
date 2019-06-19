proc sort data=TMP1.jungso ;
by SI KSIC1 ;
run ;

proc summary data=TMP1.jungso ;
var SALES ;
by SI KSIC1 ;
output out=TMP1.res1 n=number mean=mean ;
run ;

proc print data=TMP1.res1 ;
run ;
