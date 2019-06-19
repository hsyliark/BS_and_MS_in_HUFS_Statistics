data BPressure;
   input PatientID $ Systolic Diastolic @@;
   datalines;
CK 120 50  SS 96  60 FR 100 70
CP 120 75  BL 140 90 ES 120 70
CP 165 110 JI 110 40 MC 119 66
FC 125 76  RW 133 60 KD 108 54
DS 110 50  JW 130 80 BH 120 65
JW 134 80  SB 118 76 NS 122 78
GS 122 70  AB 122 78 EC 112 62
HH 122 82
;
title 'Systolic and Diastolic Blood Pressure';
proc univariate data=BPressure;
   var Systolic Diastolic;
run;



proc contents varnum data=sashelp.baseball;
   ods select position;
run;
proc print data=sashelp.baseball;
run;


ods graphics on;

proc gplot data=sashelp.baseball;
	symbol v=circle;
	plot logSalary*nhits;
run;

proc gplot data=sashelp.baseball;
	symbol v=circle;
	plot logSalary*nHome=League;
run;

proc corr data=sashelp.baseball plots=matrix(histogram);
var logSalary nhits nruns nrbi;
run;

proc reg data=sashelp.baseball;
   model logSalary = nhits nruns nrbi nbb yrmajor crhits;
run;

proc anova data=sashelp.baseball;
   class division;
   model logSalary = division;
run;


ods graphics off;

