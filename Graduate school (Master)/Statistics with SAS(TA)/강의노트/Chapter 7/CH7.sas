data clinic.one2one;
	set clinic.patients;
	if age<60;
	set clinic.measure;
run;

data clinic.concat;
	set clinic.therapy1999 clinic.therapy2000;
run;

data clinic.interlv;
	set clinic.therapy1999 clinic.therapy2000;
	by month;
run;

proc sort data=clinic.demog;
	by id;
run;
proc print data=clinic.demog;
run;
proc sort data=clinic.visit;
	by id;
run;
proc print data=clinic.visit;
run;
data clinic.merged;
	merge clinic.demog clinic.visit;
	by id;
run;
proc print data=clinic.merged;
run;

data clinic.merged;
	merge clinic.demog(rename=(date=BirthDate))
	clinic.visit(rename=(date=VisitDate));
	by id;
run;
proc print data=clinic.merged;
run;
