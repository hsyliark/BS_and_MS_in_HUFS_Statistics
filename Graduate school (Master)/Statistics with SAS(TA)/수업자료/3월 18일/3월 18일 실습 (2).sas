** Lab class March.18th ;

* Chapter 2 ;

proc print data=sasuser.admit;
	var age height weight fee;
run;

proc print data=sasuser.admit;
	var age height weight fee;
	id ID;
run;

proc sort data=sasuser.admit out=work.wgtadmit;
	by weight age;
run;
proc print data=work.wgtadmit;
	var age height weight fee;
	where age>30;
run;

proc sort data=sasuser.admit out=work.wgtadmit;
	by descending weight age;
run;
proc print data=work.wgtadmit;
	var age height weight fee;
	where age>30;
run;


proc print data=sasuser.insure;
	var name policy balancedue;
	where pctinsured < 100;
	sum balancedue;
run;

proc sort data=sasuser.admit out=work.activity;
	by actlevel;
run;
proc print data=work.activity;
	var age height weight fee;
	where age>30;
	sum fee;
	by actlevel;
run;

proc sort data=sasuser.admit out=work.activity;
	by actlevel;
	run;
proc print data=work.activity;
	var age height weight fee;
	where age>30;
	sum fee;
	by actlevel;
	id actlevel;
run;

title1 'Heart Rates for Patients with';
title3 'Increased Stress Tolerance Levels';
proc print data=sasuser.stress;
	var resthr maxhr rechr;
	where tolerance='I';
run;

footnote1 'Data from Treadmill Tests';
footnote3 '1st Quarter Admissions';
proc print data=sasuser.stress;
	var resthr maxhr rechr;
	where tolerance='I';
run;

footnote1 'Data from Treadmill Tests';
footnote3 '1st Quarter Admissions';
proc print data=sasuser.stress;
	var resthr maxhr rechr;
	where tolerance='I';
run;
proc tabulate data=sasuser.stress;
	where tolerance='I';
	var resthr maxhr;
	table mean*(resthr maxhr);
run;

title3 'Participation in Exercise Therapy';
proc print data=sasuser.therapy;
	var swim walkjogrun aerclass;
run;
title2 'Report for March';
proc print data=sasuser.therapy;
run;

title1;
footnote1 'Data from Treadmill Tests';
footnote3 '1st Quarter Admissions';
proc print data=sasuser.stress;
var resthr maxhr rechr;
where tolerance='I';
run;
footnote;
proc tabulate data=sasuser.stress;
var timemin timesec;
table max*(timemin timesec);
run;


proc print data=sasuser.admit label;
var age height;
label age='Age of Patient';
label height='Height in Inches';
run;
proc print data=sasuser.admit label;
var actlevel height weight;
label actlevel='Activity Level'
height='Height in Inches'
weight='Weight in Pounds';
run;

proc print data=sasuser.admit;
	var actlevel fee;
	where actlevel='HIGH';
	format fee dollar4.;
run;
