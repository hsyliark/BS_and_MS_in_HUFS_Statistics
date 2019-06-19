proc means data=clinic.survey;
run;
proc means data=clinic.survey median range;
run;
proc means data=clinic.diabetes min max;
run;
proc means data=clinic.diabetes min max maxdec=0;
run;
proc means data=clinic.diabetes min max maxdec=0;
	var age height weight;
run;
proc means data=perm.survey mean stderr maxdec=2;
	var item1-item5;
run;
proc means data=clinic.heart maxdec=1;
	var arterial heart cardiac urinary;
	class survive sex;
run;
proc sort data=clinic.heart out=work.heartsort;
	by survive sex;
run;
proc means data=work.heartsort maxdec=1;
	var arterial heart cardiac urinary;
	by survive sex;
run;
proc means data=clinic.diabetes;
	var age height weight;
	class sex;
	output out=work.sum_gender
	mean=AvgAge AvgHeight AvgWeight
	min=MinAge MinHeight MinWeight;
run;
proc print data=work.sum_gender;
run;
proc summary data=clinic.diabetes;
	var age height weight;
	class sex;
	output out=work.sum_gender
	mean=AvgAge AvgHeight AvgWeight;
run;



proc freq data=clinic.admit;
	tables sex actlevel;
run;
proc freq data=clinic.admit;
	tables sex*actlevel;
run;
proc freq data=clinic.admit;
	tables sex*actlevel*fee;
run;
proc freq data=clinic.admit;
	tables sex*actlevel*fee / list;
run;


