*** SAS statistics Lab (April.1st) ;


** Producing descriptive statistics ;

* mean, standard deviation, minimum, maximum (각각의 변수에 대한) ;
proc means data=sasuser.survey;
run;

* median, range (각각의 변수에 대한) ;
proc means data=sasuser.survey median range;
run;

* minimum, maximum (각각의 변수에 대한) ;
proc means data=sasuser.diabetes min max;
run;

* minimum, maximum (각각의 변수에 대한, 소수점 첫째자리에서 반올림) ;
proc means data=sasuser.diabetes min max maxdec=0; 
run;

* minimum, maximum (각각의 변수에 대한, 소수점 첫째자리에서 반올림, 특정 변수만) ;
proc means data=sasuser.diabetes min max maxdec=0;
	var age height weight;
run;

* mean, standard error (각각의 변수에 대한, 소수점 셋째자리에서 반올림, 특정 변수만) ;
proc means data=sasuser.survey mean stderr maxdec=2;
	var item1-item5;
run;

* mean, standard deviation, minimum, maximum ;
* 소수점 둘째자리에서 반올림, 특정 변수만 ;
* 각각의 범주에 따라 분할하여 결과 나타냄 ;
proc means data=sasuser.heart maxdec=1;
	var arterial heart cardiac urinary;
	class survive sex;
run;

* 데이터 정렬 ;
* by 구문을 사용해 분할하기 위해서는 sorting이 반드시 필요 ;
proc sort data=sasuser.heart out=work.heartsort;
	by survive sex;
run;
proc means data=work.heartsort maxdec=1;
	var arterial heart cardiac urinary;
	by survive sex;
run;

* work.sum_gender 에 성별에 따른 변수의 평균값과 최솟값을 정리 ;
proc means data=sasuser.diabetes;
	var age height weight;
	class sex;
	output out=work.sum_gender
	mean=AvgAge AvgHeight AvgWeight
	min=MinAge MinHeight MinWeight;
run;
proc print data=work.sum_gender;
run;

* work.sum_gender 에 성별에 따른 변수의 평균값을 정리 (결과를 따로 출력하지 않음) ;
proc summary data=sasuser.diabetes;
	var age height weight;
	class sex;
	output out=work.sum_gender
	mean=AvgAge AvgHeight AvgWeight;
run;
proc print data=work.sum_gender;
run ;


** Producing frequency table ; 

* For each variables ;
proc freq data=sasuser.admit;
	tables sex actlevel;
run;

* Cross tables ;
proc freq data=sasuser.admit;
	tables sex*actlevel;
run;

proc freq data=sasuser.admit;
	tables sex*actlevel*fee;
run;

* 결과를 list 형식으로 출력 ;
proc freq data=sasuser.admit;
	tables sex*actlevel*fee / list;
run;
