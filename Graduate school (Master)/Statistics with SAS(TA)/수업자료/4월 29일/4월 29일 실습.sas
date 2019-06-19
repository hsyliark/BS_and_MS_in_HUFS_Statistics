*** SAS Statistics Lab (April.29th) ;


** 원하는 부분만 설정해서 데이터 merging (one-to-one) ;
* 겹치지 않는 관측치는 삭제됨 ;
data work.one2one;
set sasuser.patients;
if age<60;
set sasuser.measure;
run;
proc print data=work.one2one;
run;


** 데이터 setting ;

* 특정 기준 없음 ;
data work.concat;
set sasuser.therapy1999 sasuser.therapy2000;
run;
proc print data=work.concat;
run;

* 특정 변수를 기준으로 정렬 ;
data work.interlv;
set sasuser.therapy1999 sasuser.therapy2000;
by month;
run;
proc print data=work.interlv;
run ;


** 특정 변수 기준으로 정렬 후 데이터 merging ; 
proc sort data=sasuser.demog out=work.demog1;
by id;
run;
proc print data=work.demog1;
run;
proc sort data=sasuser.visit out=work.visit1;
by id;
run;
proc print data=work.visit1;
run;

data work.merged;
merge work.demog1 work.visit1;
by id;
run;
proc print data=work.merged;
run;

* merging 시 변수 date에 대한 overwriting 방지 ;
* rename 구문 ;
data work.merged1;
merge work.demog1(rename=(date=BirthDate))
work.visit1(rename=(date=VisitDate));
by id;
run;
proc print data=work.merged1;
run;
