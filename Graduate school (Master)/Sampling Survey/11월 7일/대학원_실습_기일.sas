proc import out = kim  
datafile = "C:\Users\ADMIN\Desktop\2016 바탕화면\BOOK\표본론\exam.csv" 
dbms = csv replace ; 
getnames = yes ; /* 첫 번째 레코드를 변수명으로 인식 */
run ;

%let n = 1000;

/* 1 */
proc summary data=kim;
class city sizes ;
types city*sizes;
var totwage;
output out=exam_first n = pop_n ; 
run;

proc print data = exam_first;
run;

/* 2, 3 */
data new_kim ;
set kim;
if sizes < 4 then new_size = 1 ;
else new_size = 2 ;
run ;


proc summary data=new_kim;
var totwage;
output out=exam_sec_pop_n;
run;

data exam_sec_pop_n;set exam_sec_pop_n;
if _STAT_='N';
call symput ('pop_n', totwage);
run;


proc summary data=new_kim;
class city new_size ;
types city*new_size;
var totwage;
output out=exam_sec  mean = new_pop_m std=new_pop_std n = new_pop_n ;
run;

data exam_sec2;
set exam_sec;
fpc = (&pop_n/new_pop_n);
new_sterr = new_pop_std/sqrt(new_pop_n)*sqrt(1-fpc);
run;

proc print data = exam_sec2;
run;

/* 4 */


data new_out_si1;
set exam_sec;
sq_pop_n_si = sqrt(new_pop_n);
run ; 

proc summary data = new_out_si1 ;
var sq_pop_n_si ;
output out = result sum = sq_sum_n;
run ; 

data result_tot_n ; set result ;
call symput ('sq_sum_n', sq_sum_n); 
run ; 

data fin_sam_n; set new_out_si1 ;
sq_n_sam = round(1000*(sq_pop_n_si/&sq_sum_n));
run;

proc print data = fin_sam_n;
run;

/*5*    설계 가중치 *******************/
data wei_n_fin; set fin_sam_n;
wei_n_sam = (new_pop_n/sq_n_sam);
keep CITY new_size wei_n_sam;
run;

proc print data = wei_n_fin ;
run;


/* 6           표본 추출 ******************/
data sq_n_sam ;
set fin_sam_n;
_NSIZE_ = sq_n_sam;
run ; 

data pop; 
set new_kim ;
run ; 

proc sort data=pop;
by  city new_size;
run;

proc surveyselect data = pop seed = 12345 out=sam_si n=sq_n_sam method=sys; 
strata  city new_size;
run ; 



/****************************계통 추출을 이용한 예비 표본 **********************************/

data pop_11_1;set pop;
if city=11;
if new_size=1;
run;

data aa1=pop_11_1;
run;

proc surveyselect data=aa1 method=sys out=aa2 n=94;
run;

data aa2_ori;set aa2;
sam_ori=1;

data a_sam_ori;
merge aa1 aa2_ori;
by i;
if sam_ori=1;
run;


data aa_sub_1;set aa2; sam_sub1=1;
i=i-1;
run;

data sam_sub1;merge aa1 aa_sub_1;
by i;
if sam_sub1=1;
run;



data aa_sub_2;set aa2; sam_sub2=1;
i=i+1;
run;

data sam_sub2;merge aa1 aa_sub_2;
by i;
if sam_sub2=1;
run;



/*******************조사된 자료와 가중치(최종 가중치)를 머지한다. *******************/

proc sort data=sam_si;
by city new_size;
run;



proc sort data=wei_n_fin;
by city new_size;
run;

data sam_si_fin; merge sam_si wei_n_fin;
by city new_size;
run;


/******************최종적으로 평균과 RSE를 계산한다************************/


proc summary data=sam_si;
class city new_size;
types city*new_size;
var totwage;
output out=sam_result mean=sam_m_si std=sam_std_si;
run;

proc print data = sam_result;
run;


/****************************************************************************************/

data pop;set pop;
aaa=1;
run;

proc summary data=pop;
class city new_size;
var aaa;
output out=pop_tot sum=_total_;
run;

 data pop_tot;
 set pop_tot;
 if _TYPE_=3;
 run;




proc surveymeans data=sam_si_fin total=pop_tot mean stderr cv;
strata city new_size;
var totwage;
weight wei_n_sam;
by  city new_size;
ods output statistics=sido_out ;
run;



data rse_city_out;set city_out;
rse=100*cv;run;



proc surveymeans data=sam_si_fin total=pop_tot mean stderr cv;
strata city new_size;
var totwage;
weight wei_n_sam;
by  city;
ods output statistics=city_out ;
run;


data rse_city_out;set city_out;
rse=100*cv;run;

data sam_si_fin;set sam_si_fin;
aaa=1;
run;



proc surveymeans data=sam_si_fin total=7057 mean stderr cv;
strata aaa city new_size;
var totwage;
weight wei_n_sam;
by  aaa;
ods output statistics=size_out ;
run;


data rse_size_out;set size_out;
rse=100*cv;run;




















