libname shin 'F:\대학원 강의\표본론_자료\jungso';
run;

data jungso;set shin.jungso;
san1=ksic1;
san2=ksic2;
jong=labor;
run;

proc summary data=jungso;
class si;
var sales;
output out=out_si mean=pop_m_si std=pop_std_si n=pop_n_si;
run;

data pop_n_si;set out_si;
if _type_=1;
keep si  pop_m_si pop_std_si pop_n_si;
run;

%let r=0.15;
%let t=1.96;


data sam_n; set pop_n_si;
r=&r;
t=&t;
n0= (t*pop_std_si)**2 /(r*pop_m_si)**2 ;
sam_n_si= int(n0/(1+n0/pop_n_si));
run;

proc summary data= sam_n;
var sam_n_si;
output out=tot_sam sum=tot_sam_n;
run;


%macro shin_si(r,si);

data sam_n&si; set pop_n_si;
if si=&si;
r=&r;
t=&t;
n0= (t*pop_std_si)**2 /(r*pop_m_si)**2 ;
sam_n&si= int(n0/(1+n0/pop_n_si));
n_sam_si=sam_n&si;
run;

 %mend;
 run;

 %shin_si(0.1, 11);
 %shin_si(0.15, 39);
 run;


 %macro sample(si);

 data pop&si;set jungso;
 if si=&si;
 run;


data sam_n&si; set  sam_n&si;
call symput ('n_sam_si', n_sam_si);

proc surveyselect data=pop&si out=sam_si&si n=&n_sam_si method=srs;
run;
%mend;

%sample(11);
%sample(39);

run;




