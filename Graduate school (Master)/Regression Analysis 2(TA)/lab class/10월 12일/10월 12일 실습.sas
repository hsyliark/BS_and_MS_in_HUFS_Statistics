/* Oct. 12th Lab class */

* Question 1~7 ;

proc import out=COMNODE3
datafile="C:\Users\user2\Desktop\회귀분석(TA)\10월 12일\COMNODE3.csv"
dbms=csv replace ;
run ;

proc print data=COMNODE3 ;
run ;

* 1) Numports 자료를 이용해서 proc reg 로 beta_0, beta_1 , sigma^2 추정치 구한다. ;

proc reg data=COMNODE3 ;
model cost=numports / clb alpha=0.05 ;
run ; quit ;

* 2)  sum_i^n (x_i - x_bar)^2 을  sas 를 이용해 구한다. ( proc univariate ) ;

proc univariate data=COMNODE3 ;
var numports ;
run ; quit ;

* 3) S.E.(\hat\mu_Y|x=40)  , S.E.(\hat\mu_Y|x=68) 을 1), 2) 의 결과를 이용해 손으로 구한다. ;
* 4) 3)을 이용하여  port 수가 40, 68일 경우에 95% C.I., 95% P.I. 를 손으로 구한다. ;
* 5) 위 4)의 결과와  sas 를 통해 구한 C.I.,  P.I. 와 비교한다. ;

data COMNODE3_1 ;
input COST NUMPORTS ; cards ;
. 40
. 68
;
run ;

data total ; set COMNODE3 COMNODE3_1 ;
run ;

proc print data=total ;
run ;

proc reg data=total;
model cost=numports ;
output out=predict p=y_pred LCL=cost_95LCL UCL=cost_95UCL LCLM=ecost_95LCL UCLM=ecost_95UCL ;
run ; quit ;

proc print data=predict(where=(cost=.)) noobs ;
run ;

* 6) Numports 자료에서 R^2 를 구하고, Corr(y, y_hat),  Corr(y, x) 를 각각 구해서 R^2 와의 관계를 확인하라. ;

proc reg data=COMNODE3 ;
model cost=numports / clb alpha=0.05 ;
output out=result p=y_hat ;
run ; quit ;

proc print data=result ;
run ;

proc corr data=result ;
var COST NUMPORTS y_hat ;
run ; quit ;
  
* 7)  교재 p.119  #21  Fill the blanks in ANOVA table. (See text book.) ;



* Question 8~9 ;

proc import out=ABXSALES3
datafile="C:\Users\user2\Desktop\회귀분석(TA)\10월 12일\ABXSALES3.csv"
dbms=csv replace ;
run ;

data ABXSALES3 ; set ABXSALES3 ;
time+1 ;
run ;

proc print data=ABXSALES3 ;
run ;

* 8)  ABX company sales (Time Linear Trend )  을 이용해서 (x,y) 산점도 그리기. (점과 점을 이은 선으로  표시할 것 -proc sgplot 의 series 옵션) ;

proc sgplot data=ABXSALES3 ;
scatter x=time y=SALES ;
series x=time y=SALES ;
run ; quit ;

* 9)  위 그래프에 Linear Trend regression line 을 overlay 시켜보아라. ;

proc reg data=ABXSALES3 ;
model SALES=time ;
output out=res_sales p=sales_hat ;
run ; quit ;

proc print data=res_sales ;
run ;

proc sgplot data=res_sales ;
scatter x=time y=SALES ;
series x=time y=SALES ;
series x=time y=sales_hat ;
run ; quit ;




 








