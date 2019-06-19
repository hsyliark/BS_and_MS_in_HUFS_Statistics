proc import 
	datafile='C:\Users\jong\Desktop\fuelcon6.csv'
	out=fuel
	dbms=csv replace;
run;

proc gplot data=fuel;
	plot fuelcon*pop;
	plot fuelcon*area;
	plot fuelcon*drivers;
	plot fuelcon*hwymiles;
	plot fuelcon*gastax;
	plot fuelcon*income;
run;

proc reg data=fuel;
	id state;
	model fuelcon = pop area drivers hwymiles gastax income / selection = rsquare adjrsq cp aic ; 
run;quit;

proc reg data=fuel;
	id state;
	model fuelcon = pop gastax area drivers hwymiles income / include=2 selection=rsquare aic;
run;quit;

proc reg data=fuel;
	id state;
	model fuelcon = pop area drivers hwymiles gastax income / selection=forward sle=0.15; 
run;

proc reg data=fuel;
	id state;
	model fuelcon = pop area drivers hwymiles gastax income / selection=backward sls=0.20; 
run;

proc reg data=fuel;
	id state;
	model fuelcon = pop area drivers hwymiles gastax income / selection=stepwise sle=0.15  sls=0.20l; 
run;



%cpplot( data= fuel ,
yvar= fuelcon , xvar = POP AREA DRIVERS HWYMILES GASTAX INCOME ,
gplot = CP  , 
plotchar = P A D H G I , cpmax=20 );
