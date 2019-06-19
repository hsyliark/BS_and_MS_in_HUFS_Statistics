filename tests 'c:\users\stress.dat';

data stress;
	infile tests;
	input ID $ 1-4 Name $ 6-25 RestHR 27-29 MaxHR 31-33
	RecHR 35-37 TimeMin 39-40 TimeSec 42-43
	Tolerance $ 45;
	TotalTime=(timemin*60)+timesec;
	SumSec+totaltime;
run;

data stress;
	infile tests;
	input ID $ 1-4 Name $ 6-25 RestHR 27-29 MaxHR 31-33
	RecHR 35-37 TimeMin 39-40 TimeSec 42-43
	Tolerance $ 45;
	TotalTime=(timemin*60)+timesec;
	retain SumSec 5400;
	sumsec+totaltime;
run;

data stress;
	infile tests;
	input ID $ 1-4 Name $ 6-25 RestHR 27-29 MaxHR 31-33
	RecHR 35-37 TimeMin 39-40 TimeSec 42-43
	Tolerance $ 45;
	TotalTime=(timemin*60)+timesec;
	retain SumSec 5400;
	sumsec+totaltime;
	if totaltime>800 then TestLength='Long';
run;

data stress;
	infile tests;
	input ID $ 1-4 Name $ 6-25 RestHR 27-29 MaxHR 31-33
	RecHR 35-37 TimeMin 39-40 TimeSec 42-43
	Tolerance $ 45;
	TotalTime=(timemin*60)+timesec;
	retain SumSec 5400;
	sumsec+totaltime;
	if totaltime>800 then TestLength='Long';
	else if 750<=totaltime<=800 then TestLength='Normal';
	else if totaltime<750 then TestLength='Short';
run;

data stress;
	infile tests;
	input ID $ 1-4 Name $ 6-25 RestHR 27-29 MaxHR 31-33
	RecHR 35-37 TimeMin 39-40 TimeSec 42-43
	Tolerance $ 45;
	if resthr<70 then delete;
	TotalTime=(timemin*60)+timesec;
	retain SumSec 5400;
	sumsec+totaltime;
	length TestLength $ 6;
	if totaltime>800 then testlength='Long';
	else if 750<=totaltime<=800 then testlength='Normal';
	else if totaltime<750 then TestLength='Short';
run;


data stress(drop=timemin timesec);
	infile tests;
	input ID $ 1-4 Name $ 6-25 RestHR 27-29 MaxHR 31-33
	RecHR 35-37 TimeMin 39-40 TimeSec 42-43
	Tolerance $ 45;
	if tolerance='D';
	TotalTime=(timemin*60)+timesec;
	retain SumSec 5400;
	sumsec+totaltime;
	length TestLength $ 6;
	if totaltime>800 then testlength='Long';
	else if 750<=totaltime<=800 then testlength='Normal';
	else if totaltime<750 then TestLength='Short';
run;

data stress;
	infile tests;
	input ID $ 1-4 Name $ 6-25 RestHR 27-29 MaxHR 31-33
	RecHR 35-37 TimeMin 39-40 TimeSec 42-43
	Tolerance $ 45;
	if tolerance='D';
	drop timemin timesec;
	TotalTime=(timemin*60)+timesec;
	retain SumSec 5400;
	sumsec+totaltime;
	length TestLength $ 6;
	if totaltime>800 then testlength='Long';
	else if 750<=totaltime<=800 then testlength='Normal';
	else if totaltime<750 then TestLength='Short';
run;

data stress;
	infile tests;
	input ID $ 1-4 Name $ 6-25 RestHR 27-29 MaxHR 31-33
	RecHR 35-37 TimeMin 39-40 TimeSec 42-43
	Tolerance $ 45;
	TotalTime=(timemin*60)+timesec;
	retain SumSec 5400;
	sumsec+totaltime;
	length TestLength $ 6 Message $ 20;
	if totaltime>800 then
	do;
		testlength='Long';
		message='Run blood panel';
	end;
	else if 750<=totaltime<=800 then testlength='Normal';
	else if totaltime<750 then TestLength='Short';
run;

data earnings;
	Amount=1000;
	Rate=.075/12;
	do month=1 to 12;
		Earned+(amount+earned)*(rate);
	end;
run;

data earn (drop=counter);
	Value=2000;
	do counter=1 to 20;
		Interest=value*.075;
		value+interest;
		Year+1;
	end;
run;

data earn;
	Value=2000;
	do Year=1 to 20;
		Interest=value*.075;
		value+interest;
	Year+1;
	output;
	end;
run;

data earn;
	Capital=2000;
	do month=1 to 12;
		Interest=capital*(.075/12);
		capital+interest;
	end;
run;

data earn;
	do year=1 to 20;
		Capital+2000;
		do month=1 to 12;
			Interest=capital*(.075/12);
			capital+interest;
		end;
	end;
run;

data invest;
	do until(Capital>=50000);
		capital+2000;
		capital+capital*.10;
		Year+1;
	end;
run;

data invest;
	do while(Capital>=50000);
		capital+2000;
		capital+capital*.10;
		Year+1;
	end;
run;

data invest;
	do until(Capital>=50000);
		Year+1;
		capital+2000;
		capital+capital*.10;
	end;
run;

data invest(drop=i);
	do i=1 to 10 until(Capital>=50000);
		Year+1;
		capital+2000;
		capital+capital*.10;
	end;
run;

data invest(drop=i);
	do i=1 to 10 until(Capital>=50000);
		Year+1;
		capital+4000;
		capital+capital*.10;
	end;
run;
