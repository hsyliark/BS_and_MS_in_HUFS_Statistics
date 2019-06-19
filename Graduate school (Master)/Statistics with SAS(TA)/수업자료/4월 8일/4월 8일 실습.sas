*** SAS statistics Lab (April.8th) ;



** 변수 지정 방법 ;


* 데이터 불러들이기 ;
filename tests 'C:\Users\user2\Desktop\Apr.8th\stress.dat';


* 해당 데이터 불러들이기 ;
* 변수TotalTime에 대한 누적 구하기 (변수 SumSec) ;
data stress;
	infile tests;
	input ID $ 1-4 Name $ 6-25 RestHR 27-29 MaxHR 31-33
	RecHR 35-37 TimeMin 39-40 TimeSec 42-43
	Tolerance $ 45;
	TotalTime=(timemin*60)+timesec;
	SumSec+totaltime;
run;


* 해당 데이터 불러들이기 ;
* 변수TotalTime에 대한 누적 구하기 (변수 SumSec는 5400부터 시작해서 더해가는 것으로 지정) ;
data stress;
	infile tests;
	input ID $ 1-4 Name $ 6-25 RestHR 27-29 MaxHR 31-33
	RecHR 35-37 TimeMin 39-40 TimeSec 42-43
	Tolerance $ 45;
	TotalTime=(timemin*60)+timesec;
	retain SumSec 5400;
	sumsec+totaltime;
run;


* 해당 데이터 불러들이기 ;
* 변수TotalTime에 대한 누적 구하기 (변수 SumSec는 5400부터 시작해서 더해가는 것으로 지정) ;
* 변수 TotalTime이 800보다 큰 경우에 한해 새로운 변수 TestLength를 'Long'으로 지정 ;
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


* 해당 데이터 불러들이기 ;
* 변수TotalTime에 대한 누적 구하기 (변수 SumSec는 5400부터 시작해서 더해가는 것으로 지정) ;
* 변수 TotalTime의 크기에 따라 새로운 변수 TestLength를 지정 ;
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


* 해당 데이터 불러들이기 ;
* 변수TotalTime에 대한 누적 구하기 (변수 SumSec는 5400부터 시작해서 더해가는 것으로 지정) ;
* 변수 TotalTime의 크기에 따라 새로운 변수 TestLength를 지정 ;
* 변수 RestHr이 70보다 작은 경우는 제거 ;
* 변수 TestLength의 최대 길이 지정 ;
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


* 해당 데이터 불러들이기 (변수 timemin, timesec 제거) ;
* 변수TotalTime에 대한 누적 구하기 (변수 SumSec는 5400부터 시작해서 더해가는 것으로 지정) ;
* 변수 Tolerance가 'D'인 경우만 남김 ;
* 변수 TotalTime의 크기에 따라 새로운 변수 TestLength를 지정 ;
* 변수 TestLength의 최대 길이 지정 ;
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


* 해당 데이터 불러들이기 (변수 timemin, timesec 제거) ;
* 변수TotalTime에 대한 누적 구하기 (변수 SumSec는 5400부터 시작해서 더해가는 것으로 지정) ;
* 변수 Tolerance가 'D'인 경우만 남김 ;
* 변수 TotalTime의 크기에 따라 새로운 변수 TestLength와 Message를 지정 ;
* 변수 TestLength와 Message의 최대 길이 지정 ;
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



** 반복 (loop) 구문 사용법 ;


* 변수 Earned를 반복구문을 통해 구하기 ;
data earnings;
	Amount=1000;
	Rate=.075/12;
	do month=1 to 12;
		Earned+(amount+earned)*(rate);
	end;
run;


* 변수 Value, Interest, Year를 반복구문을 통해 구하기 ;
* 변수 counter 제거 ;
data earn (drop=counter);
	Value=2000;
	do counter=1 to 20;
		Interest=value*.075;
		value+interest;
		Year+1;
	end;
run;


* 변수 Value, Interest, Year를 반복구문을 통해 구하기 ;
* 반복과정을 모두 출력하기 ;
* 10번만 반복 (변수 Year은 짝수만 남김) ;
data earn;
	Value=2000;
	do Year=1 to 20;
		Interest=value*.075;
		value+interest;
	Year+1;
	output;
	end;
run;


* 변수 Capital을 반복구문을 통해 구하기 ;
data earn;
	Capital=2000;
	do month=1 to 12;
		Interest=capital*(.075/12);
		capital+interest;
		output ;
	end;
run;


* 이중 loop 구문 ;
* 변수 Capital을 반복구문을 통해 구하기 ; 
data earn;
	do year=1 to 20;
		Capital+2000;
		do month=1 to 12;
			Interest=capital*(.075/12);
			capital+interest;
			output ;
		end;
		output ;
	end;
run;


* until(조건문을 만족하는 결과가 나올때까지 반복) ;
data invest;
	do until(Capital>=50000);
		capital+2000;
		capital+capital*.10;
		Year+1;
		output ;
	end;
run;
data invest;
	do until(Capital>=50000);
		Year+1;
		capital+2000;
		capital+capital*.10;
		output ;
	end;
run;


* while(조건문을 만족하는 경우에 한해서 반복) ;
data invest;
	do while(Capital<50000);
		capital+2000;
		capital+capital*.10;
		Year+1;
		output ;
	end;
run;


* 2가지 조건 하에서 수행하는 반복계산 ;
* 10번 반복 시까지 Capital의 값이 50000을 넘지 않음 ;
data invest(drop=i);
	do i=1 to 10 until(Capital>=50000);
		Year+1;
		capital+2000;
		capital+capital*.10;
		output ;
	end;
run;


* 2가지 조건 하에서 수행하는 반복계산 ;
* 10번 반복하는 중간에 Capital의 값이 50000을 넘음 ;
data invest(drop=i);
	do i=1 to 10 until(Capital>=50000);
		Year+1;
		capital+4000;
		capital+capital*.10;
		output ;
	end;
run;


