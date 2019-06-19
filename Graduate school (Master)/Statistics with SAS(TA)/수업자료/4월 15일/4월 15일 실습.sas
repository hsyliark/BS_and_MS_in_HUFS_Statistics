*** SAS statistics Lab (April.15th) ;



** 복잡한 형식의 데이터 읽기 ;
* @1, @9, @19 -> 지정된 숫자에 해당하는 줄부터 새로운 변수로 읽기 ;
* $5., $7. -> 문자변수의 최대길이 지정 ;
* +7 -> FirstName 변수를 읽은 뒤 포인터를 오른쪽 방향으로 7칸 이동시킴 ; 
* comma9. -> ','표시 제거하고 숫자변수로 읽기 ;

filename empdata 'C:\Users\user\Desktop\practice\empdata.txt' ;

data empinfo;
	infile empdata;
	input @9 FirstName $5. @1 LastName $7. +7 JobTitle 3.
	@19 Salary comma9.;
run;
proc print data=empinfo;
run;



** 구분자로 분리된 데이터 읽기 ;
* 1 구분자를 통해 읽기 -> ' ' ;

filename credit 'C:\Users\user\Desktop\practice\credit1.txt' ;

data survey;
	infile credit;
	input Gender $ Age Bankcard FreqBank Deptcard FreqDept;
run;
proc print data=survey ;
run ;


* 2 구분자를 통해 읽기 -> ',' ;

filename credit 'C:\Users\user\Desktop\practice\credit2.txt' ;

data survey;
	infile credit dlm=',';
	input Gender $ Age Bankcard FreqBank Deptcard FreqDept;
run;
proc print data=survey ;
run ;



** 복잡한 형식의 데이터 읽기 ;
* @13, @21 -> 지정된 숫자에 해당하는 줄부터 새로운 변수로 읽기 ;
* date7. -> 날짜형식의 변수를 숫자로 변환 ;
* comma6. -> ','표시를 없애고 숫자변수로 읽기 ;
* $9. -> 문자변수의 최대길이 지정 ; 

filename rawdata 'C:\Users\user\Desktop\practice\rawdata.txt' ;

data mixed;
	infile rawdata;
	input SSN $ 1-11 @13 HireDate date7. @21 Salary comma6.
	Department : $9. Phone;
run;
proc print data=mixed;
run;
