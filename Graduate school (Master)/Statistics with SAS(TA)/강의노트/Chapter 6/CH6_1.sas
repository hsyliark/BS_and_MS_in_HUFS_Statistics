filename empdata 'C:\Users\admin\Desktop\empdata.dat';

data empinfo;
	infile empdata;
	input @9 FirstName $5. @1 LastName $7. +7 JobTitle 3.
	@19 Salary comma9.;
run;
proc print data=empinfo;
run;


filename empdata2 'C:\Users\admin\Desktop\empdata2.dat';

data empinfo2;
	infile empdata2;
	input FirstName $ LastName $ JobTitle  Salary;
run;

filename empdata3 'C:\Users\admin\Desktop\empdata3.dat';

data empinfo3;
	infile empdata3 dlm="#";
	input FirstName $ LastName $ JobTitle  Salary;
run;
