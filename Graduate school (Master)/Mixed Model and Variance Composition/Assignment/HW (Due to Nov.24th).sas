/* HW (Due to Nov.24th) */

data hr ;
input patient drug $ basehr hr1 hr5 hr15 hr30 hr1h ;
array hra{5} hr1 hr5 hr15 hr30 hr1h ;
do i=1 to 5 ;
if (i=1) then minute=1/60 ;
else if (i=2) then minute=5 ;
else if (i=3) then minute=15 ;
else if (i=4) then minute=30 ;
else minute=60 ;
time=minute ;
hours=minute/60 ;
hours1=hours ;
HR=hra{i} ;
output ;
end ;
drop i hr1 hr5 hr15 hr30 hr1h ;
datalines ;
201 p 92 76 84 88 96 84
202 b 54 58 60 60 60 64
203 p 84 86 82 84 86 82
204 a 72 72 68 68 78 72
205 b 80 84 84 96 92 72
206 p 68 72 68 68 64 62
207 a 100 104 100 92 92 68
208 a 60 60 58 56 50 56
209 a 88 104 88 88 78 84
210 b 92 82 82 76 82 80
211 b 88 80 84 80 80 78
212 p 102 86 86 96 86 88
214 a 84 92 100 88 88 80
215 b 104 100 96 88 92 84
216 a 92 80 72 64 68 64
217 p 92 88 84 76 88 84
218 a 72 84 78 80 80 76
219 b 72 100 92 84 88 80
220 p 80 80 80 78 80 78
221 p 72 68 76 72 72 68
222 b 88 88 98 98 96 88
223 b 88 88 96 88 88 80
224 p 88 78 84 64 68 64
232 a 78 72 72 78 80 68
;
run ;

proc print data=hr ;
run ;

* Question 1 ;

proc sort data=hr out=hrtime ;
by time ;
run ;

proc print data=hrtime ;
run ;

proc boxplot data=hrtime ;
plot HR*time ;
run ; quit ;

* Question 2 ;

proc mixed data=hr ;
class patient drug time ;
model HR = drug time drug*time ;
repeated time / subject=patient(drug) type=CS ;
run ; quit ;

* Question 3 ;

proc mixed data=hr ;
class patient drug time ;
model HR = drug time drug*time ;
repeated time / subject=patient(drug) type=AR(1) ;
run ; quit ;





