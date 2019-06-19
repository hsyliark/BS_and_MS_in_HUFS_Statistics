/* HW p44 exercise 2-34 */

data employee ;
xbar1=82 ; xbar2=78 ; n1=15 ; n2=15 ;
s1=3 ; s2=2.5 ;
sp2=((n1-1)*s1**2+(n2-1)*s2**2)/(n1+n2-2) ;
t=tinv(0.975,n1+n2-2) ;
ci_lower=(xbar1-xbar2)-t*sqrt(sp2*(1/n1+1/n2)) ;
ci_high=(xbar1-xbar2)+t*sqrt(sp2*(1/n1+1/n2)) ;
run ;

proc print data=employee ;
run ;
