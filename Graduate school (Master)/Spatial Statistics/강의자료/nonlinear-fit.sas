/*data cluster;
   infile '/ncsu/st564_info/st564data/cluster2G.dat' firstobs=9;
   input x y u v lnu lnv;
proc gplot data=cluster;
   bubble y*x=lnu / bsize=10 ;
   title 'Cluster data set from GSLIB';
run;
*/
data one;
   input lagno h gammahat nh tailmean headmean;
   title 'Omnidirectional semivariogram of ln(u) in cluster.dat';
   title2  'lag separation distance=2. lag tolerance=1.';
   cards;
   3        1.610      0.53089      318        1.69306        1.69306
   4        3.873      0.99974      316        1.07179        1.07179
   5        5.755      1.37742      696        0.92081        0.92081
   6        7.923      1.96924      688        0.54213        0.54213
   7        9.926      2.15287      814        0.67455        0.67455
   8       11.895      2.11709      866        0.53567        0.53567
   9       13.945      1.98197      970        0.60189        0.60189
  10       15.834      2.06063      928        0.63321        0.63321
  11       17.840      2.10188     1078        0.47217        0.47217
  12       19.848      2.41102      880        0.38467        0.38467
  13       21.953      2.34496      952        0.39011        0.39011
  14       23.877      2.58649     1096        0.47974        0.47974
  15       25.801      2.33276      994        0.41703        0.41703
  16       27.898      2.36804      738        0.56947        0.56947
  17       29.823      2.40436      616        0.53878        0.53878
  18       31.838      2.58280      612        0.53030        0.53030
  19       33.852      2.57877      464        0.47762        0.47762
;
proc nlin data=one nohalve;
   parms c0=0.005 c1=2 a=10;
*   bounds c0 >0.00001;
 
   gam=1-exp(-3*h/a);
   gamma=c0+ c1*gam;
   model gammahat=gamma;
   _weight_=nh/gamma**2;
   output out=two  p=vario;
   title 'Exponential semivariogram';run;

proc nlin data=one nohalve;
   parms c0=0.005 c1=2 a=10;
*   bounds c0 >0.00001;
 
   gam=1-exp(-3*h**2/a**2);
   gamma=c0+ c1*gam;
   model gammahat=gamma;
   _weight_=nh/gamma**2;
   output out=two  p=vario;
   title 'Gaussian semivariogram';run;

proc sort data=two;
   by h;
proc gplot data=two;
   plot gammahat*h=1 vario*h=2 / overlay;
   symbol1 c=black i=none v='*';
   symbol2 c=black i=join l=1 v=none;
run;
proc nlin data=one nohalve;
   parms   c0=0 c1=2      a=10;

   if h=0 then do;
   c0=0;
   der.c0=0;
   der.c1=0;
   der.a=0;
   end;

   if (0 < h < a) then do;
   gam=1.5*(h/a)-.5*((h/a)**3);
   der.c0=1;
   der.c1=gam;
   der.a=1.5*c1*(h/a**2)*(h/a**2-1);
   end;

   if (h >= a) then do;
   gam=1;
   der.c0=1;
   der.c1=1;
   der.a=0;
   end;

   gamma=c0+c1*gam;
   model gammahat=gamma;
   _weight_=nh/gamma**2;

   output out=twob p=vario;
   title3 'Fit spherical variogram model';
   title4 'Weighted least squares. Gauss-Newton with nohalve option';
   title5 'lags up to h=34';

proc sort data=twob;
   by h;

proc gplot data=twob;
   plot gammahat*h=1 vario*h=2 / overlay;
   symbol1 c=black i=none v='*';
   symbol2 c=black i=join l=1 v=none;
run;
