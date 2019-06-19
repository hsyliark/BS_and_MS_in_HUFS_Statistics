/*-------------------------------------------------------------------*/
 /*       SAS/STAT(R) Technical Report: Spatial Prediction            */
 /*                      Using the SAS System                         */
 /*       Copyright(c) 1996 by SAS Institute Inc., Cary, NC, USA      */
 /*                   SAS Publications order # 55715                  */
 /*                        ISBN 1-55544-876-3                         */
 /*-------------------------------------------------------------------*/
 /*                                                                   */
 /* This material is provided "as is" by SAS Institute Inc.  There    */
 /* are no warranties, expressed or implied, as to merchantability or */
 /* fitness for a particular purpose regarding the materials or code  */
 /* contained herein. The Institute is not responsible for errors     */
 /* in this material as it now exists or will exist, nor does the     */
 /* Institute provide technical support for it.                       */
 /*                                                                   */
 /*-------------------------------------------------------------------*/
 /* Date Last Updated: 7Aug00                                         */
 /*-------------------------------------------------------------------*/
 /****************************************************************/
 /*          S A S   S A M P L E   L I B R A R Y                 */
 /*                                                              */
 /*    NAME: SPATIAL.SAS                                         */
 /*   TITLE: PROCS VARIOGRAM, KRIG2D EXAMPLEs                    */
 /* PRODUCT: SAS/STAT  SAS/GRAPH BASE                            */
 /* RELEASE: 6.12                                                */
 /*  SYSTEM: ALL                                                 */
 /*    KEYS: VARIOGRAM KRIGING SPATIAL PREDICTION                */
 /*   PROCS: VARIOGRAM, KRIGE2D, GPLOT, PRINT                    */
 /*    DATA:                                                     */
 /*                                                              */
 /* SUPPORT:                       UPDATE:                       */
 /*     REF: SPATIAL PREDICTION USING SAS/STAT (TECH REPROT)     */
 /*    MISC:                                                     */
 /*                                                              */
 /****************************************************************/
data thick ;
   input east north thick ;
   cards ;
  0.7  59.6  34.1
  2.1  82.7  42.2
  4.7  75.1  39.5
  4.8  52.8  34.3
  5.9  67.1  37.0
  6.0  35.7  35.9
  6.4  33.7  36.4
  7.0  46.7  34.6
  8.2  40.1  35.4
 13.3   0.6  44.7
 13.3  68.2  37.8
 13.4  31.3  37.8
 17.8   6.9  43.9
 20.1  66.3  37.7
 22.7  87.6  42.8
 23.0  93.9  43.6
 24.3  73.0  39.3
 24.8  15.1  42.3
 24.8  26.3  39.7
 26.4  58.0  36.9
 26.9  65.0  37.8
 27.7  83.3  41.8
 27.9  90.8  43.3
 29.1  47.9  36.7
 29.5  89.4  43.0
 30.1   6.1  43.6
 30.8  12.1  42.8
 32.7  40.2  37.5
 34.8   8.1  43.3
 35.3  32.0  38.8
 37.0  70.3  39.2
 38.2  77.9  40.7
 38.9  23.3  40.5
 39.4  82.5  41.4
 43.0   4.7  43.3
 43.7   7.6  43.1
 46.4  84.1  41.5
 46.7  10.6  42.6
 49.9  22.1  40.7
 51.0  88.8  42.0
 52.8  68.9  39.3
 52.9  32.7  39.2
 55.5  92.9  42.2
 56.0   1.6  42.7
 60.6  75.2  40.1
 62.1  26.6  40.1
 63.0  12.7  41.8
 69.0  75.6  40.1
 70.5  83.7  40.9
 70.9  11.0  41.7
 71.5  29.5  39.8
 78.1  45.5  38.7
 78.2   9.1  41.7
 78.4  20.0  40.8
 80.5  55.9  38.7
 81.1  51.0  38.6
 83.8   7.9  41.6
 84.5  11.0  41.5
 85.2  67.3  39.4
 85.5  73.0  39.8
 86.7  70.4  39.6
 87.2  55.7  38.8
 88.1   0.0  41.6
 88.4  12.1  41.3
 88.4  99.6  41.2
 88.8  82.9  40.5
 88.9   6.2  41.5
 90.6   7.0  41.5
 90.7  49.6  38.9
 91.5  55.4  39.0
 92.9  46.8  39.1
 93.4  70.9  39.7
 94.8  71.5  39.7
 96.2  84.3  40.3
 98.2  58.2  39.5
;
run ;

proc print data=thick split='#' ;
   var east north thick ;
   label east  = 'east# 1000s ft'
      north = 'north#1000s ft'
      thick = 'Coal Seam#Thickness (feet)' 
      ;
   title 'Simulated Coal Seam Thickness' ;
   run ;


symbol1 value=dot height=.5 cv=black ;

proc gplot data=thick ;
   plot north * east ;
   label east  = 'East-1000s ft.'
         north = 'North-1000s ft.'
      ;
   title 'Scatter Plot of Measurement Locations' ;
   run ;
   
proc g3d data=thick ;
   label east  = 'East'
         north = 'North'
         thick = 'Thickness' 
         ;

   scatter north*east=thick/ xticknum=5 yticknum=5 grid ; 
   title 'Surface Plot of Coal Seam Thickness' ;
run ;

 /*- Produce an initial histogram of inter-pair distances  ---------*/
proc variogram data=thick outdistance = outd ;
   compute novariogram  ;
   coordinates xc=east yc=north ;
   var thick ;
run ;

 /*- Print the outd data set   -------------------------------------*/

proc print data=outd ;
   title 'OUTDISTANCE= Data Set Showing Distance Intervals' ;
run ;

 /*- Get a midpoint for charting  ----------------------------------*/
data outd ; set outd ;
   mdpt = round((lb+ub)/2,.1) ;
   label mdpt = 'Midpoint of Interval' ;
run ;

 /*- Chart of initial histogram   ----------------------------------*/

proc gchart data=outd ;
   vbar mdpt / type=sum sumvar=count discrete ;
   title 'Distribution of Pairwise Distances' ;
run ;

 /*- Produce last histogram of inter-pair distances  --------------*/
proc variogram data=thick outdistance = outd ;
  compute novariogram nhc=20  ;
  coordinates xc=east yc=north ;
  var thick ;
run ;

 /*- Print the new outd data set   -------------------------------*/
proc print data=outd ;
   title 'OUTDISTANCE= Data Set Showing Distance Intervals' ;
run ;

 /*- Get a midpoint for charting  ----------------------------------*/
data outd ; set outd ;
  mdpt = round((lb+ub)/2,.1) ;
  label mdpt = 'Midpoint of Interval' ;
run ;

 /*- Chart of last histogram   -------------------------------------*/
proc gchart data=outd ;
   vbar mdpt / type=sum sumvar=count discrete ;
   title 'Distribution of Pairwise Distances' ;
run ;

 /*- Compute the variogram   ---------------------------------------*/
proc variogram data=thick
   outvar = outv
   ;
   compute lagdistance=7 maxlag=10 robust ;
   coordinates xc=east yc=north ;
   var thick ;
   run ;

 /*- Print the outv data set   -------------------------------------*/
 proc print data=outv label ;
   var lag count distance variog rvario ;
   title 'OUTVAR= Data Set Showing Sample Variogram Results' ;
run ;

 /*- Rearrange data for plotting semivariogram   ------------------*/
data outv ; set outv ;
  vari = variog ; type = 'regular' ; output ;
  vari = rvario ; type = 'robust'  ; output ;
run ;
   
axis1 minor=none label=(c=black 'lag distance') offset=(3,3) ;
axis2 minor=(number=1) 
      label=(c=black 'Variogram') offset=(3,3) ;

symbol1 i=join l=1 v=star c=black  ;
symbol2 i=join l=1 v=square c=black  ;

 /*- plot the semivariogram   ------------------------------------*/
proc gplot data=outv ;
  plot vari*distance=type / haxis=axis1 vaxis=axis2 ;
   title 'Standard and Robust Semivariogram for Coal Seam Thickness Data' ;
run ;
   
 /*- Add in theoretical semivariogram for plotting  --------------*/

data outv3 ; set outv ;
  c0 = 7.5 ; a0 = 30 ; 
  vari = c0*(1-exp(-distance*distance/(a0*a0))) ;
  type = 'Gaussian'; output ;
  vari = variog ; type = 'regular' ; output ;
  vari = rvario ; type = 'robust'  ; output ;
run ;
   
symbol1 i=join l=1 v=star c=black  ;
symbol2 i=join l=1 v=square c=black ;
symbol3 i=join l=1 v=diamond c=black ;


 /*- Plot all the semivariograma   ---------------------------------*/
proc gplot data=outv3 ;
   plot vari*distance=type / vaxis=axis2 haxis=axis1 ;
   title 'Theoretical and Sample Semivariogram for Coal Seam Thickness Data' ;
run ;

 /*- Do the kriging   ----------------------------------------------*/
proc krige2d data=thick outest=est ;
   pred var=thick r=60 ;
   model scale=7.5
         range=30
         form =gauss
         ;
  coord xc=east yc=north ;
  grid x=0 to 100 by 5 y=0 to 100 by 5 ;
run;

 /*- Plot the predicted values   -----------------------------------*/
proc g3d data=est ;
 scatter gyc*gxc=estimate / 
         grid 
         ;
 label gyc='north'
       gxc='east'
       estimate='thickness'
       ;
   title 'Surface Plot of Kriged Coal Seam Thickness' ;
run ;

 /*- Plot the standard errors   -----------------------------------*/
proc g3d data=est ;
 scatter gyc*gxc=stderr / 
         grid 
         ;
 label gyc='north'
       gxc='east'
       stderr='Std Error'
       ;
   title 'Surface Plot of Standard Errors of Kriging Estimates' ;
run ;

