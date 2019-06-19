Final Assignment 1
========================================================
author: Sung-yoon Hwang (200903877) 
date: 2014.6.16
autosize: true
font-family: 'Helvetica'

-Find the optimal portfolio from using monthly returns..
-Period : January.2004 ~ April.2014
(10 company in Korea : Doosan, Hanwha, Hynix, Hyosung, Hyundai,
Kia, Kyobo, Lotte, Posco, S-oil)

Reading data & Calculate monthly returns
========================================================

Returns from KOSPI will use to benchmarking.


```r
kospi <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Final Exam/Final Assignment 1/kospi_month.csv", sep = ",", header = T)
x <- kospi$Close
n <- length(x)
kospi.return <- x[2:n]/x[1:(n - 1)] - 1
```


Reading data & Calculate monthly returns
========================================================


```r
doosan <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Final Exam/Final Assignment 1/doosan_month.csv", sep = ",", header = T)
x <- doosan$Close
n <- length(x)
doosan.return <- x[2:n]/x[1:(n - 1)] - 1
```


Reading data & Calculate monthly returns
========================================================


```r
hanwha <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Final Exam/Final Assignment 1/hanwha_month.csv", sep = ",", header = T)
x <- hanwha$Close
n <- length(x)
hanwha.return <- x[2:n]/x[1:(n - 1)] - 1
```


Reading data & Calculate monthly returns
========================================================


```r
hynix <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Final Exam/Final Assignment 1/hynix_month.csv", sep = ",", header = T)
x <- hynix$Close
n <- length(x)
hynix.return <- x[2:n]/x[1:(n - 1)] - 1
```


Reading data & Calculate monthly returns
========================================================


```r
hyosung <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Final Exam/Final Assignment 1/hyosung_month.csv", sep = ",", header = T)
x <- hyosung$Close
n <- length(x)
hyosung.return <- x[2:n]/x[1:(n - 1)] - 1
```


Reading data & Calculate monthly returns
========================================================


```r
hyundai <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Final Exam/Final Assignment 1/hyundai_month.csv", sep = ",", header = T)
x <- hyundai$Close
n <- length(x)
hyundai.return <- x[2:n]/x[1:(n - 1)] - 1
```


Reading data & Calculate monthly returns
========================================================


```r
kia <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Final Exam/Final Assignment 1/kia_month.csv", sep = ",", header = T)
x <- kia$Close
n <- length(x)
kia.return <- x[2:n]/x[1:(n - 1)] - 1
```


Reading data & Calculate monthly returns
========================================================


```r
kyobo <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Final Exam/Final Assignment 1/kyobo_month.csv", sep = ",", header = T)
x <- kyobo$Close
n <- length(x)
kyobo.return <- x[2:n]/x[1:(n - 1)] - 1
```


Reading data & Calculate monthly returns
========================================================


```r
lotte <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Final Exam/Final Assignment 1/lotte_month.csv", sep = ",", header = T)
x <- lotte$Close
n <- length(x)
lotte.return <- x[2:n]/x[1:(n - 1)] - 1
```


Reading data & Calculate monthly returns
========================================================


```r
posco <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Final Exam/Final Assignment 1/posco_month.csv", sep = ",", header = T)
x <- posco$Close
n <- length(x)
posco.return <- x[2:n]/x[1:(n - 1)] - 1
```


Reading data & Calculate monthly returns
========================================================


```r
soil <- read.csv("C:/Users/user/Desktop/Sung-yoon.R/Final Exam/Final Assignment 1/s-oil_month.csv", sep = ",", header = T)
x <- soil$Close
n <- length(x)
soil.return <- x[2:n]/x[1:(n - 1)] - 1
```



Make Return Matrix
========================================================


```r
Returns <- cbind(doosan.return, hanwha.return, hynix.return, hyosung.return, hyundai.return, kia.return, kyobo.return, lotte.return, posco.return, soil.return)
summary(Returns)
apply(Returns, 2, mean)
cov(Returns)
```


Make Return Matrix
========================================================


```
 doosan.return     hanwha.return      hynix.return     hyosung.return   
 Min.   :-0.3613   Min.   :-0.5517   Min.   :-0.4419   Min.   :-0.5185  
 1st Qu.:-0.0834   1st Qu.:-0.0696   1st Qu.:-0.0727   1st Qu.:-0.0705  
 Median : 0.0173   Median : 0.0160   Median : 0.0151   Median : 0.0102  
 Mean   : 0.0243   Mean   : 0.0258   Mean   : 0.0225   Mean   : 0.0254  
 3rd Qu.: 0.1035   3rd Qu.: 0.1058   3rd Qu.: 0.0892   3rd Qu.: 0.1258  
 Max.   : 0.7011   Max.   : 0.5039   Max.   : 0.6119   Max.   : 0.4282  
 hyundai.return      kia.return       kyobo.return      lotte.return    
 Min.   :-0.3239   Min.   :-0.3458   Min.   :-0.4220   Min.   :-0.3370  
 1st Qu.:-0.0537   1st Qu.:-0.0499   1st Qu.:-0.0834   1st Qu.:-0.0402  
 Median : 0.0079   Median : 0.0056   Median :-0.0119   Median : 0.0152  
 Mean   : 0.0171   Mean   : 0.0200   Mean   : 0.0183   Mean   : 0.0217  
 3rd Qu.: 0.0843   3rd Qu.: 0.0823   3rd Qu.: 0.0823   3rd Qu.: 0.0835  
 Max.   : 0.2734   Max.   : 0.3189   Max.   : 0.7438   Max.   : 0.2802  
  posco.return      soil.return     
 Min.   :-0.2804   Min.   :-0.1812  
 1st Qu.:-0.0509   1st Qu.:-0.0521  
 Median : 0.0066   Median :-0.0051  
 Mean   : 0.0112   Mean   : 0.0092  
 3rd Qu.: 0.0826   3rd Qu.: 0.0593  
 Max.   : 0.3321   Max.   : 0.4140  
```


Make Return Matrix
========================================================


```
 doosan.return  hanwha.return   hynix.return hyosung.return hyundai.return 
      0.024287       0.025818       0.022455       0.025366       0.017117 
    kia.return   kyobo.return   lotte.return   posco.return    soil.return 
      0.020010       0.018320       0.021667       0.011197       0.009197 
```


Make Return Matrix
========================================================


```
               doosan.return hanwha.return hynix.return hyosung.return
doosan.return       0.023963      0.013356     0.004373       0.008097
hanwha.return       0.013356      0.021613     0.009634       0.011450
hynix.return        0.004373      0.009634     0.022675       0.005320
hyosung.return      0.008097      0.011450     0.005320       0.020125
hyundai.return      0.005277      0.006807     0.004513       0.005018
kia.return          0.003177      0.006015     0.009325       0.003138
kyobo.return        0.011549      0.012938     0.011322       0.010883
lotte.return        0.002786      0.004251     0.002350       0.003853
posco.return        0.006598      0.006049     0.002924       0.006790
soil.return         0.004470      0.005701     0.002067       0.003139
               hyundai.return kia.return kyobo.return lotte.return
doosan.return        0.005277   0.003177     0.011549     0.002786
hanwha.return        0.006807   0.006015     0.012938     0.004251
hynix.return         0.004513   0.009325     0.011322     0.002350
hyosung.return       0.005018   0.003138     0.010883     0.003853
hyundai.return       0.010595   0.003908     0.006349     0.003103
kia.return           0.003908   0.012475     0.007082     0.001149
kyobo.return         0.006349   0.007082     0.027426     0.005206
lotte.return         0.003103   0.001149     0.005206     0.011851
posco.return         0.005180   0.002175     0.006504     0.003925
soil.return          0.002591   0.002164     0.004158     0.002146
               posco.return soil.return
doosan.return      0.006598    0.004470
hanwha.return      0.006049    0.005701
hynix.return       0.002924    0.002067
hyosung.return     0.006790    0.003139
hyundai.return     0.005180    0.002591
kia.return         0.002175    0.002164
kyobo.return       0.006504    0.004158
lotte.return       0.003925    0.002146
posco.return       0.010736    0.004058
soil.return        0.004058    0.008326
```


Make Return Matrix
========================================================
#### Interpretation
Refer to the prior slide, means of 10 monthly returns are close to 0. And when looking at the covariance matrix, there are little pairs with a strong correlation. Let's find optimal portfolio with multivariate stocks.

Portfolio with multivariate stocks
========================================================

#### Portfolio with multivariate stocks (w1,w2,...,wn)
#### w1 + w2 + ... + wn = 1
#### R1, R2, R3, ... , Rn
#### Rp = w1 * R1 + w2 * R2 + ... + wn * Rn = t(w) % * % ( R )
#### mu(p) = E(Rp) = t(w) % * % E ( R ) = t(w) % * % (MU)
#### sigma(p)^2 = Var(Rp) = t(w) % * % Var ( R ) % * % (w)
#### = t(w) % * % (SIGMA) % * % (w)
#### MU : mean vector , SIGMA : covariance matrix
#### We have to predict distribution of portfolio returns.

Portfolio with multivariate stocks
========================================================

#### We can't control MU and SIGMA.
#### So we have to decide vector w carefully.

#### Find the vector w with minimizing sigma(p).
#### (The method of Lagrange multiplier)
#### ( t(w) % * % (MU) = mu(p) , w1 + w2 + ... + wn = 1 )

#### When element of vector w is less then 0,
#### we have to borrow stock and sell it. (Short selling)
#### Because stocks are traded in one markets, 
#### correlation coefficient is positive number in many cases.

Portfolio with multivariate stocks
========================================================

### Method to find the optimal portfolio when specific return is decided.

#### When mu(p) is decided, find the wector w with minimizing volatility.
#### When t(w) % * % (MU)=mu(p), w1 + w2 + ... + wn = 1 ,
#### find the vector w with minimizing 
#### t(w) % * % (SIGMA) % * % (w) .  (Optimizing)
#### MU : mean vector , SIGMA : covariance matrix

Make function 'Minimum.variance.portfolio'
========================================================


```r
Minimum.variance.portfolio <- function(Returns, mu.p, plot=F, prt=F)
{
  n.p <- length(mu.p)
  mu <- apply(Returns, 2, mean)
  Sigma <- cov(Returns)
  Sigma.inv <- solve(Sigma)
  ones <- rep(1, dim(Returns)[2])
  
  Av <- (t(ones)%*%Sigma.inv%*%mu)[1,1]
  Bv <- (t(mu)%*%Sigma.inv%*%mu)[1,1]
  Cv <- (t(ones)%*%Sigma.inv%*%ones)[1,1]
  Dv <- Bv*Cv-Av*Av
  
  g <- (Bv*Sigma.inv%*%ones-Av*Sigma.inv%*%mu)/Dv
  h <- (Cv*Sigma.inv%*%mu-Av*Sigma.inv%*%ones)/Dv
  
  w.p <- sigma.p <- NULL
  for ( j in 1:n.p ) {
    w <- g+h*mu.p[j]
    w.p <- cbind(w.p, w)
    sigma.p <- c(sigma.p, sqrt((t(w)%*%Sigma%*%w)[1,1]))
  }
  
  if (plot) {
    plot(sigma.p, mu.p, xlim=c(0, max(sigma.p)), xlab="volatility", ylab="mean return")
    lines(sigma.p, mu.p, col=2)
  }
  if (prt) print(cbind(mu.p, sigma.p, t(w.p)))
  
  return(list(mu.p=mu.p, sigma.p=sigma.p, w.p=w.p))
}
```


Find optimizing portfolio for each returns.
========================================================


```r
mu.p <- seq(from=0.001,to=0.05,by=0.0005)
mvp <- Minimum.variance.portfolio(Returns,mu.p,plot=T,prt=T)
```


Find optimizing portfolio for each returns.
========================================================
























```
processing file: Final Assignment 1 (200903877 È²¼ºÀ±).Rpres
(if (out_format(c("latex", "sweave", "listings", "markdown"))) sanitize_fn else str_c)(path, ¿¡¼­ ´ÙÀ½°ú °°Àº °æ°í°¡ ¹ß»ýÇß½À´Ï´Ù :
  replaced special characters in figure filename "Final Assignment 1 (200903877 È²¼ºÀ±)-figure/unnamed-chunk-18" -> "Final_Assignment_1__200903877_È²¼ºÀ±_-figure/unnamed-chunk-18"
png(..., res = dpi, units = "in")¿¡¼­ °æ°í°¡ ¹ß»ýÇß½À´Ï´Ù : ¾²±â À§ÇØ¼­ ÆÄÀÏ 'Final_Assignment_1__200903877_í™©ì„±ìœ¤_-figure/unnamed-chunk-18.png'¸¦ ¿­ ¼ö ¾ø½À´Ï´Ù
png(..., res = dpi, units = "in")¿¡¼­ °æ°í°¡ ¹ß»ýÇß½À´Ï´Ù : opening device failed
Quitting from lines 417-450 (Final Assignment 1 (200903877 È²¼ºÀ±).Rpres) 
´ÙÀ½¿¡ ¿À·ù°¡ ÀÖ½À´Ï´Ùpng(..., res = dpi, units = "in") : 
  png() ÀåÄ¡¸¦ ½ÃÀÛÇÒ ¼ö ¾ø½À´Ï´Ù
```
