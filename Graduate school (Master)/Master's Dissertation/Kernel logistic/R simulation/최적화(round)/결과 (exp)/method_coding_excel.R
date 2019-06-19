
iter = 1000
p = 2

for(i in 1:iter){
  cat( 'i = ', i , '\n')
  x <- matrix(rnorm(1000), ncol=p )
  x.bar <- colMeans(x)
  if( i == 431 ) break
  write( x.bar , file=paste0("xbar_iter", iter , "_p" , p, ".txt") , append= TRUE, ncolumns=p )
}


mat <- matrix( rnorm( 100*8) , nc = 8 )
colnames(mat) <- c("KRLC" , "KRLC_s" , "KRLC_b1")

mat.df <- as.data.frame.table( mat )
mat.df <- mat.df[,-1]
mat.df <- cbind( mat.df , 2 )

