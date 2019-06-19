#### Homework 3 (Due to Oct.27th)

(y <- as.matrix(c(74,67,83,77,71,94,82,69,78,68,62,75,59,79,68)))

(X <- as.matrix(rep(1,15)))

Z1 <- matrix(rep(c(1,0,0),5),5,3,byrow=T)
Z2 <- matrix(rep(c(0,1,0),5),5,3,byrow=T)
Z3 <- matrix(rep(c(0,0,1),5),5,3,byrow=T)
(Z <- rbind(Z1,Z2,Z3))

(G <- 8.62*diag(rep(1,3)))

(R <- 73.766*diag(rep(1,15)))

(V <- Z%*%G%*%t(Z)+R)

(b_gls <- solve(t(X)%*%solve(V)%*%X)%*%t(X)%*%solve(V)%*%y)

(u <- G%*%t(Z)%*%solve(V)%*%(y-X%*%b_gls))