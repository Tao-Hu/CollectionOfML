

# Computing the least squares solution

##  Simulate a fake dataset

 n<-10000
 p<-1000

 X<-matrix(rnorm(n*p),n,p)
 Y<-rnorm(n)

## Compute the solution using the lm function

 tick <- proc.time()[3]
 beta <- lm(Y~X-1)$coef
 tock <- proc.time()[3]
 
 beta[1:5]
 tock-tick

## Compute the solution using solve to invert $X'X$


 tick <- proc.time()[3]
 tXX  <- crossprod(X)    # equivalent to t(X)%*%X
 tXY  <- t(X)%*%Y
 beta <- solve(tXX)%*%tXY
 tock <- proc.time()[3]
 
 beta[1:5]
 tock-tick



## Compute the solution using cholesky to invert $X'X$


 tick <- proc.time()[3]
 tXX  <- crossprod(X)    # equivalent to t(X)%*%X
 tXY  <- t(X)%*%Y
 beta <- chol2inv(chol(tXX))%*%tXY
 tock <- proc.time()[3]
 
 beta[1:5]
 tock-tick



## Compute the solution without inverting $X'X$


 tick <- proc.time()[3]
 tXX  <- crossprod(X)    # equivalent to t(X)%*%X
 tXY  <- t(X)%*%Y
 beta <- solve(tXX,tXY)
 tock <- proc.time()[3]
 
 beta[1:5]
 tock-tick


## Now exclude the time used to compute $X'X$ and $X'Y$

 tXX  <- crossprod(X)
 tXY  <- t(X)%*%Y

 system.time(solve(tXX)%*%tXY)
 system.time(chol2inv(chol(tXX))%*%tXY)
 system.time(solve(tXX,tXY))

