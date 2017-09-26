

nt  <- 100  # number of time points
n   <- 5    # number of observations at each time point
p   <- 2    # number of covariates


# True parameter values

 rho <- 0.99
 sig <- 0.5
 tau <- 0.2

 set.seed(0820)

 beta_true     <- matrix(0,p,nt)
 beta_true[,1] <- rnorm(p)
 for(t in 2:nt){
   beta_true[,t] <- rho*beta_true[,t-1] + rnorm(p,0,tau)
 }

 matplot(t(beta_true),type="l",lty=1,col=1:2)
 legend("topleft",paste("beta",1:p,sep=""),lty=1,col=1:2,inset=0.05)

# Generate data

 X      <- array(rnorm(nt*n*p),c(n,p,nt))
 X[,1,] <- 1
 Y      <- matrix(0,n,nt)
 for(t in 1:nt){
   Y[,t] <- X[,,t]%*%beta_true[,t] + rnorm(n,0,sig)
 }

# Fit least squares at each time point

 beta_ols <- matrix(0,p,nt)
 for(t in 1:nt){
  beta_ols[,t] <- lm(Y[,t]~X[,,t]-1)$coef
 }


# Apply the Kalman filter

 sig2    <- sig^2
 tau2    <- tau^2
 rho2    <- rho^2
 
 beta_kf <- matrix(0,p,nt)

 #First time point is least squares
 tXXinv  <- solve(t(X[,,1])%*%X[,,1])
 V       <- sig2*tXXinv
 M       <- tXXinv%*%t(X[,,1])%*%Y[,1] 

 for(t in 2:nt){
  Xt <- X[,,t]
  Q  <- rho2*V + tau2*diag(p)
  Qi <- solve(Q)
  V  <- solve(t(Xt)%*%Xt/sig2 + Qi)
  M  <- V%*%(t(Xt)%*%Y[,t]/sig2 + rho*Qi%*%M) 

  beta_kf[,t] <- M
 }

# Plot the results:

 beta1 <- cbind(beta_true[1,],beta_ols[1,],beta_kf[1,])
 beta2 <- cbind(beta_true[2,],beta_ols[2,],beta_kf[2,])

 matplot(beta1,type="l",col=1:3,lty=1,xlab="t",ylab="Coefficient",main=expression(beta[1](t)))
 legend("bottomright",c("True","OLS","Kalman"),lty=1,col=1:3,inset=0.05)

 matplot(beta2,type="l",col=1:3,lty=1,xlab="t",ylab="Coefficient",main=expression(beta[2](t)))
 legend("bottomright",c("True","OLS","Kalman"),lty=1,col=1:3,inset=0.05)

 rowMeans((beta_true-beta_ols)^2)
 rowMeans((beta_true-beta_kf)^2)

