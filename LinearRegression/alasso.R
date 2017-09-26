
# Simulate a fake dataset

 n         <- 50
 p         <- 20
 beta      <- rep(0,p)
 beta[1:5] <- 1:5/5

 X <- matrix(rnorm(n*p),n,p)
 X <- scale(X) 

 Xb <- X%*%beta
 Y <- X%*%beta+rnorm(n)
 Y <- Y-mean(Y)


# Fit the lasso with lars

 library(lars)
 lasso  <- lars(X,Y)
 plot(lasso)

 df         <- lasso$df
 MSE        <- lasso$RSS/n
 bic        <- log(n)*df+n*log(MSE)
 beta_lasso <- lasso$beta[which.min(bic),] 

# Fit the adaptive lasso with lars

 beta_ols <- lm(Y~X)$coef[-1]
 w        <- abs(beta_ols)
 Xw       <- scale(X,center=FALSE,scale=1/w)
 alasso   <- lars(Xw,Y,normalize=FALSE)

 plot(alasso)

 df          <- alasso$df
 MSE         <- alasso$RSS/n
 bic         <- log(n)*df+n*log(MSE)
 beta_alasso <- w*alasso$beta[which.min(bic),] 

 plot(beta_lasso,beta_alasso)
 abline(0,1)


##################################################
#              Simulation study
##################################################

 nsims <- 100
 beta1 <- matrix(0,nsims,p)
 beta2 <- matrix(0,nsims,p)

 for(sim in 1:nsims){

   X <- matrix(rnorm(n*p),n,p)
   X <- scale(X) 

   Xb <- X%*%beta
   Y <- X%*%beta+rnorm(n)
   Y <- Y-mean(Y)


  # Fit the lasso

   lasso      <- lars(X,Y)
   df         <- lasso$df
   MSE        <- lasso$RSS/n
   bic        <- log(n)*df+n*log(MSE)
   beta_lasso <- lasso$beta[which.min(bic),] 

 # Fit the adaptive lasso

   beta_ols <- lm(Y~X)$coef[-1]                      # ols fit on standardized
   w        <- abs(beta_ols)
   Xw       <- scale(X,center=FALSE,scale=1/w)
   alasso   <- lars(Xw,Y,normalize=FALSE)
   df          <- alasso$df
   MSE         <- alasso$RSS/n
   bic         <- log(n)*df+n*log(MSE)
   beta_alasso <- w*alasso$beta[which.min(bic),] 

  beta1[sim,]<-beta_lasso
  beta2[sim,]<-beta_alasso
 }


boxplot(beta1)
lines(beta,col=2)

boxplot(beta2)
lines(beta,col=2)

beta  <- matrix(beta,nsims,p,byrow=TRUE)
bias1 <- colMeans(beta1-beta)
bias2 <- colMeans(beta2-beta)
var1  <- apply(beta1,2,var)
var2  <- apply(beta2,2,var)
mse1  <- colMeans((beta1-beta)^2)
mse2  <- colMeans((beta2-beta)^2)

plot(bias1,type="l",xlab="Variable",ylab="Bias")
lines(bias2,col=2)
legend("bottomright",c("Lasso","A-Lasso"),lty=1,col=1:2,inset=0.05)

plot(var2,col=2,type="l",xlab="Variable",ylab="Variance")
lines(var1,col=1)
legend("topright",c("Lasso","A-Lasso"),lty=1,col=1:2,inset=0.05)

plot(mse1,type="l",xlab="Variable",ylab="MSE")
lines(mse2,col=2)
legend("topright",c("Lasso","A-Lasso"),lty=1,col=1:2,inset=0.05)
