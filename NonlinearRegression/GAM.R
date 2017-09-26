# Fit a Generalized additive model in R using the gam package.

 # Generate a fake dataset
   set.seed(0820)

   n  <- 500
  
   X1 <- rnorm(n)
   X2 <- rnorm(n)
   X3 <- rnorm(n)
   X4 <- rnorm(n)
   f1 <- 2*X1
   f2 <- 3*cos(2*X2)
   f3 <- X3^2 
   f4 <- 0*X4
   Y  <- f1 + f2 + f3 + f4 + rnorm(n)

   plot(X1,Y);points(X1,f1,col=2,pch=19)
   plot(X2,Y);points(X2,f2,col=2,pch=19)
   plot(X3,Y);points(X3,f3,col=2,pch=19)
   plot(X4,Y);points(X4,f4,col=2,pch=19)

 # Added variable plots to look for non-linearity

  res   <- lm(Y~X1+X2+X3+X4)$res

  plot(X1,res)
  lo    <- loess(res~X1)
  lines(lo$x[order(X1)],lo$fit[order(X1)],col=2,lwd=2)
 
  plot(X2,res)
  lo    <- loess(res~X2)
  lines(lo$x[order(X2)],lo$fit[order(X2)],col=2,lwd=2)

  plot(X3,res)
  lo    <- loess(res~X3)
  lines(lo$x[order(X3)],lo$fit[order(X3)],col=2,lwd=2)

  plot(X4,res)
  lo    <- loess(res~X4)
  lines(lo$x[order(X4)],lo$fit[order(X4)],col=2,lwd=2)

 # Fit the linear model in GAM

  library(gam)
  par(mfrow=c(2,2))

  fit1 <- gam(Y~X1 + X2 + X3 + X4)
  summary(fit1)
  plot(fit1) 

 # Fit the GAM with splines (note that gam automatically inserts a linear trend for each covariate)

  fit2 <- gam(Y~s(X1) + s(X2) + s(X3) + s(X4))
  summary(fit2)
  plot(fit2) 

 # Fit the GAM with loess

  fit3 <- gam(Y~lo(X1) + lo(X2) + lo(X3) + lo(X4))
  summary(fit3)
  plot(fit3)
 
 # Fit the GAM with loess for X2 and X3 only

  fit4 <- gam(Y ~ X1 + lo(X2) + lo(X3) + X4)
  summary(fit4)
  plot(fit4)

 # Fit the GAM with loess for X2, X3, and their interaction

  fit5 <- gam(Y ~ X1 + lo(X2) + lo(X3) + lo(X2,X3) + X4)
  summary(fit5)
  plot(fit5)

  