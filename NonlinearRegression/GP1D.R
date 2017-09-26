rm(list=ls())

# Local and Gaussian process regression for the homes data.

## Extract the longitude and precipitation variables


 load("S:\\Documents\\www\\BigData\\code\\homes.RData")
 long     <- homes[,5]
 precip   <- homes[,7]

 ooo      <- order(long)

 long     <- long[ooo]
 precip   <- precip[ooo]

 n        <- length(precip)
 
 plot(long,precip)
  
# Split the data into test and training data

  library(fields)
  set.seed(0820)

  np   <- 100
  test <- sort(sample(1:n,np,replace=TRUE))

  Xp   <- long[test]
  Yp   <- precip[test]
  Xo   <- long[-test]
  Yo   <- precip[-test]

  do       <- rdist(Xo,Xo)
  diag(do) <- 0
  dp       <- rdist(Xp,Xo)

## Local linear regression
 
 # Define the local weights for each prediction point for two bandwidths

  bw1 <- 2
  w1  <- exp(-dp/bw1)
  bw2 <- 0.5
  w2  <- exp(-dp/bw2)

  plot(Xo,w1[20,],type="l")
  points(Xo,w1[20,])
  lines(Xo,w2[20,],col=2)
  points(Xo,w2[20,],col=2)
  abline(v=Xp[20],col=3)
  legend("topright",c("bw = 10","bw=2","Prediction point"),
         col=1:3,lty=1,pch=c(1,1,NA),inset=0.05)

 # Perform local regression

  yhat1 <- yhat2 <- Xp
  for(j in 1:np){
    fit      <- lm(Yo~Xo,w=w1[j,])
    yhat1[j] <- fit$coef[1] + fit$coef[2]*Xp[j]
    fit      <- lm(Yo~Xo,w=w2[j,])
    yhat2[j] <- fit$coef[1] + fit$coef[2]*Xp[j]
  }
  fit <- loess(Yo~Xo)
  yhat3<- predict(fit,data.frame(Xo = Xp))

  plot(Xo,Yo)
  lines(Xp,yhat1,col=2,lwd=2)
  lines(Xp,yhat2,col=3,lwd=2)
  lines(Xp,yhat3,col=4,lwd=2)
  legend("topright",c("bw1","bw2","loess"),lwd=2,col=2:4,inset=0.05)

 
## K nearest neighbors for K = 5 or 20

  yhat4 <- yhat5 <- Xp
  K1 <- 5
  K2 <- 20
  for(j in 1:np){
    rrr      <- rank(dp[j,])
    rrr      <- ifelse(rrr==min(rrr),0,rrr) 
    yhat4[j] <- mean(Yo[rrr<=K1])
    yhat5[j] <- mean(Yo[rrr<=K2])
  }

  plot(Xo,Yo)
  lines(Xp,yhat4,col=2,lwd=2)
  lines(Xp,yhat5,col=3,lwd=2)
  legend("topright",c("K=5","K=20"),lwd=2,col=2:3,inset=0.05)


## Gaussian process regression

# The steps below include several shortcuts.  The mean and variance parameters
# are estimated using simple linear regression, and only one variance component
# is estimated using MLE.


# Remove the mean trend and standardize residuals

  b <- lm(Yo~Xo)$coef
  r <- Yo - b[1] - b[2]*Xo
  s <- sd(r)
  r <- r/s
  
  # Define the covariance of the GP
  GPcov <- function(d,rho){
    0.5*exp(-d/rho)+0.5*(d==0)
  }

  # log likelihood function of rho only
  log.like <- function(y,d,rho){
      S <- solve(GPcov(d,rho))
      l <- 0.5*determinant(S)$modulus[1] - 
           0.5*t(y)%*%S%*%y
  return(l)}

  # Plot the log likelihood on a grid of rho
  rho.grid <- seq(0.1,5,length=20)
  ll       <- rep(NA,20)
 
  for(j in 1:length(ll)){
    ll[j] <- log.like(r,do,rho.grid[j])
     plot(rho.grid,ll,type="l")
  }

  # Pick the MLE
  rho <- rho.grid[which.max(ll)]

  # Now perform prediction (kriging)
  So  <- GPcov(do,rho)
  Sp  <- GPcov(dp,rho)

  rhat  <- Sp%*%solve(So)%*%r
  yhat6 <- b[1] + b[2]*Xp + s*rhat

  plot(Xo,Yo)
  lines(Xp,yhat6,col=2,lwd=2)

# Compare MSE

 MSE <- c(mean((Yp-yhat1)^2),
          mean((Yp-yhat2)^2),
          mean((Yp-yhat3)^2),
          mean((Yp-yhat4)^2),
          mean((Yp-yhat5)^2),
          mean((Yp-yhat6)^2))

 names(MSE) <- c("Local bw1","Local bw2","loess","KNN1","KNN2","GP")
 print(MSE)

 